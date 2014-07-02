{-# LANGUAGE OverloadedStrings #-}

-- see http://snapforbeginners.com/chapters/index.html

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as C
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
-- handleLogin produces a handler which uses the authentication snaplet from an optional error message
-- ist binds splices for processing a login error to the template "login" (and as I understand it only for that template ("heistLocal"))
-- shortcut: renderWithSplices templatename "errs"
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs :: Monad m0 => Splices (HeistT n0 m0 Template)
    errs = maybe noSplices splice authError
    -- if there is an error, apply a binding of that error message to the "loginError" element
    --  (which is defined in _login.tpl which in turn is applied to login.tpl)
    -- TODO: what happens, if there is no error? discards noSplices that element or ignores it? no example here given since redirect without error
    splice :: Monad m0 => T.Text -> Splices (HeistT n0 m0 Template)
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    -- specifies form parameter names (login, password, rememberMe), fail and success handlers
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
  ("/login", with auth handleLoginSubmit)
  , ("/logout", with auth handleLogout)
  , ("/new_user", with auth handleNewUser)
  , ("/helloworld", writeBS "helloworld")
  , ("/test", listTestEntriesHandler)
  , ("", serveDirectory "static")
  ]

listTestEntriesHandler :: Handler App App  ()
listTestEntriesHandler = do
        results <- getData 
        liftIO $ print (results :: [String]) -- logging
        renderWithSplices "list_test_entries" ("listTestEntries" ## listTestEntriesSplice results)

getData :: Handler App App [String]
getData = return ["1", "2", "3"]

listTestEntriesSplice  :: [String] -> I.Splice AppHandler
listTestEntriesSplice = I.mapSplices (I.runChildrenWith . listTestEntrySplice)

listTestEntrySplice :: Monad m => String -> Splices (HeistT n m Template)
listTestEntrySplice dataEntry = do
  "testEntry" ## I.textSplice (T.pack $ "data: " ++ dataEntry)

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
-- id description "packaged snaplet init?" init
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    -- init heist with template folder snap/heist/templates and accessor in App
    h <- nestSnaplet "" heist $ heistInit "templates"
    -- init sessions: cookie-based: encryption-key accessor-name timeout
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    -- defined routes
    addRoutes routes
    -- add the standard auth-splices globally (<ifLoggedIn>, <ifLoggedOut>, <loggedInUser/>)
    addAuthSplices h auth
    -- init user defined application
    return $ App h s a
