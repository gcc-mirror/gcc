------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       G N A T . C G I . C O O K I E                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2000-2021, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a package to interface a GNAT program with a Web server via the
--  Common Gateway Interface (CGI). It exports services to deal with Web
--  cookies (piece of information kept in the Web client software).

--  The complete CGI Cookie specification can be found in the RFC2109 at:
--     http://www.ics.uci.edu/pub/ietf/http/rfc2109.txt

--  This package builds up data tables whose memory is not released. A CGI
--  program is expected to be a short lived program and so it is adequate to
--  have the underlying OS free the program on exit.

package GNAT.CGI.Cookie is

   --  The package will initialize itself by parsing the HTTP_Cookie runtime
   --  CGI environment variable during elaboration but we do not want to raise
   --  an exception at this time, so the exception Data_Error is deferred and
   --  will be raised when calling any services below (except for Ok).

   Cookie_Not_Found : exception;
   --  This exception is raised when a specific parameter is not found

   procedure Put_Header
     (Header : String  := Default_Header;
      Force  : Boolean := False);
   --  Output standard CGI header by default. This header must be returned
   --  back to the server at the very beginning and will be output only for
   --  the first call to Put_Header if Force is set to False. This procedure
   --  also outputs the Cookies that have been defined. If the program uses
   --  the GNAT.CGI.Put_Header service, cookies will not be set.
   --
   --  Cookies are passed back to the server in the header, the format is:
   --
   --    Set-Cookie: <key>=<value>; comment=<comment>; domain=<domain>;
   --     max_age=<max_age>; path=<path>[; secured]

   function Ok return Boolean;
   --  Returns True if the CGI cookie environment is valid and False otherwise.
   --  Every service used when the CGI environment is not valid will raise the
   --  exception Data_Error.

   function Count return Natural;
   --  Returns the number of cookies received by the CGI

   function Value
     (Key      : String;
      Required : Boolean := False) return String;
   --  Returns the cookie value associated with the cookie named Key. If cookie
   --  does not exist, returns an empty string if Required is False and raises
   --  the exception Cookie_Not_Found otherwise.

   function Value (Position : Positive) return String;
   --  Returns the value associated with the cookie number Position of the CGI.
   --  It raises Cookie_Not_Found if there is no such cookie (i.e. Position >
   --  Count)

   function Exists (Key : String) return Boolean;
   --  Returns True if the cookie named Key exist and False otherwise

   function Key (Position : Positive) return String;
   --  Returns the key associated with the cookie number Position of the CGI.
   --  It raises Cookie_Not_Found if there is no such cookie (i.e. Position >
   --  Count)

   procedure Set
     (Key     : String;
      Value   : String;
      Comment : String  := "";
      Domain  : String  := "";
      Max_Age : Natural := Natural'Last;
      Path    : String  := "/";
      Secure  : Boolean := False);
   --  Add a cookie to the list of cookies. This will be sent back to the
   --  server by the Put_Header service above.

   generic
      with procedure
        Action
          (Key      : String;
           Value    : String;
           Position : Positive;
           Quit     : in out Boolean);
   procedure For_Every_Cookie;
   --  Iterate through all cookies received from the server and call
   --  the Action supplied procedure. The Key, Value parameters are set
   --  appropriately, Position is the cookie order in the list, Quit is set to
   --  True by default. Quit can be set to False to control the iterator
   --  termination.

end GNAT.CGI.Cookie;
