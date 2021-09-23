------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T . C G I                              --
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
--  Common Gateway Interface (CGI).

--  Other related packages are:

--     GNAT.CGI.Cookie which deal with Web HTTP Cookies.
--     GNAT.CGI.Debug  which output complete CGI runtime environment

--  Basically this package parse the CGI parameter which are a set of key/value
--  pairs. It builds a table whose index is the key and provides some services
--  to deal with this table.

--  Example:

--     Consider the following simple HTML form to capture a client name:

--        <!DOCTYPE HTML PUBLIC "-//W3C//DTD W3 HTML 3.2//EN">
--        <html>
--        <head>
--        <title>My Web Page</title>
--        </head>

--        <body>
--        <form action="/cgi-bin/new_client" method="POST">
--        <input type=text name=client_name>
--        <input type=submit name="Enter">
--        </form>
--        </body>
--        </html>

--     The following program will retrieve the client's name:

--        with GNAT.CGI;

--        procedure New_Client is
--           use GNAT;

--           procedure Add_Client_To_Database (Name : String) is
--           begin
--              ...
--           end Add_Client_To_Database;

--        begin
--           --  Check that we have 2 arguments (there is two inputs tag in
--           --  the HTML form) and that one of them is called "client_name".

--           if CGI.Argument_Count = 2
--             and then CGI.Key_Exists ("client_name")
--           then
--              Add_Client_To_Database (CGI.Value ("client_name"));
--           end if;

--           ...

--           CGI.Put_Header;
--           Text_IO.Put_Line ("<html><body>< ... Ok ... >");

--        exception
--           when CGI.Data_Error =>
--              CGI.Put_Header ("Location: /htdocs/error.html");
--              --  This returns the address of a Web page to be displayed
--              --  using a "Location:" header style.
--        end New_Client;

--  Note that the names in this package interface have been designed so that
--  they read nicely with the CGI prefix. The recommended style is to avoid
--  a use clause for GNAT.CGI, but to include a use clause for GNAT.

--  This package builds up a table of CGI parameters whose memory is not
--  released. A CGI program is expected to be a short lived program and
--  so it is adequate to have the underlying OS free the program on exit.

package GNAT.CGI is

   Data_Error : exception;
   --  This is raised when there is a problem with the CGI protocol. Either
   --  the data could not be retrieved or the CGI environment is invalid.
   --
   --  The package will initialize itself by parsing the runtime CGI
   --  environment during elaboration but we do not want to raise an
   --  exception at this time, so the exception Data_Error is deferred
   --  and will be raised when calling any services below (except for Ok).

   Parameter_Not_Found : exception;
   --  This exception is raised when a specific parameter is not found

   Default_Header : constant String := "Content-type: text/html";
   --  This is the default header returned by Put_Header. If the CGI program
   --  returned data is not an HTML page, this header must be change to a
   --  valid MIME type.

   type Method_Type is (Get, Post);
   --  The method used to pass parameter from the Web client to the
   --  server. With the GET method parameters are passed via the command
   --  line, with the POST method parameters are passed via environment
   --  variables. Others methods are not supported by this implementation.

   type Metavariable_Name is
     (Auth_Type,
      Content_Length,
      Content_Type,
      Document_Root,          --  Web server dependent
      Gateway_Interface,
      HTTP_Accept,
      HTTP_Accept_Encoding,
      HTTP_Accept_Language,
      HTTP_Connection,
      HTTP_Cookie,
      HTTP_Extension,
      HTTP_From,
      HTTP_Host,
      HTTP_Referer,
      HTTP_User_Agent,
      Path,
      Path_Info,
      Path_Translated,
      Query_String,
      Remote_Addr,
      Remote_Host,
      Remote_Port,            --  Web server dependent
      Remote_Ident,
      Remote_User,
      Request_Method,
      Request_URI,            --  Web server dependent
      Script_Filename,        --  Web server dependent
      Script_Name,
      Server_Addr,            --  Web server dependent
      Server_Admin,           --  Web server dependent
      Server_Name,
      Server_Port,
      Server_Protocol,
      Server_Signature,       --  Web server dependent
      Server_Software);
   --  CGI metavariables that are set by the Web server during program
   --  execution. All these variables are part of the restricted CGI runtime
   --  environment and can be read using Metavariable service. The detailed
   --  meanings of these metavariables are out of the scope of this
   --  description. Please refer to http://www.w3.org/CGI/ for a description
   --  of the CGI specification. Some metavariables are Web server dependent
   --  and are not described in the cited document.

   procedure Put_Header
     (Header : String  := Default_Header;
      Force  : Boolean := False);
   --  Output standard CGI header by default. The header string is followed by
   --  an empty line. This header must be the first answer sent back to the
   --  server. Do nothing if this function has already been called and Force
   --  is False.

   function Ok return Boolean;
   --  Returns True if the CGI environment is valid and False otherwise.
   --  Every service used when the CGI environment is not valid will raise
   --  the exception Data_Error.

   function Method return Method_Type;
   --  Returns the method used to call the CGI

   function Metavariable
     (Name     : Metavariable_Name;
      Required : Boolean := False) return String;
   --  Returns parameter Name value. Returns the null string if Name
   --  environment variable is not defined or raises Data_Error if
   --  Required is set to True.

   function Metavariable_Exists (Name : Metavariable_Name) return Boolean;
   --  Returns True if the environment variable Name is defined in
   --  the CGI runtime environment and False otherwise.

   function URL return String;
   --  Returns the URL used to call this script without the parameters.
   --  The URL form is: http://<server_name>[:<server_port>]<script_name>

   function Argument_Count return Natural;
   --  Returns the number of parameters passed to the client. This is the
   --  number of input tags in a form or the number of parameters passed to
   --  the CGI via the command line.

   ---------------------------------------------------
   -- Services to retrieve key/value CGI parameters --
   ---------------------------------------------------

   function Value
     (Key      : String;
      Required : Boolean := False) return String;
   --  Returns the parameter value associated to the parameter named Key.
   --  If parameter does not exist, returns an empty string if Required
   --  is False and raises the exception Parameter_Not_Found otherwise.

   function Value (Position : Positive) return String;
   --  Returns the parameter value associated with the CGI parameter number
   --  Position. Raises Parameter_Not_Found if there is no such parameter
   --  (i.e. Position > Argument_Count)

   function Key_Exists (Key : String) return Boolean;
   --  Returns True if the parameter named Key exists and False otherwise

   function Key (Position : Positive) return String;
   --  Returns the parameter key associated with the CGI parameter number
   --  Position. Raises the exception Parameter_Not_Found if there is no
   --  such parameter (i.e. Position > Argument_Count)

   generic
     with procedure
       Action
         (Key      : String;
          Value    : String;
          Position : Positive;
          Quit     : in out Boolean);
   procedure For_Every_Parameter;
   --  Iterate through all existing key/value pairs and call the Action
   --  supplied procedure. The Key and Value are set appropriately, Position
   --  is the parameter order in the list, Quit is set to True by default.
   --  Quit can be set to False to control the iterator termination.

private

   function Decode (S : String) return String;
   --  Decode Web string S. A string when passed to a CGI is encoded,
   --  this function will decode the string to return the original
   --  string's content. Every triplet of the form %HH (where H is an
   --  hexadecimal number) is translated into the character such that:
   --  Hex (Character'Pos (C)) = HH.

end GNAT.CGI;
