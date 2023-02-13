------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E R R U T I L                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines to output error messages and the
--  corresponding instantiation of Styleg, suitable to instantiate Scng.

--  It uses the same global variables as Errout, located in packages Atree and
--  Err_Vars. Like Errout, it also uses the common variables and routines
--  in package Erroutc.

--  This package is used by the preprocessor (gprep.adb).

with Styleg;
with Types; use Types;

package Errutil is

   ---------------------------------------------------------
   -- Error Message Text and Message Insertion Characters --
   ---------------------------------------------------------

   --  Error message text strings are composed of lower case letters, digits
   --  and the special characters space, comma, period, colon and semicolon,
   --  apostrophe and parentheses. Special insertion characters can also
   --  appear which cause the error message circuit to modify the given
   --  string. For a full list of these, see the spec of errout.

   -----------------------------------------------------
   -- Format of Messages and Manual Quotation Control --
   -----------------------------------------------------

   --  Messages are generally all in lower case, except for inserted names
   --  and appear in one of the following two forms:

   --    error: text
   --    warning: text

   --  The prefixes error and warning are supplied automatically (depending
   --  on the use of the ? insertion character), and the call to the error
   --  message routine supplies the text. The "error: " prefix is omitted
   --  in brief error message formats.

   --  Reserved keywords in the message are in the default keyword case
   --  (determined from the given source program), surrounded by quotation
   --  marks. This is achieved by spelling the reserved word in upper case
   --  letters, which is recognized as a request for insertion of quotation
   --  marks by the error text processor. Thus for example:

   --    Error_Msg_AP ("IS expected");

   --  would result in the output of one of the following:

   --    error: "is" expected
   --    error: "IS" expected
   --    error: "Is" expected

   --  the choice between these being made by looking at the casing convention
   --  used for keywords (actually the first compilation unit keyword) in the
   --  source file.

   --  In the case of names, the default mode for the error text processor
   --  is to surround the name by quotation marks automatically. The case
   --  used for the identifier names is taken from the source program where
   --  possible, and otherwise is the default casing convention taken from
   --  the source file usage.

   --  In some cases, better control over the placement of quote marks is
   --  required. This is achieved using manual quotation mode. In this mode,
   --  one or more insertion sequences is surrounded by backquote characters.
   --  The backquote characters are output as double quote marks, and normal
   --  automatic insertion of quotes is suppressed between the double quotes.
   --  For example:

   --    Error_Msg_AP ("`END &;` expected");

   --  generates a message like

   --    error: "end Open_Scope;" expected

   --  where the node specifying the name Open_Scope has been stored in
   --  Error_Msg_Node_1 prior to the call. The great majority of error
   --  messages operates in normal quotation mode.

   --  Note: the normal automatic insertion of spaces before insertion
   --  sequences (such as those that come from & and %) is suppressed in
   --  manual quotation mode, so blanks, if needed as in the above example,
   --  must be explicitly present.

   ------------------------------
   -- Error Output Subprograms --
   ------------------------------

   procedure Initialize;
   --  Initializes for output of error messages. Must be called for each
   --  file before using any of the other routines in the package.

   procedure Finalize (Source_Type : String := "project");
   --  Finalize processing of error messages for one file and output message
   --  indicating the number of detected errors.
   --  Source_Type is used in verbose mode to indicate the type of the source
   --  being parsed (project file, definition file or input file for the
   --  preprocessor).

   procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr);
   --  Output a message at specified location

   procedure Error_Msg_S (Msg : String);
   --  Output a message at current scan pointer location

   procedure Error_Msg_SC (Msg : String);
   --  Output a message at the start of the current token, unless we are at
   --  the end of file, in which case we always output the message after the
   --  last real token in the file.

   procedure Error_Msg_SP (Msg : String);
   --  Output a message at the start of the previous token

   procedure Set_Ignore_Errors (To : Boolean);
   --  Indicate, when To = True, that all reported errors should
   --  be ignored. By default reported errors are not ignored.

   package Style is new Styleg
     (Error_Msg    => Error_Msg,
      Error_Msg_S  => Error_Msg_S,
      Error_Msg_SC => Error_Msg_SC,
      Error_Msg_SP => Error_Msg_SP);
   --  Instantiation of the generic style package, suitable for an
   --  instantiation of Scng.

end Errutil;
