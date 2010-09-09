------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E R R                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2010, Free Software Foundation, Inc.         --
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

--  This package contains the routines to output error messages and the scanner
--  for the project files. It replaces Errout and Scn. It is not dependent on
--  the GNAT tree packages (Atree, Sinfo, ...). It uses exactly the same global
--  variables as Errout, located in package Err_Vars. Like Errout, it also uses
--  the common variables and routines in package Erroutc.
--
--  Parameters are set through Err_Vars.Error_Msg_File_* or
--  Err_Vars.Error_Msg_Name_*, and replaced automatically in the messages
--  ("{{" for files, "%%" for names).
--
--  However, in this package you can configure the error messages to be sent
--  to your own callback by setting Report_Error in the flags. This ensures
--  that applications can control where error messages are displayed.

with Scng;
with Errutil;

package Prj.Err is

   ---------------------------------------------------------
   -- Error Message Text and Message Insertion Characters --
   ---------------------------------------------------------

   --  See errutil.ads

   -----------------------------------------------------
   -- Format of Messages and Manual Quotation Control --
   -----------------------------------------------------

   --  See errutil.ads

   ------------------------------
   -- Error Output Subprograms --
   ------------------------------

   procedure Initialize renames Errutil.Initialize;
   --  Initializes for output of error messages. Must be called for each
   --  file before using any of the other routines in the package.

   procedure Finalize (Source_Type : String := "project")
     renames Errutil.Finalize;
   --  Finalize processing of error messages for one file and output message
   --  indicating the number of detected errors.

   procedure Error_Msg
     (Flags    : Processing_Flags;
      Msg      : String;
      Location : Source_Ptr := No_Location;
      Project  : Project_Id := null);
   --  Output an error message, either through Flags.Error_Report or through
   --  Errutil. The location defaults to the project's location ("project"
   --  in the source code). If Msg starts with "?", this is a warning, and
   --  Warning: is added at the beginning. If Msg starts with "<", see comment
   --  for Err_Vars.Error_Msg_Warn.

   -------------
   -- Scanner --
   -------------

   procedure Post_Scan;
   --  Convert an Ada operator symbol into a standard string

   package Scanner is new Scng
     (Post_Scan    => Post_Scan,
      Error_Msg    => Errutil.Error_Msg,
      Error_Msg_S  => Errutil.Error_Msg_S,
      Error_Msg_SC => Errutil.Error_Msg_SC,
      Error_Msg_SP => Errutil.Error_Msg_SP,
      Style        => Errutil.Style);
   --  Instantiation of the generic scanner

end Prj.Err;
