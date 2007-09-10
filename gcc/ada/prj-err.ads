------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E R R                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2007, Free Software Foundation, Inc.         --
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

   procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr)
     renames Errutil.Error_Msg;
   --  Output a message at specified location

   procedure Error_Msg_S (Msg : String) renames Errutil.Error_Msg_S;
   --  Output a message at current scan pointer location

   procedure Error_Msg_SC (Msg : String) renames Errutil.Error_Msg_SC;
   --  Output a message at the start of the current token, unless we are at
   --  the end of file, in which case we always output the message after the
   --  last real token in the file.

   procedure Error_Msg_SP (Msg : String) renames Errutil.Error_Msg_SP;
   --  Output a message at the start of the previous token

   -------------
   -- Scanner --
   -------------

   package Style renames Errutil.Style;
   --  Instantiation of the generic style package, needed for the instantiation
   --  of the generic scanner below.

   procedure Obsolescent_Check (S : Source_Ptr);
   --  Dummy null procedure for Scng instantiation

   procedure Post_Scan;
   --  Convert an Ada operator symbol into a standard string

   package Scanner is new Scng
     (Post_Scan         => Post_Scan,
      Error_Msg         => Error_Msg,
      Error_Msg_S       => Error_Msg_S,
      Error_Msg_SC      => Error_Msg_SC,
      Error_Msg_SP      => Error_Msg_SP,
      Obsolescent_Check => Obsolescent_Check,
      Style             => Style);
   --  Instantiation of the generic scanner

end Prj.Err;
