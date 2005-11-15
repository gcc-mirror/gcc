------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E R R _ V A R S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains variables common to error reporting packages
--  including Errout and Prj.Err.

with Types; use Types;
with Uintp; use Uintp;

package Err_Vars is

   Serious_Errors_Detected : Nat;
   --  This is a count of errors that are serious enough to stop expansion,
   --  and hence to prevent generation of an object file even if the
   --  switch -gnatQ is set.

   Total_Errors_Detected : Nat;
   --  Number of errors detected so far. Includes count of serious errors
   --  and non-serious errors, so this value is always greater than or
   --  equal to the Serious_Errors_Detected value.

   Warnings_Detected : Nat;
   --  Number of warnings detected

   Current_Error_Source_File : Source_File_Index;
   --  Id of current messages. Used to post file name when unit changes. This
   --  is initialized to Main_Source_File at the start of a compilation, which
   --  means that no file names will be output unless there are errors in units
   --  other than the main unit. However, if the main unit has a pragma
   --  Source_Reference line, then this is initialized to No_Source_File,
   --  to force an initial reference to the real source file name.

   Raise_Exception_On_Error : Nat := 0;
   --  If this value is non-zero, then any attempt to generate an error
   --  message raises the exception Error_Msg_Exception, and the error
   --  message is not output. This is used for defending against junk
   --  resulting from illegalities, and also for substitution of more
   --  appropriate error messages from higher semantic levels. It is
   --  a counter so that the increment/decrement protocol nests neatly.

   Error_Msg_Exception : exception;
   --  Exception raised if Raise_Exception_On_Error is true

   -----------------------------------------------------
   -- Global Values Used for Error Message Insertions --
   -----------------------------------------------------

   --  The following global variables are essentially additional parameters
   --  passed to the error message routine for insertion sequences described
   --  above. The reason these are passed globally is that the insertion
   --  mechanism is essentially an untyped one in which the appropriate
   --  variables are set dependingon the specific insertion characters used.

   Error_Msg_Col : Column_Number;
   --  Column for @ insertion character in message

   Error_Msg_Uint_1 : Uint;
   Error_Msg_Uint_2 : Uint;
   --  Uint values for ^ insertion characters in message

   Error_Msg_Sloc : Source_Ptr;
   --  Source location for # insertion character in message

   Error_Msg_Name_1 : Name_Id;
   Error_Msg_Name_2 : Name_Id;
   Error_Msg_Name_3 : Name_Id;
   --  Name_Id values for % insertion characters in message

   Error_Msg_Unit_1 : Name_Id;
   Error_Msg_Unit_2 : Name_Id;
   --  Name_Id values for $ insertion characters in message

   Error_Msg_Node_1 : Node_Id;
   Error_Msg_Node_2 : Node_Id;
   --  Node_Id values for & insertion characters in message

   Error_Msg_Qual_Level : Int := 0;
   --  Number of levels of qualification required for type name (see the
   --  description of the } insertion character. Note that this value does
   --  note get reset by any Error_Msg call, so the caller is responsible
   --  for resetting it.

   Error_Msg_Warn : Boolean;
   --  Used if current message contains a < insertion character to indicate
   --  if the current message is a warning message.

   Warn_On_Instance : Boolean := False;
   --  Normally if a warning is generated in a generic template from the
   --  analysis of the template, then the warning really belongs in the
   --  template, and the default value of False for this Boolean achieves
   --  that effect. If Warn_On_Instance is set True, then the warnings are
   --  generated on the instantiation (referring to the template) rather
   --  than on the template itself.

end Err_Vars;
