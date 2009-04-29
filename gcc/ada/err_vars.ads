------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E R R _ V A R S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  This package contains variables common to error reporting packages
--  including Errout and Prj.Err.

with Namet; use Namet;
with Types; use Types;
with Uintp; use Uintp;

package Err_Vars is

   --  All of these variables are set when needed, so they do not need to be
   --  initialized. However, there is code that saves and restores existing
   --  values, which may malfunction in -gnatVa mode if the variable has never
   --  been iniitalized, so we initialize some variables to avoid exceptions
   --  from invalid values in such cases.

   ------------------
   -- Error Counts --
   ------------------

   Serious_Errors_Detected : Nat := 0;
   --  This is a count of errors that are serious enough to stop expansion,
   --  and hence to prevent generation of an object file even if the
   --  switch -gnatQ is set. Initialized to zero at the start of compilation.
   --  Initialized for -gnatVa use, see comment above.

   Total_Errors_Detected : Nat := 0;
   --  Number of errors detected so far. Includes count of serious errors and
   --  non-serious errors, so this value is always greater than or equal to the
   --  Serious_Errors_Detected value. Initialized to zero at the start of
   --  compilation. Initialized for -gnatVa use, see comment above.

   Warnings_Detected : Nat := 0;
   --  Number of warnings detected. Initialized to zero at the start of
   --  compilation. Initialized for -gnatVa use, see comment above.

   ----------------------------------
   -- Error Message Mode Variables --
   ----------------------------------

   --  These variables control special error message modes. The initialized
   --  values below give the normal default behavior, but they can be reset
   --  by the caller to get different behavior as noted in the comments. These
   --  variables are not reset by calls to the error message routines, so the
   --  caller is responsible for resetting the default behavior after use.

   Error_Msg_Qual_Level : Int;
   --  Number of levels of qualification required for type name (see the
   --  description of the } insertion character. Note that this value does
   --  note get reset by any Error_Msg call, so the caller is responsible
   --  for resetting it.

   Warn_On_Instance : Boolean := False;
   --  Normally if a warning is generated in a generic template from the
   --  analysis of the template, then the warning really belongs in the
   --  template, and the default value of False for this Boolean achieves
   --  that effect. If Warn_On_Instance is set True, then the warnings are
   --  generated on the instantiation (referring to the template) rather
   --  than on the template itself.

   Raise_Exception_On_Error : Nat := 0;
   --  If this value is non-zero, then any attempt to generate an error
   --  message raises the exception Error_Msg_Exception, and the error
   --  message is not output. This is used for defending against junk
   --  resulting from illegalities, and also for substitution of more
   --  appropriate error messages from higher semantic levels. It is
   --  a counter so that the increment/decrement protocol nests neatly.
   --  Initialized for -gnatVa use, see comment above.

   Error_Msg_Exception : exception;
   --  Exception raised if Raise_Exception_On_Error is true

   Current_Error_Source_File : Source_File_Index := Internal_Source_File;
   --  Id of current messages. Used to post file name when unit changes. This
   --  is initialized to Main_Source_File at the start of a compilation, which
   --  means that no file names will be output unless there are errors in units
   --  other than the main unit. However, if the main unit has a pragma
   --  Source_Reference line, then this is initialized to No_Source_File,
   --  to force an initial reference to the real source file name.

   ----------------------------------------
   -- Error Message Insertion Parameters --
   ----------------------------------------

   --  The error message routines work with strings that contain insertion
   --  sequences that result in the insertion of variable data. The following
   --  variables contain the required data. The procedure is to set one or more
   --  of the following global variables to appropriate values before making a
   --  call to one of the error message routines with a string containing the
   --  insertion character to get the value inserted in an appropriate format.

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

   Error_Msg_File_1 : File_Name_Type;
   Error_Msg_File_2 : File_Name_Type;
   Error_Msg_File_3 : File_Name_Type;
   --  File_Name_Type values for { insertion characters in message

   Error_Msg_Unit_1 : Unit_Name_Type;
   Error_Msg_Unit_2 : Unit_Name_Type;
   --  Unit_Name_Type values for $ insertion characters in message

   Error_Msg_Node_1 : Node_Id;
   Error_Msg_Node_2 : Node_Id;
   --  Node_Id values for & insertion characters in message

   Error_Msg_Warn : Boolean;
   --  Used if current message contains a < insertion character to indicate
   --  if the current message is a warning message.

   Error_Msg_String : String (1 .. 4096);
   Error_Msg_Strlen : Natural;
   --  Used if current message contains a ~ insertion character to indicate
   --  insertion of the string Error_Msg_String (1 .. Error_Msg_Strlen).

end Err_Vars;
