------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            V M S _ C O N V                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2003-2013, Free Software Foundation, Inc.         --
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

--  This package is part of the GNAT driver. It contains the procedure
--  VMS_Conversion to convert a VMS command line to the equivalent command
--  line with switches for the GNAT tools that the GNAT driver will invoke.
--  The qualifier declarations are contained in package VMS_Data.

with Table;
with VMS_Data; use VMS_Data;
with VMS_Cmds; use VMS_Cmds;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package VMS_Conv is

   --  A table to keep the switches on the command line

   package Last_Switches is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Gnatcmd.Last_Switches");

   Normal_Exit : exception;
   --  Raise this exception for normal program termination

   Error_Exit : exception;
   --  Raise this exception if error detected

   Errors : Natural := 0;
   --  Count errors detected

   Display_Command : Boolean := False;
   --  Set true if /? switch causes display of generated command (on VMS)

   -------------------
   -- Command Table --
   -------------------

   --  The command table contains an entry for each command recognized by
   --  GNATCmd. The entries are represented by an array of records.

   type Parameter_Type is
   --  A parameter is defined as a whitespace bounded string, not beginning
   --   with a slash. (But see note under FILES_OR_WILDCARD).
     (File,
      --  A required file or directory parameter

      Optional_File,
      --  An optional file or directory parameter

      Other_As_Is,
      --  A parameter that's passed through as is (not canonicalized)

      Unlimited_Files,
      --  An unlimited number of whitespace separate file or directory
      --  parameters including wildcard specifications.

      Unlimited_As_Is,
      --  An unlimited number of whitespace separated parameters that are
      --  passed through as is (not canonicalized).

      Files_Or_Wildcard);
      --  A comma separated list of files and/or wildcard file specifications.
      --  A comma preceded by or followed by whitespace is considered as a
      --  single comma character w/o whitespace.

   type Parameter_Array is array (Natural range <>) of Parameter_Type;
   type Parameter_Ref is access all Parameter_Array;

   type Alternate_Command is (Comp, Ls, Kr, Pp, Prep);
   --  Alternate command label for non VMS system use

   Corresponding_To : constant array (Alternate_Command) of Command_Type :=
     (Comp  => Compile,
      Ls    => List,
      Kr    => Krunch,
      Prep  => Preprocess,
      Pp    => Pretty);
   --  Mapping of alternate commands to commands

   type Command_Entry is record
      Cname : String_Ptr;
      --  Command name for GNAT xxx command

      Usage : String_Ptr;
      --  A usage string, used for error messages

      Unixcmd : String_Ptr;
      --  Corresponding Unix command

      Unixsws : Argument_List_Access;
      --  Switches for the Unix command

      VMS_Only : Boolean;
      --  When True, the command can only be used on VMS

      Switches : Switches_Ptr;
      --  Pointer to array of switch strings

      Params : Parameter_Ref;
      --  Describes the allowable types of parameters.
      --  Params (1) is the type of the first parameter, etc.
      --  An empty parameter array means this command takes no parameters.

      Defext : String (1 .. 3);
      --  Default extension. If non-blank, then this extension is supplied by
      --  default as the extension for any file parameter which does not have
      --  an extension already.
   end record;

   -------------------
   -- Switch Tables --
   -------------------

   --  The switch tables contain an entry for each switch recognized by the
   --  command processor. It is initialized by procedure Initialize.

   Command_List : array (Real_Command_Type) of Command_Entry;

   ----------------
   -- Procedures --
   ----------------

   procedure Initialize;
   --  Initialized the switch table Command_List

   procedure Output_Version;
   --  Output the version of this program

   procedure VMS_Conversion (The_Command : out Command_Type);
   --  Converts VMS command line to equivalent Unix command line

end VMS_Conv;
