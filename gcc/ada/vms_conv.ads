------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            V M S _ C O N V                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2003 Free Software Foundation, Inc.               --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package is part of the GNAT driver. It contains a procedure
--  VMS_Conversion to convert the command line in VMS form to the equivalent
--  command line with switches for the GNAT tools that the GNAT driver will
--  invoke.
--
--  The qualifier declarations are contained in package VMS_Data.

with Table;
with VMS_Data; use VMS_Data;

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
   -- COMMAND TABLE --
   -------------------

   --  The command table contains an entry for each command recognized by
   --  GNATCmd. The entries are represented by an array of records.

   type Parameter_Type is
   --  A parameter is defined as a whitespace bounded string, not begining
   --   with a slash. (But see note under FILES_OR_WILDCARD).
     (File,
      --  A required file or directory parameter.

      Optional_File,
      --  An optional file or directory parameter.

      Other_As_Is,
      --  A parameter that's passed through as is (not canonicalized)

      Unlimited_Files,
      --  An unlimited number of whitespace separate file or directory
      --  parameters including wildcard specifications.

      Unlimited_As_Is,
      --  Un unlimited number of whitespace separated paameters that are
      --  passed through as is (not canonicalized).

      Files_Or_Wildcard);
      --  A comma separated list of files and/or wildcard file specifications.
      --  A comma preceded by or followed by whitespace is considered as a
      --  single comma character w/o whitespace.

   type Parameter_Array is array (Natural range <>) of Parameter_Type;
   type Parameter_Ref is access all Parameter_Array;

   type Command_Type is
     (Bind, Chop, Clean, Compile, Elim, Find, Krunch, Library, Link, List,
      Make, Name, Preprocess, Pretty, Shared, Stub, Xref, Undefined);

   type Alternate_Command is (Comp, Ls, Kr, Pp, Prep);
   --  Alternate command libel for non VMS system

   Corresponding_To : constant array (Alternate_Command) of Command_Type :=
     (Comp  => Compile,
      Ls    => List,
      Kr    => Krunch,
      Prep  => Preprocess,
      Pp    => Pretty);
   --  Mapping of alternate commands to commands

   subtype Real_Command_Type is Command_Type range Bind .. Xref;

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

   -------------------------
   -- INTERNAL STRUCTURES --
   -------------------------

   --  The switches and commands are defined by strings in the previous
   --  section so that they are easy to modify, but internally, they are
   --  kept in a more conveniently accessible form described in this
   --  section.

   --  Commands, command qualifers and options have a similar common format
   --  so that searching for matching names can be done in a common manner.

   type Item_Id is (Id_Command, Id_Switch, Id_Option);

   type Translation_Type is
     (
      T_Direct,
      --  A qualifier with no options.
      --  Example: GNAT MAKE /VERBOSE

      T_Directories,
      --  A qualifier followed by a list of directories
      --  Example: GNAT COMPILE /SEARCH=([], [.FOO], [.BAR])

      T_Directory,
      --  A qualifier followed by one directory
      --  Example: GNAT LIBRARY /SET=[.VAXFLOATLIB]

      T_File,
      --  A qualifier followed by a filename
      --  Example: GNAT LINK /EXECUTABLE=FOO.EXE

      T_No_Space_File,
      --  A qualifier followed by a filename
      --  Example: GNAT MAKE /PROJECT_FILE=PRJ.GPR

      T_Numeric,
      --  A qualifier followed by a numeric value.
      --  Example: GNAT CHOP /FILE_NAME_MAX_LENGTH=39

      T_String,
      --  A qualifier followed by a quoted string. Only used by
      --  /IDENTIFICATION qualifier.
      --  Example: GNAT LINK /IDENTIFICATION="3.14a1 version"

      T_Options,
      --  A qualifier followed by a list of options.
      --  Example: GNAT COMPILE /REPRESENTATION_INFO=(ARRAYS,OBJECTS)

      T_Commands,
      --  A qualifier followed by a list. Only used for
      --  MAKE /COMPILER_QUALIFIERS /BINDER_QUALIFIERS /LINKER_QUALIFIERS
      --  (gnatmake -cargs -bargs -largs )
      --  Example: GNAT MAKE ... /LINKER_QUALIFIERS /VERBOSE FOOBAR.OBJ

      T_Other,
      --  A qualifier passed directly to the linker. Only used
      --  for LINK and SHARED if no other match is found.
      --  Example: GNAT LINK FOO.ALI /SYSSHR

      T_Alphanumplus
      --  A qualifier followed by a legal linker symbol prefix. Only used
      --  for BIND /BUILD_LIBRARY (gnatbind -Lxyz).
      --  Example: GNAT BIND /BUILD_LIBRARY=foobar
      );

   type Item (Id : Item_Id);
   type Item_Ptr is access all Item;

   type Item (Id : Item_Id) is record
      Name : String_Ptr;
      --  Name of the command, switch (with slash) or option

      Next : Item_Ptr;
      --  Pointer to next item on list, always has the same Id value

      Command : Command_Type := Undefined;

      Unix_String : String_Ptr := null;
      --  Corresponding Unix string. For a command, this is the unix command
      --  name and possible default switches. For a switch or option it is
      --  the unix switch string.

      case Id is

         when Id_Command =>

            Switches : Item_Ptr;
            --  Pointer to list of switch items for the command, linked
            --  through the Next fields with null terminating the list.

            Usage : String_Ptr;
            --  Usage information, used only for errors and the default
            --  list of commands output.

            Params : Parameter_Ref;
            --  Array of parameters

            Defext : String (1 .. 3);
            --  Default extension. If non-blank, then this extension is
            --  supplied by default as the extension for any file parameter
            --  which does not have an extension already.

         when Id_Switch =>

            Translation : Translation_Type;
            --  Type of switch translation. For all cases, except Options,
            --  this is the only field needed, since the Unix translation
            --  is found in Unix_String.

            Options : Item_Ptr;
            --  For the Options case, this field is set to point to a list
            --  of options item (for this case Unix_String is null in the
            --  main switch item). The end of the list is marked by null.

         when Id_Option =>

            null;
            --  No special fields needed, since Name and Unix_String are
            --  sufficient to completely described an option.

      end case;
   end record;

   subtype Command_Item is Item (Id_Command);
   subtype Switch_Item  is Item (Id_Switch);
   subtype Option_Item  is Item (Id_Option);

   ------------------
   -- SWITCH TABLE --
   ------------------

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
