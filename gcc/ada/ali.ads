------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  A L I                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
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

--  This package defines the internal data structures used for representation
--  of Ada Library Information (ALI) acquired from the ALI files generated
--  by the front end.

with Casing;  use Casing;
with Gnatvsn; use Gnatvsn;
with Namet;   use Namet;
with Rident;  use Rident;
with Table;
with Types;   use Types;

with GNAT.HTable; use GNAT.HTable;

package ALI is

   --------------
   -- Id Types --
   --------------

   --  The various entries are stored in tables with distinct subscript ranges.
   --  The following type definitions show the ranges used for the subscripts
   --  (Id values) for the various tables.

   type ALI_Id is range 0 .. 999_999;
   --  Id values used for ALIs table entries

   type Unit_Id is range 1_000_000 .. 1_999_999;
   --  Id values used for Unit table entries

   type With_Id is range 2_000_000 .. 2_999_999;
   --  Id values used for Withs table entries

   type Arg_Id is range 3_000_000 .. 3_999_999;
   --  Id values used for argument table entries

   type Sdep_Id is range 4_000_000 .. 4_999_999;
   --  Id values used for Sdep table entries

   type Source_Id is range 5_000_000 .. 5_999_999;
   --  Id values used for Source table entries

   type Interrupt_State_Id is range 6_000_000 .. 6_999_999;
   --  Id values used for Interrupt_State table entries

   type Priority_Specific_Dispatching_Id is range 7_000_000 .. 7_999_999;
   --  Id values used for Priority_Specific_Dispatching table entries

   --------------------
   -- ALI File Table --
   --------------------

   --  Each ALI file read generates an entry in the ALIs table

   No_ALI_Id : constant ALI_Id := ALI_Id'First;
   --  Special value indicating no ALI entry

   First_ALI_Entry : constant ALI_Id := No_ALI_Id + 1;
   --  Id of first actual entry in table

   type Main_Program_Type is (None, Proc, Func);
   --  Indicator of whether unit can be used as main program

   type ALIs_Record is record

      Afile : File_Name_Type;
      --  Name of ALI file

      Ofile_Full_Name : File_Name_Type;
      --  Full name of object file corresponding to the ALI file

      Sfile : File_Name_Type;
      --  Name of source file that generates this ALI file (which is equal
      --  to the name of the source file in the first unit table entry for
      --  this ALI file, since the body if present is always first).

      Ver : String (1 .. Ver_Len_Max);
      --  Value of library version (V line in ALI file). Not set if
      --  V lines are ignored as a result of the Ignore_Lines parameter.

      Ver_Len : Natural;
      --  Length of characters stored in Ver. Not set if V lines are ignored as
      --  a result of the Ignore_Lines parameter.

      SAL_Interface : Boolean;
      --  Set True when this is an interface to a standalone library

      First_Unit : Unit_Id;
      --  Id of first Unit table entry for this file

      Last_Unit : Unit_Id;
      --  Id of last Unit table entry for this file

      First_Sdep : Sdep_Id;
      --  Id of first Sdep table entry for this file

      Last_Sdep : Sdep_Id;
      --  Id of last Sdep table entry for this file

      Main_Program : Main_Program_Type;
      --  Indicator of whether first unit can be used as main program. Not set
      --  if 'M' appears in Ignore_Lines.

      Main_Priority : Int;
      --  Indicates priority value if Main_Program field indicates that this
      --  can be a main program. A value of -1 (No_Main_Priority) indicates
      --  that no parameter was found, or no M line was present. Not set if
      --  'M' appears in Ignore_Lines.

      Main_CPU : Int;
      --  Indicates processor if Main_Program field indicates that this can
      --  be a main program. A value of -1 (No_Main_CPU) indicates that no C
      --  parameter was found, or no M line was present. Not set if 'M' appears
      --  in Ignore_Lines.

      Time_Slice_Value : Int;
      --  Indicates value of time slice parameter from T=xxx on main program
      --  line. A value of -1 indicates that no T=xxx parameter was found, or
      --  no M line was present. Not set if 'M' appears in Ignore_Lines.

      Allocator_In_Body : Boolean;
      --  Set True if an AB switch appears on the main program line. False
      --  if no M line, or AB not present, or 'M appears in Ignore_Lines.

      WC_Encoding : Character;
      --  Wide character encoding if main procedure. Otherwise not relevant.
      --  Not set if 'M' appears in Ignore_Lines.

      Locking_Policy : Character;
      --  Indicates locking policy for units in this file. Space means tasking
      --  was not used, or that no Locking_Policy pragma was present or that
      --  this is a language defined unit. Otherwise set to first character
      --  (upper case) of policy name. Not set if 'P' appears in Ignore_Lines.

      Queuing_Policy : Character;
      --  Indicates queuing policy for units in this file. Space means tasking
      --  was not used, or that no Queuing_Policy pragma was present or that
      --  this is a language defined unit. Otherwise set to first character
      --  (upper case) of policy name. Not set if 'P' appears in Ignore_Lines.

      Task_Dispatching_Policy : Character;
      --  Indicates task dispatching policy for units in this file. Space means
      --  tasking was not used, or that no Task_Dispatching_Policy pragma was
      --  present or that this is a language defined unit. Otherwise set to
      --  first character (upper case) of policy name. Not set if 'P' appears
      --  in Ignore_Lines.

      Compile_Errors : Boolean;
      --  Set to True if compile errors for unit. Note that No_Object will
      --  always be set as well in this case. Not set if 'P' appears in
      --  Ignore_Lines.

      Float_Format : Character;
      --  Set to float format (set to I if no float-format given). Not set if
      --  'P' appears in Ignore_Lines.

      No_Object : Boolean;
      --  Set to True if no object file generated. Not set if 'P' appears in
      --  Ignore_Lines.

      Normalize_Scalars : Boolean;
      --  Set to True if file was compiled with Normalize_Scalars. Not set if
      --  'P' appears in Ignore_Lines.

      Unit_Exception_Table : Boolean;
      --  Set to True if unit exception table pointer generated. Not set if 'P'
      --  appears in Ignore_Lines.

      Zero_Cost_Exceptions : Boolean;
      --  Set to True if file was compiled with zero cost exceptions. Not set
      --  if 'P' appears in Ignore_Lines.

      Restrictions : Restrictions_Info;
      --  Restrictions information reconstructed from R lines

      First_Interrupt_State : Interrupt_State_Id;
      Last_Interrupt_State  : Interrupt_State_Id'Base;
      --  These point to the first and last entries in the interrupt state
      --  table for this unit. If no entries, then Last_Interrupt_State =
      --  First_Interrupt_State - 1 (that's why the 'Base reference is there,
      --  it can be one less than the lower bound of the subtype). Not set if
      --  'I' appears in Ignore_Lines

      First_Specific_Dispatching : Priority_Specific_Dispatching_Id;
      Last_Specific_Dispatching  : Priority_Specific_Dispatching_Id'Base;
      --  These point to the first and last entries in the priority specific
      --  dispatching table for this unit. If there are no entries, then
      --  Last_Specific_Dispatching = First_Specific_Dispatching - 1. That
      --  is why the 'Base reference is there, it can be one less than the
      --  lower bound of the subtype. Not set if 'S' appears in Ignore_Lines.

   end record;

   No_Main_Priority : constant Int := -1;
   --  Code for no main priority set

   No_Main_CPU : constant Int := -1;
   --  Code for no main cpu set

   package ALIs is new Table.Table (
     Table_Component_Type => ALIs_Record,
     Table_Index_Type     => ALI_Id,
     Table_Low_Bound      => First_ALI_Entry,
     Table_Initial        => 500,
     Table_Increment      => 200,
     Table_Name           => "ALIs");

   ----------------
   -- Unit Table --
   ----------------

   --  Each unit within an ALI file generates an entry in the unit table

   No_Unit_Id : constant Unit_Id := Unit_Id'First;
   --  Special value indicating no unit table entry

   First_Unit_Entry : constant Unit_Id := No_Unit_Id + 1;
   --  Id of first actual entry in table

   type Unit_Type is (Is_Spec, Is_Body, Is_Spec_Only, Is_Body_Only);
   --  Indicates type of entry, if both body and spec appear in the ALI file,
   --  then the first unit is marked Is_Body, and the second is marked Is_Spec.
   --  If only a spec appears, then it is marked as Is_Spec_Only, and if only
   --  a body appears, then it is marked Is_Body_Only).

   subtype Version_String is String (1 .. 8);
   --  Version string, taken from unit record

   type Unit_Record is record

      My_ALI : ALI_Id;
      --  Corresponding ALI entry

      Uname : Unit_Name_Type;
      --  Name of Unit

      Sfile : File_Name_Type;
      --  Name of source file

      Preelab : Boolean;
      --  Indicates presence of PR parameter for a preelaborated package

      No_Elab : Boolean;
      --  Indicates presence of NE parameter for a unit that has does not
      --  have an elaboration routine (since it has no elaboration code).

      Pure : Boolean;
      --  Indicates presence of PU parameter for a package having pragma Pure

      Dynamic_Elab : Boolean;
      --  Set to True if the unit was compiled with dynamic elaboration checks
      --  (i.e. either -gnatE or pragma Elaboration_Checks (RM) was used to
      --  compile the unit).

      Elaborate_Body : Boolean;
      --  Indicates presence of EB parameter for a package which has a pragma
      --  Elaborate_Body, and also for generic package instantiations.

      Set_Elab_Entity : Boolean;
      --  Indicates presence of EE parameter for a unit which has an
      --  elaboration entity which must be set true as part of the
      --  elaboration of the entity.

      Has_RACW : Boolean;
      --  Indicates presence of RA parameter for a package that declares at
      --  least one Remote Access to Class_Wide (RACW) object.

      Remote_Types : Boolean;
      --  Indicates presence of RT parameter for a package which has a
      --  pragma Remote_Types.

      Shared_Passive : Boolean;
      --  Indicates presence of SP parameter for a package which has a pragma
      --  Shared_Passive.

      RCI : Boolean;
      --  Indicates presence of RC parameter for a package which has a pragma
      --  Remote_Call_Interface.

      Predefined : Boolean;
      --  Indicates if unit is language predefined (or a child of such a unit)

      Internal : Boolean;
      --  Indicates if unit is an internal unit (or a child of such a unit)

      First_With : With_Id;
      --  Id of first withs table entry for this file

      Last_With : With_Id;
      --  Id of last withs table entry for this file

      First_Arg : Arg_Id;
      --  Id of first args table entry for this file

      Last_Arg : Arg_Id;
      --  Id of last args table entry for this file

      Utype : Unit_Type;
      --  Type of entry

      Is_Generic : Boolean;
      --  True for generic unit (i.e. a generic declaration, or a generic
      --  body). False for a non-generic unit.

      Unit_Kind : Character;
      --  Indicates the nature of the unit. 'p' for Packages and 's' for
      --  subprograms.

      Version : Version_String;
      --  Version of unit

      Icasing : Casing_Type;
      --  Indicates casing of identifiers in source file for this unit. This
      --  is used for informational output, and also for constructing the main
      --  unit if it is being built in Ada.

      Kcasing : Casing_Type;
      --  Indicates casing of keywords in source file for this unit. This is
      --  used for informational output, and also for constructing the main
      --  unit if it is being built in Ada.

      Elab_Position : aliased Natural;
      --  Initialized to zero. Set non-zero when a unit is chosen and
      --  placed in the elaboration order. The value represents the
      --  ordinal position in the elaboration order.

      Init_Scalars : Boolean;
      --  Set True if IS qualifier appears in ALI file, indicating that
      --  an Initialize_Scalars pragma applies to the unit.

      SAL_Interface : Boolean;
      --  Set True when this is an interface to a standalone library

      Directly_Scanned : Boolean;
      --  True iff it is a unit from an ALI file specified to gnatbind

      Body_Needed_For_SAL : Boolean;
      --  Indicates that the source for the body of the unit (subprogram,
      --  package, or generic unit) must be included in a standalone library.

      Elaborate_Body_Desirable : Boolean;
      --  Indicates that the front end elaboration circuitry decided that it
      --  would be a good idea if this package had Elaborate_Body. The binder
      --  will attempt, but does not promise, to place the elaboration call
      --  for the body right after the call for the spec, or at least as close
      --  together as possible.

      Optimize_Alignment : Character;
      --  Optimize_Alignment setting. Set to L/S/T/O for OL/OS/OT/OO present

   end record;

   package Units is new Table.Table (
     Table_Component_Type => Unit_Record,
     Table_Index_Type     => Unit_Id,
     Table_Low_Bound      => First_Unit_Entry,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Unit");

   ---------------------------
   -- Interrupt State Table --
   ---------------------------

   --  An entry is made in this table for each I (interrupt state) line
   --  encountered in the input ALI file. The First/Last_Interrupt_Id
   --  fields of the ALI file entry show the range of entries defined
   --  within a particular ALI file.

   type Interrupt_State_Record is record
      Interrupt_Id : Nat;
      --  Id from interrupt state entry

      Interrupt_State : Character;
      --  State from interrupt state entry ('u'/'r'/'s')

      IS_Pragma_Line : Nat;
      --  Line number of Interrupt_State pragma
   end record;

   package Interrupt_States is new Table.Table (
     Table_Component_Type => Interrupt_State_Record,
     Table_Index_Type     => Interrupt_State_Id'Base,
     Table_Low_Bound      => Interrupt_State_Id'First,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Interrupt_States");

   -----------------------------------------
   -- Priority Specific Dispatching Table --
   -----------------------------------------

   --  An entry is made in this table for each S (priority specific
   --  dispatching) line encountered in the input ALI file. The
   --  First/Last_Specific_Dispatching_Id fields of the ALI file
   --  entry show the range of entries defined within a particular
   --  ALI file.

   type Specific_Dispatching_Record is record
      Dispatching_Policy : Character;
      --  First character (upper case) of the corresponding policy name

      First_Priority     : Nat;
      --  Lower bound of the priority range to which the specified dispatching
      --  policy applies.

      Last_Priority      : Nat;
      --  Upper bound of the priority range to which the specified dispatching
      --  policy applies.

      PSD_Pragma_Line : Nat;
      --  Line number of Priority_Specific_Dispatching pragma
   end record;

   package Specific_Dispatching is new Table.Table (
     Table_Component_Type => Specific_Dispatching_Record,
     Table_Index_Type     => Priority_Specific_Dispatching_Id'Base,
     Table_Low_Bound      => Priority_Specific_Dispatching_Id'First,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Priority_Specific_Dispatching");

   --------------
   -- Switches --
   --------------

   --  These switches record status information about ali files that
   --  have been read, for quick reference without searching tables.

   --  Note: a switch will be left set at its default value if the line
   --  which might otherwise set it is ignored (from Ignore_Lines).

   Dynamic_Elaboration_Checks_Specified : Boolean := False;
   --  Set to False by Initialize_ALI. Set to True if Scan_ALI reads
   --  a unit for which dynamic elaboration checking is enabled.

   Float_Format_Specified : Character := ' ';
   --  Set to blank by Initialize_ALI. Set to appropriate float format
   --  character (V or I, see Opt.Float_Format) if an ali file that
   --  is read contains an F line setting the floating point format.

   Initialize_Scalars_Used : Boolean := False;
   --  Set True if an ali file contains the Initialize_Scalars flag

   Locking_Policy_Specified : Character := ' ';
   --  Set to blank by Initialize_ALI. Set to the appropriate locking policy
   --  character if an ali file contains a P line setting the locking policy.

   No_Normalize_Scalars_Specified : Boolean := False;
   --  Set to False by Initialize_ALI. Set to True if an ali file indicates
   --  that the file was compiled without normalize scalars.

   No_Object_Specified : Boolean := False;
   --  Set to False by Initialize_ALI. Set to True if an ali file contains
   --  the No_Object flag.

   Normalize_Scalars_Specified : Boolean := False;
   --  Set to False by Initialize_ALI. Set to True if an ali file indicates
   --  that the file was compiled in Normalize_Scalars mode.

   Queuing_Policy_Specified : Character := ' ';
   --  Set to blank by Initialize_ALI. Set to the appropriate queuing policy
   --  character if an ali file contains a P line setting the queuing policy.

   Cumulative_Restrictions : Restrictions_Info := No_Restrictions;
   --  This variable records the cumulative contributions of R lines in all
   --  ali files, showing whether a restriction pragma exists anywhere, and
   --  accumulating the aggregate knowledge of violations.

   Stack_Check_Switch_Set : Boolean := False;
   --  Set to True if at least one ALI file contains '-fstack-check' in its
   --  argument list.

   Static_Elaboration_Model_Used : Boolean := False;
   --  Set to False by Initialize_ALI. Set to True if any ALI file for a
   --  non-internal unit compiled with the static elaboration model is
   --  encountered.

   Task_Dispatching_Policy_Specified : Character := ' ';
   --  Set to blank by Initialize_ALI. Set to the appropriate task dispatching
   --  policy character if an ali file contains a P line setting the
   --  task dispatching policy.

   Unreserve_All_Interrupts_Specified : Boolean := False;
   --  Set to False by Initialize_ALI. Set to True if an ali file is read that
   --  has  P line specifying unreserve all interrupts mode.

   Zero_Cost_Exceptions_Specified : Boolean := False;
   --  Set to False by Initialize_ALI. Set to True if an ali file is read that
   --  has a P line specifying the generation of zero cost exceptions.

   -----------------
   -- Withs Table --
   -----------------

   --  Each With line (W line) in an ALI file generates a Withs table entry

   --  Note: there will be no entries in this table if 'W' lines are ignored

   No_With_Id : constant With_Id := With_Id'First;
   --  Special value indicating no withs table entry

   First_With_Entry : constant With_Id := No_With_Id + 1;
   --  Id of first actual entry in table

   type With_Record is record

      Uname : Unit_Name_Type;
      --  Name of Unit

      Sfile : File_Name_Type;
      --  Name of source file, set to No_File in generic case

      Afile : File_Name_Type;
      --  Name of ALI file, set to No_File in generic case

      Elaborate : Boolean;
      --  Indicates presence of E parameter

      Elaborate_All : Boolean;
      --  Indicates presence of EA parameter

      Elab_All_Desirable : Boolean;
      --  Indicates presence of AD parameter

      Elab_Desirable     : Boolean;
      --  Indicates presence of ED parameter

      SAL_Interface : Boolean := False;
      --  True if the Unit is an Interface of a Stand-Alone Library

      Limited_With : Boolean := False;
      --  True if unit is named in a limited_with_clause
   end record;

   package Withs is new Table.Table (
     Table_Component_Type => With_Record,
     Table_Index_Type     => With_Id,
     Table_Low_Bound      => First_With_Entry,
     Table_Initial        => 5000,
     Table_Increment      => 200,
     Table_Name           => "Withs");

   ---------------------
   -- Arguments Table --
   ---------------------

   --  Each Arg line (A line) in an ALI file generates an Args table entry

   --  Note: there will be no entries in this table if 'A' lines are ignored

   No_Arg_Id : constant Arg_Id := Arg_Id'First;
   --  Special value indicating no args table entry

   First_Arg_Entry : constant Arg_Id := No_Arg_Id + 1;
   --  Id of first actual entry in table

   package Args is new Table.Table (
     Table_Component_Type => String_Ptr,
     Table_Index_Type     => Arg_Id,
     Table_Low_Bound      => First_Arg_Entry,
     Table_Initial        => 1000,
     Table_Increment      => 100,
     Table_Name           => "Args");

   --------------------------
   -- Linker_Options Table --
   --------------------------

   --  If an ALI file has one of more Linker_Options lines, then a single
   --  entry is made in this table. If more than one Linker_Options lines
   --  appears in a given ALI file, then the arguments are concatenated
   --  to form the entry in this table, using a NUL character as the
   --  separator, and a final NUL character is appended to the end.

   --  Note: there will be no entries in this table if 'L' lines are ignored

   type Linker_Option_Record is record
      Name : Name_Id;
      --  Name entry containing concatenated list of Linker_Options
      --  arguments separated by NUL and ended by NUL as described above.

      Unit : Unit_Id;
      --  Unit_Id for the entry

      Internal_File : Boolean;
      --  Set True if the linker options are from an internal file. This is
      --  used to insert certain standard entries after all the user entries
      --  but before the entries from the run-time.

      Original_Pos : Positive;
      --  Keep track of original position in the linker options table. This
      --  is used to implement a stable sort when we sort the linker options
      --  table.
   end record;

   --  The indexes of active entries in this table range from 1 to the
   --  value of Linker_Options.Last. The zero'th element is for sort call.

   package Linker_Options is new Table.Table (
     Table_Component_Type => Linker_Option_Record,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 0,
     Table_Initial        => 200,
     Table_Increment      => 400,
     Table_Name           => "Linker_Options");

   -----------------
   -- Notes Table --
   -----------------

   --  The notes table records entries from N lines

   type Notes_Record is record
      Pragma_Type : Character;
      --  'A', 'C', 'I', 'S', 'T' for Annotate/Comment/Ident/Subtitle/Title

      Pragma_Line : Nat;
      --  Line number of pragma

      Pragma_Col : Nat;
      --  Column number of pragma

      Unit : Unit_Id;
      --  Unit_Id for the entry

      Pragma_Args : Name_Id;
      --  Pragma arguments. No_Name if no arguments, otherwise a single
      --  name table entry consisting of all the characters on the notes
      --  line from the first non-blank character following the source
      --  location to the last character on the line.
   end record;

   --  The indexes of active entries in this table range from 1 to the
   --  value of Linker_Options.Last. The zero'th element is for convenience
   --  if the table needs to be sorted.

   package Notes is new Table.Table (
     Table_Component_Type => Notes_Record,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 0,
     Table_Initial        => 200,
     Table_Increment      => 400,
     Table_Name           => "Notes");

   -------------------------------------------
   -- External Version Reference Hash Table --
   -------------------------------------------

   --  This hash table keeps track of external version reference strings
   --  as read from E lines in the ali file. The stored values do not
   --  include the terminating quote characters.

   --  Note: there will be no entries in this table if 'E' lines are ignored

   type Vindex is range 0 .. 98;
   --  Type to define range of headers

   function SHash (S : String_Ptr) return Vindex;
   --  Hash function for this table

   function SEq (F1, F2 : String_Ptr) return Boolean;
   --  Equality function for this table

   package Version_Ref is new Simple_HTable (
     Header_Num => Vindex,
     Element    => Boolean,
     No_Element => False,
     Key        => String_Ptr,
     Hash       => SHash,
     Equal      => SEq);

   -------------------------
   -- No_Dependency Table --
   -------------------------

   --  Each R line for a No_Dependency Restriction generates an entry in
   --  this No_Dependency table.

   type No_Dep_Record is record
      ALI_File : ALI_Id;
      --  ALI File containing the entry

      No_Dep_Unit : Name_Id;
      --  Id for names table entry including entire name, including periods
   end record;

   package No_Deps is new Table.Table (
     Table_Component_Type => No_Dep_Record,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 0,
     Table_Initial        => 200,
     Table_Increment      => 400,
     Table_Name           => "No_Deps");

   ------------------------------------
   -- Sdep (Source Dependency) Table --
   ------------------------------------

   --  Each source dependency (D line) in an ALI file generates an entry in the
   --  Sdep table.

   --  Note: there will be no entries in this table if 'D' lines are ignored

   No_Sdep_Id : constant Sdep_Id := Sdep_Id'First;
   --  Special value indicating no Sdep table entry

   First_Sdep_Entry : Sdep_Id := No_Sdep_Id + 1;
   --  Id of first Sdep entry for current ali file. This is initialized to the
   --  first Sdep entry in the table, and then incremented appropriately as
   --  successive ALI files are scanned.

   type Sdep_Record is record

      Sfile : File_Name_Type;
      --  Name of source file

      Stamp : Time_Stamp_Type;
      --  Time stamp value. Note that this will be all zero characters for the
      --  dummy entries for missing or non-dependent files.

      Checksum : Word;
      --  Checksum value. Note that this will be all zero characters for the
      --  dummy entries for missing or non-dependent files

      Dummy_Entry : Boolean;
      --  Set True for dummy entries that correspond to missing files or files
      --  where no dependency relationship exists.

      Subunit_Name : Name_Id;
      --  Name_Id for subunit name if present, else No_Name

      Rfile : File_Name_Type;
      --  Reference file name. Same as Sfile unless a Source_Reference pragma
      --  was used, in which case it reflects the name used in the pragma.

      Start_Line : Nat;
      --  Starting line number in file. Always 1, unless a Source_Reference
      --  pragma was used, in which case it reflects the line number value
      --  given in the pragma.

   end record;

   package Sdep is new Table.Table (
     Table_Component_Type => Sdep_Record,
     Table_Index_Type     => Sdep_Id,
     Table_Low_Bound      => First_Sdep_Entry,
     Table_Initial        => 5000,
     Table_Increment      => 200,
     Table_Name           => "Sdep");

   ----------------------------
   -- Use of Name Table Info --
   ----------------------------

   --  All unit names and file names are entered into the Names table. The Info
   --  fields of these entries are used as follows:

   --    Unit name           Info field has Unit_Id of unit table entry
   --    ALI file name       Info field has ALI_Id of ALI table entry
   --    Source file name    Info field has Source_Id of source table entry

   --------------------------
   -- Cross-Reference Data --
   --------------------------

   --  The following table records cross-reference sections, there is one entry
   --  for each X header line in the ALI file for an xref section.

   --  Note: there will be no entries in this table if 'X' lines are ignored

   type Xref_Section_Record is record
      File_Num : Sdep_Id;
      --  Dependency number for file (entry in Sdep.Table)

      File_Name : File_Name_Type;
      --  Name of file

      First_Entity : Nat;
      --  First entry in Xref_Entity table

      Last_Entity : Nat;
      --  Last entry in Xref_Entity table
   end record;

   package Xref_Section is new Table.Table (
     Table_Component_Type => Xref_Section_Record,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 50,
     Table_Increment      => 300,
     Table_Name           => "Xref_Section");

   --  The following is used to indicate whether a typeref field is present
   --  for the entity, and if so what kind of typeref field.

   type Tref_Kind is (
     Tref_None,    --  No typeref present
     Tref_Access,  --  Access type typeref (points to designated type)
     Tref_Derived, --  Derived type typeref (points to parent type)
     Tref_Type);   --  All other cases

   type Visibility_Kind is
     (Global, --  Library level entity
      Static, --  Static C/C++ entity
      Other); --  Local and other entity

   --  The following table records entities for which xrefs are recorded

   type Xref_Entity_Record is record
      Line : Pos;
      --  Line number of definition

      Etype : Character;
      --  Set to the identification character for the entity. See section
      --  "Cross-Reference Entity Identifiers" in lib-xref.ads for details.

      Col : Pos;
      --  Column number of definition

      Visibility : Visibility_Kind;
      --  Visibility of entity

      Entity : Name_Id;
      --  Name of entity

      Iref_File_Num : Sdep_Id;
      --  This field is set to the dependency reference for the file containing
      --  the generic entity that this one instantiates, or to No_Sdep_Id if
      --  the current entity is not an instantiation

      Iref_Line : Nat;
      --  This field is set to the line number in Iref_File_Num of the generic
      --  entity that this one instantiates, or to zero if the current entity
      --  is not an instantiation.

      Rref_Line : Nat;
      --  This field is set to the line number of a renaming reference if
      --  one is present, or to zero if no renaming reference is present

      Rref_Col : Nat;
      --  This field is set to the column number of a renaming reference
      --  if one is present, or to zero if no renaming reference is present.

      Tref : Tref_Kind;
      --  Indicates if a typeref is present, and if so what kind. Set to
      --  Tref_None if no typeref field is present.

      Tref_File_Num : Sdep_Id;
      --  This field is set to No_Sdep_Id if no typeref is present, or
      --  if the typeref refers to an entity in standard. Otherwise it
      --  it is the dependency reference for the file containing the
      --  declaration of the typeref entity.

      Tref_Line : Nat;
      --  This field is set to zero if no typeref is present, or if the
      --  typeref refers to an entity in standard. Otherwise it contains
      --  the line number of the declaration of the typeref entity.

      Tref_Type : Character;
      --  This field is set to blank if no typeref is present, or if the
      --  typeref refers to an entity in standard. Otherwise it contains
      --  the identification character for the typeref entity. See section
      --  "Cross-Reference Entity Identifiers" in lib-xref.ads for details.

      Tref_Col : Nat;
      --  This field is set to zero if no typeref is present, or if the
      --  typeref refers to an entity in standard. Otherwise it contains
      --  the column number of the declaration of the parent type.

      Tref_Standard_Entity : Name_Id;
      --  This field is set to No_Name if no typeref is present or if the
      --  typeref refers to a declared entity rather than an entity in
      --  package Standard. If there is a typeref that references an
      --  entity in package Standard, then this field is a Name_Id
      --  reference for the entity name.

      Oref_File_Num : Sdep_Id;
      --  This field is set to No_Sdep_Id if the entity doesn't override any
      --  other entity, or to the dependency reference for the overridden
      --  entity.

      Oref_Line : Nat;
      Oref_Col  : Nat;
      --  These two fields are set to the line and column of the overridden
      --  entity.

      First_Xref : Nat;
      --  Index into Xref table of first cross-reference

      Last_Xref : Nat;
      --  Index into Xref table of last cross-reference. The value in
      --  Last_Xref can be less than the First_Xref value to indicate
      --  that no entries are present in the Xref Table.
   end record;

   package Xref_Entity is new Table.Table (
     Table_Component_Type => Xref_Entity_Record,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 500,
     Table_Increment      => 300,
     Table_Name           => "Xref_Entity");

   Array_Index_Reference : constant Character := '*';
   Interface_Reference   : constant Character := 'I';
   --  Some special types of references. In the ALI file itself, these
   --  are output as attributes of the entity, not as references, but
   --  there is no provision in Xref_Entity_Record for storing multiple
   --  such references.

   --  The following table records actual cross-references

   type Xref_Record is record
      File_Num : Sdep_Id;
      --  Set to the file dependency number for the cross-reference. Note
      --  that if no file entry is present explicitly, this is just a copy
      --  of the reference for the current cross-reference section.

      Line : Nat;
      --  Line number for the reference. This is zero when referencing a
      --  predefined entity, but in this case Name is set.

      Rtype : Character;
      --  Indicates type of reference, using code used in ALI file:
      --    r = reference
      --    m = modification
      --    b = body entity
      --    c = completion of private or incomplete type
      --    x = type extension
      --    i = implicit reference
      --    Array_Index_Reference = reference to the index of an array
      --    Interface_Reference   = reference to an interface implemented
      --                            by the type
      --  See description in lib-xref.ads for further details

      Col : Nat;
      --  Column number for the reference

      Name : Name_Id := No_Name;
      --  This is only used when referencing a predefined entity. Currently,
      --  this only occurs for array indexes.

      --  Note: for instantiation references, Rtype is set to ' ', and Col is
      --  set to zero. One or more such entries can follow any other reference.
      --  When there is more than one such entry, this is to be read as:
      --     e.g. ref1  ref2  ref3
      --     ref1 is a reference to an entity that was instantied at ref2.
      --     ref2 itself is also the result of an instantiation, that took
      --     place at ref3
   end record;

   package Xref is new Table.Table (
     Table_Component_Type => Xref_Record,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 2000,
     Table_Increment      => 300,
     Table_Name           => "Xref");

   --------------------------------------
   -- Subprograms for Reading ALI File --
   --------------------------------------

   procedure Initialize_ALI;
   --  Initialize the ALI tables. Also resets all switch values to defaults

   function Scan_ALI
     (F                : File_Name_Type;
      T                : Text_Buffer_Ptr;
      Ignore_ED        : Boolean;
      Err              : Boolean;
      Read_Xref        : Boolean := False;
      Read_Lines       : String  := "";
      Ignore_Lines     : String  := "X";
      Ignore_Errors    : Boolean := False;
      Directly_Scanned : Boolean := False) return ALI_Id;
   --  Given the text, T, of an ALI file, F, scan and store the information
   --  from the file, and return the Id of the resulting entry in the ALI
   --  table. Switch settings may be modified as described above in the
   --  switch description settings.
   --
   --    Ignore_ED is normally False. If set to True, it indicates that
   --    all AD/ED (elaboration desirable) indications in the ALI file are
   --    to be ignored. This parameter is obsolete now that the -f switch
   --    is removed from gnatbind, and should be removed ???
   --
   --    Err determines the action taken on an incorrectly formatted file.
   --    If Err is False, then an error message is output, and the program
   --    is terminated. If Err is True, then no error message is output,
   --    and No_ALI_Id is returned.
   --
   --    Ignore_Lines requests that Scan_ALI ignore any lines that start
   --    with any given key character. The default value of X causes all
   --    Xref lines to be ignored. The corresponding data in the ALI
   --    tables will not be filled in this case. It is not possible
   --    to ignore U (unit) lines, they are always read.
   --
   --    Read_Lines requests that Scan_ALI process only lines that start
   --    with one of the given characters. The corresponding data in the
   --    ALI file for any characters not given in the list will not be
   --    set. The default value of the null string indicates that all
   --    lines should be read (unless Ignore_Lines is specified). U
   --    (unit) lines are always read regardless of the value of this
   --    parameter.
   --
   --    Note: either Ignore_Lines or Read_Lines should be non-null, but not
   --    both. If both are provided then only the Read_Lines value is used,
   --    and the Ignore_Lines parameter is ignored.
   --
   --    Read_XREF is set True to read and acquire the cross-reference
   --    information. If Read_XREF is set to True, then the effect is to ignore
   --    all lines other than U, W, D and X lines and the Ignore_Lines and
   --    Read_Lines parameters are ignored (i.e. the use of True for Read_XREF
   --    is equivalent to specifying an argument of "UWDX" for Read_Lines.
   --
   --    Ignore_Errors is normally False. If it is set True, then Scan_ALI
   --    will do its best to scan through a file and extract all information
   --    it can, even if there are errors. In this case Err is only set if
   --    Scan_ALI was completely unable to process the file (e.g. it did not
   --    look like an ALI file at all). Ignore_Errors is intended to improve
   --    the downward compatibility of new compilers with old tools.
   --
   --    Directly_Scanned is normally False. If it is set to True, then the
   --    units (spec and/or body) corresponding to the ALI file are marked as
   --    such. It is used to decide for what units gnatbind should generate
   --    the symbols corresponding to 'Version or 'Body_Version in
   --    Stand-Alone Libraries.

end ALI;
