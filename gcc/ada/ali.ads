------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  A L I                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.6 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

--  This package defines the internal data structures used for representation
--  of Ada Library Information (ALI) acquired from the ALI files generated
--  by the front end.

with Casing;  use Casing;
with Gnatvsn; use Gnatvsn;
with Rident;  use Rident;
with Table;
with Types;   use Types;

with GNAT.HTable; use GNAT.HTable;

package ALI is

   --------------
   -- Id Types --
   --------------

   --  The various entries are stored in tables with distinct subscript
   --  ranges. The following type definitions indicate the ranges used
   --  for the subscripts (Id values) for the various tables.

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

   type Restrictions_String is array (Partition_Restrictions) of Character;
   --  Type used to hold string from R line

   type ALIs_Record is record

      Afile : File_Name_Type;
      --  Name of ALI file

      Ofile_Full_Name : Name_Id;
      --  Full name of object file corresponding to the ALI file

      Sfile : File_Name_Type;
      --  Name of source file that generates this ALI file (which is equal
      --  to the name of the source file in the first unit table entry for
      --  this ALI file, since the body if present is always first).

      Ver : String (1 .. Ver_Len_Max);
      --  Value of library version (V line in ALI file)

      Ver_Len : Natural;
      --  Length of characters stored in Ver

      First_Unit : Unit_Id;
      --  Id of first Unit table entry for this file

      Last_Unit : Unit_Id;
      --  Id of last Unit table entry for this file

      First_Sdep : Sdep_Id;
      --  Id of first Sdep table entry for this file

      Last_Sdep : Sdep_Id;
      --  Id of last Sdep table entry for this file

      Main_Program : Main_Program_Type;
      --  Indicator of whether first unit can be used as main program

      Main_Priority : Int;
      --  Indicates priority value if Main_Program field indicates that
      --  this can be a main program. A value of -1 (No_Main_Priority)
      --  indicates that no parameter was found, or no M line was present.

      Time_Slice_Value : Int;
      --  Indicates value of time slice parameter from T=xxx on main program
      --  line. A value of -1 indicates that no T=xxx parameter was found,
      --  or no M line was present.

      WC_Encoding : Character;
      --  Wide character encoding if main procedure. Otherwise not relevant.

      Locking_Policy : Character;
      --  Indicates locking policy for units in this file. Space means
      --  tasking was not used, or that no Locking_Policy pragma was
      --  present or that this is a language defined unit. Otherwise set
      --  to first character (upper case) of policy name.

      Queuing_Policy : Character;
      --  Indicates queuing policy for units in this file. Space means
      --  tasking was not used, or that no Queuing_Policy pragma was
      --  present or that this is a language defined unit. Otherwise set
      --  to first character (upper case) of policy name.

      Task_Dispatching_Policy : Character;
      --  Indicates task dispatching policy for units in this file. Space
      --  means tasking was not used, or that no Task_Dispatching_Policy
      --  pragma was present or that this is a language defined unit.
      --  Otherwise set to first character (upper case) of policy name.

      Compile_Errors : Boolean;
      --  Set to True if compile errors for unit. Note that No_Object
      --  will always be set as well in this case.

      Float_Format : Character;
      --  Set to float format (set to I if no float-format given)

      No_Object : Boolean;
      --  Set to True if no object file generated

      No_Run_Time : Boolean;
      --  Set to True if file was compiled with pragma No_Run_Time

      Normalize_Scalars : Boolean;
      --  Set to True if file was compiled with Normalize_Scalars

      Unit_Exception_Table : Boolean;
      --  Set to True if unit exception table pointer generated

      Zero_Cost_Exceptions : Boolean;
      --  Set to True if file was compiled with zero cost exceptions

      Restrictions : Restrictions_String;
      --  Copy of restrictions letters from R line

   end record;

   No_Main_Priority : constant Int := -1;
   --  Code for no main priority set

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
      --  Indicates presence of PU parameter for a pure package

      Dynamic_Elab : Boolean;
      --  Set to True if the unit was compiled with dynamic elaboration
      --  checks (i.e. either -gnatE or pragma Elaboration_Checks (Static)
      --  was used to compile the unit).

      Elaborate_Body : Boolean;
      --  Indicates presence of EB parameter for a package which has a
      --  pragma Preelaborate_Body.

      Set_Elab_Entity : Boolean;
      --  Indicates presence of EE parameter for a unit which has an
      --  elaboration entity which must be set true as part of the
      --  elaboration of the entity.

      Has_RACW : Boolean;
      --  Indicates presence of RA parameter for a package that declares
      --  at least one Remote Access to Class_Wide (RACW) object.

      Remote_Types : Boolean;
      --  Indicates presence of RT parameter for a package which has a
      --  pragma Remote_Types.

      Shared_Passive : Boolean;
      --  Indicates presence of SP parameter for a package which has a
      --  pragma Shared_Passive.

      RCI : Boolean;
      --  Indicates presence of RC parameter for a package which has a
      --  pragma Remote_Call_Interface.

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
      --  is used for informational output, and also for constructing the
      --  main unit if it is being built in Ada.

      Kcasing : Casing_Type;
      --  Indicates casing of keyowords in source file for this unit. This
      --  is used for informational output, and also for constructing the
      --  main unit if it is being built in Ada.

      Elab_Position : aliased Natural;
      --  Initialized to zero. Set non-zero when a unit is chosen and
      --  placed in the elaboration order. The value represents the
      --  ordinal position in the elaboration order.

      Init_Scalars : Boolean;
      --  Set True if IS qualifier appears in ALI file, indicating that
      --  an Initialize_Scalars pragma applies to the unit.

   end record;

   package Units is new Table.Table (
     Table_Component_Type => Unit_Record,
     Table_Index_Type     => Unit_Id,
     Table_Low_Bound      => First_Unit_Entry,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Unit");

   --------------
   -- Switches --
   --------------

   --  These switches record status information about ali files that
   --  have been read, for quick reference without searching tables.

   Dynamic_Elaboration_Checks_Specified : Boolean := False;
   --  Set to False by Initialize_ALI. Set to True if Read_ALI reads
   --  a unit for which dynamic elaboration checking is enabled.

   Float_Format_Specified : Character := ' ';
   --  Set to blank by Initialize_ALI. Set to appropriate float format
   --  character (V or I, see Opt.Float_Format) if an an ali file that
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

   No_Run_Time_Specified : Boolean := False;
   --  Set to False by Initialize_ALI, Set to True if an ali file indicates
   --  that the file was compiled in No_Run_Time mode.

   Queuing_Policy_Specified : Character := ' ';
   --  Set to blank by Initialize_ALI. Set to the appropriate queuing policy
   --  character if an ali file contains a P line setting the queuing policy.

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
      --  Indicates presence of ED parameter

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

   --  Each unique linker option (L line) in an ALI file generates
   --  an entry in the Linker_Options table. Note that only unique
   --  entries are stored, i.e. if the same entry appears twice, the
   --  second entry is suppressed. Each entry is a character sequence
   --  terminated by a NUL character.

   type Linker_Option_Record is record
      Name          : Name_Id;
      Unit          : Unit_Id;
      Internal_File : Boolean;
      Original_Pos  : Positive;
   end record;

   --  Declare the Linker_Options Table

   --  The indexes of active entries in this table range from 1 to the
   --  value of Linker_Options.Last. The zero'th element is for sort call.

   package Linker_Options is new Table.Table (
     Table_Component_Type => Linker_Option_Record,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 0,
     Table_Initial        => 200,
     Table_Increment      => 400,
     Table_Name           => "Linker_Options");

   -------------------------------------------
   -- External Version Reference Hash Table --
   -------------------------------------------

   --  This hash table keeps track of external version reference strings
   --  as read from E lines in the ali file. The stored values do not
   --  include the terminating quote characters.

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

   ------------------------------------
   -- Sdep (Source Dependency) Table --
   ------------------------------------

   --  Each source dependency (D line) in an ALI file generates an
   --  entry in the Sdep table.

   No_Sdep_Id : constant Sdep_Id := Sdep_Id'First;
   --  Special value indicating no Sdep table entry

   First_Sdep_Entry : constant Sdep_Id := No_Sdep_Id + 1;
   --  Id of first actual entry in table

   type Sdep_Record is record

      Sfile : File_Name_Type;
      --  Name of source file

      Stamp : Time_Stamp_Type;
      --  Time stamp value. Note that this will be all zero characters
      --  for the dummy entries for missing or non-dependent files.

      Checksum : Word;
      --  Checksum value. Note that this will be all zero characters
      --  for the dummy entries for missing or non-dependent files

      Dummy_Entry : Boolean;
      --  Set True for dummy entries that correspond to missing files
      --  or files where no dependency relationship exists.

      Subunit_Name : Name_Id;
      --  Name_Id for subunit name if present, else No_Name

      Rfile : File_Name_Type;
      --  Reference file name. Same as Sfile unless a Source_Reference
      --  pragma was used, in which case it reflects the name used in
      --  the pragma.

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

   --  All unit names and file names are entered into the Names table. The
   --  Info fields of these entries are used as follows:

   --    Unit name           Info field has Unit_Id of unit table entry
   --    ALI file name       Info field has ALI_Id of ALI table entry
   --    Source file name    Info field has Source_Id of source table entry

   --------------------------
   -- Cross-Reference Data --
   --------------------------

   --  The following table records cross-reference sections, there is one
   --  entry for each X header line in the ALI file for an xref section.
   --  Note that there will be no entries in this table if the Read_Xref
   --  parameter to Scan_ALI was set to False.

   type Xref_Section_Record is record
      File_Num : Sdep_Id;
      --  Dependency number for file (entry in Sdep.Table)

      File_Name : Name_Id;
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

   --  The following table records entities for which xrefs are recorded

   type Xref_Entity_Record is record
      Line : Pos;
      --  Line number of definition

      Etype : Character;
      --  Set to the identification character for the entity. See section
      --  "Cross-Reference Entity Identifiers in lib-xref.ads for details.

      Col : Pos;
      --  Column number of definition

      Lib : Boolean;
      --  True if entity is library level entity

      Entity : Name_Id;
      --  Name of entity

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
      --  "Cross-Reference Entity Identifiers in lib-xref.ads for details.

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

   --  The following table records actual cross-references

   type Xref_Record is record
      File_Num : Sdep_Id;
      --  Set to the file dependency number for the cross-reference. Note
      --  that if no file entry is present explicitly, this is just a copy
      --  of the reference for the current cross-reference section.

      Line : Pos;
      --  Line number for the reference

      Rtype : Character;
      --  Indicates type of reference, using code used in ALI file:
      --    r = reference
      --    m = modification
      --    b = body entity
      --    c = completion of private or incomplete type
      --    x = type extension
      --    i = implicit reference
      --  See description in lib-xref.ads for further details

      Col : Nat;
      --  Column number for the reference

      --  Note: for instantiation references, Rtype is set to ' ', and Col is
      --  set to zero. One or more such entries can follow any other reference.
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
   --  Initialize the ALI tables. Also resets all switch values to defaults.

   function Scan_ALI
     (F         : File_Name_Type;
      T         : Text_Buffer_Ptr;
      Ignore_ED : Boolean;
      Err       : Boolean;
      Read_Xref : Boolean := False)
      return      ALI_Id;
   --  Given the text, T, of an ALI file, F, scan and store the information
   --  from the file, and return the Id of the resulting entry in the ALI
   --  table. Switch settings may be modified as described above in the
   --  switch description settings.
   --
   --    Ignore_ED is normally False. If set to True, it indicates that
   --    all ED (elaboration desirable) indications in the ALI file are
   --    to be ignored.
   --
   --    Err determines the action taken on an incorrectly formatted file.
   --    If Err is False, then an error message is output, and the program
   --    is terminated. If Err is True, then no error message is output,
   --    and No_ALI_Id is returned.
   --
   --    Read_XREF is set True to read and acquire the cross-reference
   --    information, otherwise the scan is terminated when a cross-
   --    reference line is encountered.

end ALI;
