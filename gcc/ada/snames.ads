------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S N A M E S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.7 $
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;

package Snames is

--  This package contains definitions of standard names (i.e. entries in the
--  Names table) that are used throughout the GNAT compiler). It also contains
--  the definitions of some enumeration types whose definitions are tied to
--  the order of these preset names.

--  WARNING: There is a C file, a-snames-h which duplicates some of the
--  definitions in this file and must be kept properly synchronized.

   ------------------
   -- Preset Names --
   ------------------

   --  The following are preset entries in the names table, which are
   --  entered at the start of every compilation for easy access. Note
   --  that the order of initialization of these names in the body must
   --  be coordinated with the order of names in this table.

   --  Note: a name may not appear more than once in the following list.
   --  If additional pragmas or attributes are introduced which might
   --  otherwise cause a duplicate, then list it only once in this table,
   --  and adjust the definition of the functions for testing for pragma
   --  names and attribute names, and returning their ID values. Of course
   --  everything is simpler if no such duplications occur!

   --  First we have the one character names used to optimize the lookup
   --  process for one character identifiers (avoid the hashing in this case)
   --  There are a full 256 of these, but only the entries for lower case
   --  and upper case letters have identifiers

   --  The lower case letter entries are used for one character identifiers
   --  appearing in the source, for example in pragma Interface (C).

   Name_A         : constant Name_Id := First_Name_Id + Character'Pos ('a');
   Name_B         : constant Name_Id := First_Name_Id + Character'Pos ('b');
   Name_C         : constant Name_Id := First_Name_Id + Character'Pos ('c');
   Name_D         : constant Name_Id := First_Name_Id + Character'Pos ('d');
   Name_E         : constant Name_Id := First_Name_Id + Character'Pos ('e');
   Name_F         : constant Name_Id := First_Name_Id + Character'Pos ('f');
   Name_G         : constant Name_Id := First_Name_Id + Character'Pos ('g');
   Name_H         : constant Name_Id := First_Name_Id + Character'Pos ('h');
   Name_I         : constant Name_Id := First_Name_Id + Character'Pos ('i');
   Name_J         : constant Name_Id := First_Name_Id + Character'Pos ('j');
   Name_K         : constant Name_Id := First_Name_Id + Character'Pos ('k');
   Name_L         : constant Name_Id := First_Name_Id + Character'Pos ('l');
   Name_M         : constant Name_Id := First_Name_Id + Character'Pos ('m');
   Name_N         : constant Name_Id := First_Name_Id + Character'Pos ('n');
   Name_O         : constant Name_Id := First_Name_Id + Character'Pos ('o');
   Name_P         : constant Name_Id := First_Name_Id + Character'Pos ('p');
   Name_Q         : constant Name_Id := First_Name_Id + Character'Pos ('q');
   Name_R         : constant Name_Id := First_Name_Id + Character'Pos ('r');
   Name_S         : constant Name_Id := First_Name_Id + Character'Pos ('s');
   Name_T         : constant Name_Id := First_Name_Id + Character'Pos ('t');
   Name_U         : constant Name_Id := First_Name_Id + Character'Pos ('u');
   Name_V         : constant Name_Id := First_Name_Id + Character'Pos ('v');
   Name_W         : constant Name_Id := First_Name_Id + Character'Pos ('w');
   Name_X         : constant Name_Id := First_Name_Id + Character'Pos ('x');
   Name_Y         : constant Name_Id := First_Name_Id + Character'Pos ('y');
   Name_Z         : constant Name_Id := First_Name_Id + Character'Pos ('z');

   --  The upper case letter entries are used by expander code for local
   --  variables that do not require unique names (e.g. formal parameter
   --  names in constructed procedures)

   Name_uA        : constant Name_Id := First_Name_Id + Character'Pos ('A');
   Name_uB        : constant Name_Id := First_Name_Id + Character'Pos ('B');
   Name_uC        : constant Name_Id := First_Name_Id + Character'Pos ('C');
   Name_uD        : constant Name_Id := First_Name_Id + Character'Pos ('D');
   Name_uE        : constant Name_Id := First_Name_Id + Character'Pos ('E');
   Name_uF        : constant Name_Id := First_Name_Id + Character'Pos ('F');
   Name_uG        : constant Name_Id := First_Name_Id + Character'Pos ('G');
   Name_uH        : constant Name_Id := First_Name_Id + Character'Pos ('H');
   Name_uI        : constant Name_Id := First_Name_Id + Character'Pos ('I');
   Name_uJ        : constant Name_Id := First_Name_Id + Character'Pos ('J');
   Name_uK        : constant Name_Id := First_Name_Id + Character'Pos ('K');
   Name_uL        : constant Name_Id := First_Name_Id + Character'Pos ('L');
   Name_uM        : constant Name_Id := First_Name_Id + Character'Pos ('M');
   Name_uN        : constant Name_Id := First_Name_Id + Character'Pos ('N');
   Name_uO        : constant Name_Id := First_Name_Id + Character'Pos ('O');
   Name_uP        : constant Name_Id := First_Name_Id + Character'Pos ('P');
   Name_uQ        : constant Name_Id := First_Name_Id + Character'Pos ('Q');
   Name_uR        : constant Name_Id := First_Name_Id + Character'Pos ('R');
   Name_uS        : constant Name_Id := First_Name_Id + Character'Pos ('S');
   Name_uT        : constant Name_Id := First_Name_Id + Character'Pos ('T');
   Name_uU        : constant Name_Id := First_Name_Id + Character'Pos ('U');
   Name_uV        : constant Name_Id := First_Name_Id + Character'Pos ('V');
   Name_uW        : constant Name_Id := First_Name_Id + Character'Pos ('W');
   Name_uX        : constant Name_Id := First_Name_Id + Character'Pos ('X');
   Name_uY        : constant Name_Id := First_Name_Id + Character'Pos ('Y');
   Name_uZ        : constant Name_Id := First_Name_Id + Character'Pos ('Z');

   --  Note: the following table is read by the utility program XSNAMES and
   --  its format should not be changed without coordinating with this program.

   N : constant Name_Id := First_Name_Id + 256;
   --  Synonym used in standard name definitions

   --  Some names that are used by gigi, and whose definitions are reflected
   --  in the C header file a-snames.h. They are placed at the start so that
   --  the need to modify a-snames.h is minimized.

   Name_uParent                        : constant Name_Id := N + 000;
   Name_uTag                           : constant Name_Id := N + 001;
   Name_Off                            : constant Name_Id := N + 002;
   Name_Space                          : constant Name_Id := N + 003;
   Name_Time                           : constant Name_Id := N + 004;
   Name_uInit_Proc                     : constant Name_Id := N + 005;
   Name_uSize                          : constant Name_Id := N + 006;

   --  Some special names used by the expander. Note that the lower case u's
   --  at the start of these names get translated to extra underscores. These
   --  names are only referenced internally by expander generated code.

   Name_uAbort_Signal                  : constant Name_Id := N + 007;
   Name_uAddress_Resolver              : constant Name_Id := N + 008;
   Name_uAssign                        : constant Name_Id := N + 009;
   Name_uChain                         : constant Name_Id := N + 010;
   Name_uClean                         : constant Name_Id := N + 011;
   Name_uController                    : constant Name_Id := N + 012;
   Name_uEntry_Bodies                  : constant Name_Id := N + 013;
   Name_uExpunge                       : constant Name_Id := N + 014;
   Name_uFinal_List                    : constant Name_Id := N + 015;
   Name_uIdepth                        : constant Name_Id := N + 016;
   Name_uInit                          : constant Name_Id := N + 017;
   Name_uLocal_Final_List              : constant Name_Id := N + 018;
   Name_uMaster                        : constant Name_Id := N + 019;
   Name_uObject                        : constant Name_Id := N + 020;
   Name_uPriority                      : constant Name_Id := N + 021;
   Name_uService                       : constant Name_Id := N + 022;
   Name_uTags                          : constant Name_Id := N + 023;
   Name_uTask                          : constant Name_Id := N + 024;
   Name_uTask_Id                       : constant Name_Id := N + 025;
   Name_uTask_Info                     : constant Name_Id := N + 026;
   Name_uTask_Name                     : constant Name_Id := N + 027;
   Name_uTrace_Sp                      : constant Name_Id := N + 028;

   --  Names of routines in Ada.Finalization, needed by expander

   Name_Initialize                     : constant Name_Id := N + 029;
   Name_Adjust                         : constant Name_Id := N + 030;
   Name_Finalize                       : constant Name_Id := N + 031;

   --  Names of fields declared in System.Finalization_Implementation,
   --  needed by the expander when generating code for finalization.

   Name_Next                           : constant Name_Id := N + 032;
   Name_Prev                           : constant Name_Id := N + 033;

   --  Names of TSS routines (see Exp_TSS); Name_uInit_Proc above is also
   --  one of these.

   Name_uDeep_Adjust                   : constant Name_Id := N + 034;
   Name_uEquality                      : constant Name_Id := N + 035;
   Name_uDeep_Finalize                 : constant Name_Id := N + 036;
   Name_uDeep_Initialize               : constant Name_Id := N + 037;
   Name_uInput                         : constant Name_Id := N + 038;
   Name_uOutput                        : constant Name_Id := N + 039;
   Name_uRAS_Access                    : constant Name_Id := N + 040;
   Name_uRAS_Dereference               : constant Name_Id := N + 041;
   Name_uRead                          : constant Name_Id := N + 042;
   Name_uRep_To_Pos                    : constant Name_Id := N + 043;
   Name_uWrite                         : constant Name_Id := N + 044;

   --  Names of allocation routines, also needed by expander

   Name_Allocate                       : constant Name_Id := N + 045;
   Name_Deallocate                     : constant Name_Id := N + 046;
   Name_Dereference                    : constant Name_Id := N + 047;

   --  Names of Text_IO generic subpackages (see Rtsfind.Text_IO_Kludge)

   First_Text_IO_Package               : constant Name_Id := N + 048;
   Name_Decimal_IO                     : constant Name_Id := N + 048;
   Name_Enumeration_IO                 : constant Name_Id := N + 049;
   Name_Fixed_IO                       : constant Name_Id := N + 050;
   Name_Float_IO                       : constant Name_Id := N + 051;
   Name_Integer_IO                     : constant Name_Id := N + 052;
   Name_Modular_IO                     : constant Name_Id := N + 053;
   Last_Text_IO_Package                : constant Name_Id := N + 053;

   subtype Text_IO_Package_Name is Name_Id
     range First_Text_IO_Package .. Last_Text_IO_Package;

   --  Names of files in library for Ada.Text_IO and Ada.Wide_Text_IO

   Name_a_textio                       : constant Name_Id := N + 054;
   Name_a_witeio                       : constant Name_Id := N + 055;

   --  Some miscellaneous names used for error detection/recovery

   Name_Const                          : constant Name_Id := N + 056;
   Name_Error                          : constant Name_Id := N + 057;
   Name_Go                             : constant Name_Id := N + 058;
   Name_Put                            : constant Name_Id := N + 059;
   Name_Put_Line                       : constant Name_Id := N + 060;
   Name_To                             : constant Name_Id := N + 061;

   --  Names for packages that are treated specially by the compiler

   Name_Finalization                   : constant Name_Id := N + 062;
   Name_Finalization_Root              : constant Name_Id := N + 063;
   Name_Interfaces                     : constant Name_Id := N + 064;
   Name_Standard                       : constant Name_Id := N + 065;
   Name_System                         : constant Name_Id := N + 066;
   Name_Text_IO                        : constant Name_Id := N + 067;
   Name_Wide_Text_IO                   : constant Name_Id := N + 068;

   --  Names of identifiers used in expanding distribution stubs

   Name_Addr                           : constant Name_Id := N + 069;
   Name_Async                          : constant Name_Id := N + 070;
   Name_Get_Active_Partition_ID        : constant Name_Id := N + 071;
   Name_Get_RCI_Package_Receiver       : constant Name_Id := N + 072;
   Name_Origin                         : constant Name_Id := N + 073;
   Name_Params                         : constant Name_Id := N + 074;
   Name_Partition                      : constant Name_Id := N + 075;
   Name_Partition_Interface            : constant Name_Id := N + 076;
   Name_Ras                            : constant Name_Id := N + 077;
   Name_RCI_Name                       : constant Name_Id := N + 078;
   Name_Receiver                       : constant Name_Id := N + 079;
   Name_Result                         : constant Name_Id := N + 080;
   Name_Rpc                            : constant Name_Id := N + 081;
   Name_Subp_Id                        : constant Name_Id := N + 082;

   --  Operator Symbol entries. The actual names have an upper case O at
   --  the start in place of the Op_ prefix (e.g. the actual name that
   --  corresponds to Name_Op_Abs is "Oabs".

   First_Operator_Name                 : constant Name_Id := N + 083;
   Name_Op_Abs                         : constant Name_Id := N + 083; -- "abs"
   Name_Op_And                         : constant Name_Id := N + 084; -- "and"
   Name_Op_Mod                         : constant Name_Id := N + 085; -- "mod"
   Name_Op_Not                         : constant Name_Id := N + 086; -- "not"
   Name_Op_Or                          : constant Name_Id := N + 087; -- "or"
   Name_Op_Rem                         : constant Name_Id := N + 088; -- "rem"
   Name_Op_Xor                         : constant Name_Id := N + 089; -- "xor"
   Name_Op_Eq                          : constant Name_Id := N + 090; -- "="
   Name_Op_Ne                          : constant Name_Id := N + 091; -- "/="
   Name_Op_Lt                          : constant Name_Id := N + 092; -- "<"
   Name_Op_Le                          : constant Name_Id := N + 093; -- "<="
   Name_Op_Gt                          : constant Name_Id := N + 094; -- ">"
   Name_Op_Ge                          : constant Name_Id := N + 095; -- ">="
   Name_Op_Add                         : constant Name_Id := N + 096; -- "+"
   Name_Op_Subtract                    : constant Name_Id := N + 097; -- "-"
   Name_Op_Concat                      : constant Name_Id := N + 098; -- "&"
   Name_Op_Multiply                    : constant Name_Id := N + 099; -- "*"
   Name_Op_Divide                      : constant Name_Id := N + 100; -- "/"
   Name_Op_Expon                       : constant Name_Id := N + 101; -- "**"
   Last_Operator_Name                  : constant Name_Id := N + 101;

   --  Names for all pragmas recognized by GNAT. The entries with the comment
   --  "Ada 83" are pragmas that are defined in Ada 83, but not in Ada 95.
   --  These pragmas are fully implemented in both Ada 83 and Ada 95 modes
   --  in GNAT.

   --  The entries marked GNAT are pragmas that are defined by GNAT
   --  and implemented in both Ada 83 and Ada 95 modes. Full descriptions
   --  of these implementation dependent pragmas may be found in the
   --  appropriate section in unit Sem_Prag in file sem-prag.adb.

   --  The entries marked VMS are VMS specific pragmas that are recognized
   --  only in OpenVMS versions of GNAT. They are ignored in other versions
   --  with an appropriate warning.

   First_Pragma_Name                   : constant Name_Id := N + 102;

   --  Configuration pragmas are grouped at start

   Name_Ada_83                         : constant Name_Id := N + 102; -- GNAT
   Name_Ada_95                         : constant Name_Id := N + 103; -- GNAT
   Name_C_Pass_By_Copy                 : constant Name_Id := N + 104; -- GNAT
   Name_Component_Alignment            : constant Name_Id := N + 105; -- GNAT
   Name_Discard_Names                  : constant Name_Id := N + 106;
   Name_Elaboration_Checks             : constant Name_Id := N + 107; -- GNAT
   Name_Eliminate                      : constant Name_Id := N + 108; -- GNAT
   Name_Extend_System                  : constant Name_Id := N + 109; -- GNAT
   Name_Extensions_Allowed             : constant Name_Id := N + 110; -- GNAT
   Name_External_Name_Casing           : constant Name_Id := N + 111; -- GNAT
   Name_Float_Representation           : constant Name_Id := N + 112; -- GNAT
   Name_Initialize_Scalars             : constant Name_Id := N + 113; -- GNAT
   Name_License                        : constant Name_Id := N + 114; -- GNAT
   Name_Locking_Policy                 : constant Name_Id := N + 115;
   Name_Long_Float                     : constant Name_Id := N + 116; -- VMS
   Name_No_Run_Time                    : constant Name_Id := N + 117; -- GNAT
   Name_Normalize_Scalars              : constant Name_Id := N + 118;
   Name_Polling                        : constant Name_Id := N + 119; -- GNAT
   Name_Propagate_Exceptions           : constant Name_Id := N + 120; -- GNAT
   Name_Queuing_Policy                 : constant Name_Id := N + 121;
   Name_Ravenscar                      : constant Name_Id := N + 122;
   Name_Restricted_Run_Time            : constant Name_Id := N + 123;
   Name_Restrictions                   : constant Name_Id := N + 124;
   Name_Reviewable                     : constant Name_Id := N + 125;
   Name_Source_File_Name               : constant Name_Id := N + 126; -- GNAT
   Name_Style_Checks                   : constant Name_Id := N + 127; -- GNAT
   Name_Suppress                       : constant Name_Id := N + 128;
   Name_Task_Dispatching_Policy        : constant Name_Id := N + 129;
   Name_Unsuppress                     : constant Name_Id := N + 130; -- GNAT
   Name_Use_VADS_Size                  : constant Name_Id := N + 131; -- GNAT
   Name_Warnings                       : constant Name_Id := N + 132; -- GNAT
   Name_Validity_Checks                : constant Name_Id := N + 133; -- GNAT
   Last_Configuration_Pragma_Name      : constant Name_Id := N + 133;

   --  Remaining pragma names

   Name_Abort_Defer                    : constant Name_Id := N + 134; -- GNAT
   Name_All_Calls_Remote               : constant Name_Id := N + 135;
   Name_Annotate                       : constant Name_Id := N + 136; -- GNAT

   --  Note: AST_Entry is not in this list because its name matches the
   --  name of the corresponding attribute. However, it is included in the
   --  definition of the type Attribute_Id, and the functions Get_Pragma_Id
   --  and Check_Pragma_Id correctly recognize and process Name_AST_Entry.
   --  AST_Entry is a VMS specific pragma.

   Name_Assert                         : constant Name_Id := N + 137; -- GNAT
   Name_Asynchronous                   : constant Name_Id := N + 138;
   Name_Atomic                         : constant Name_Id := N + 139;
   Name_Atomic_Components              : constant Name_Id := N + 140;
   Name_Attach_Handler                 : constant Name_Id := N + 141;
   Name_Comment                        : constant Name_Id := N + 142; -- GNAT
   Name_Common_Object                  : constant Name_Id := N + 143; -- GNAT
   Name_Complex_Representation         : constant Name_Id := N + 144; -- GNAT
   Name_Controlled                     : constant Name_Id := N + 145;
   Name_Convention                     : constant Name_Id := N + 146;
   Name_CPP_Class                      : constant Name_Id := N + 147; -- GNAT
   Name_CPP_Constructor                : constant Name_Id := N + 148; -- GNAT
   Name_CPP_Virtual                    : constant Name_Id := N + 149; -- GNAT
   Name_CPP_Vtable                     : constant Name_Id := N + 150; -- GNAT
   Name_Debug                          : constant Name_Id := N + 151; -- GNAT
   Name_Elaborate                      : constant Name_Id := N + 152; -- Ada 83
   Name_Elaborate_All                  : constant Name_Id := N + 153;
   Name_Elaborate_Body                 : constant Name_Id := N + 154;
   Name_Export                         : constant Name_Id := N + 155;
   Name_Export_Exception               : constant Name_Id := N + 156; -- VMS
   Name_Export_Function                : constant Name_Id := N + 157; -- GNAT
   Name_Export_Object                  : constant Name_Id := N + 158; -- GNAT
   Name_Export_Procedure               : constant Name_Id := N + 159; -- GNAT
   Name_Export_Valued_Procedure        : constant Name_Id := N + 160; -- GNAT
   Name_External                       : constant Name_Id := N + 161; -- GNAT
   Name_Finalize_Storage_Only          : constant Name_Id := N + 162; -- GNAT
   Name_Ident                          : constant Name_Id := N + 163; -- VMS
   Name_Import                         : constant Name_Id := N + 164;
   Name_Import_Exception               : constant Name_Id := N + 165; -- VMS
   Name_Import_Function                : constant Name_Id := N + 166; -- GNAT
   Name_Import_Object                  : constant Name_Id := N + 167; -- GNAT
   Name_Import_Procedure               : constant Name_Id := N + 168; -- GNAT
   Name_Import_Valued_Procedure        : constant Name_Id := N + 169; -- GNAT
   Name_Inline                         : constant Name_Id := N + 170;
   Name_Inline_Always                  : constant Name_Id := N + 171; -- GNAT
   Name_Inline_Generic                 : constant Name_Id := N + 172; -- GNAT
   Name_Inspection_Point               : constant Name_Id := N + 173;
   Name_Interface                      : constant Name_Id := N + 174; -- Ada 83
   Name_Interface_Name                 : constant Name_Id := N + 175; -- GNAT
   Name_Interrupt_Handler              : constant Name_Id := N + 176;
   Name_Interrupt_Priority             : constant Name_Id := N + 177;
   Name_Java_Constructor               : constant Name_Id := N + 178; -- GNAT
   Name_Java_Interface                 : constant Name_Id := N + 179; -- GNAT
   Name_Link_With                      : constant Name_Id := N + 180; -- GNAT
   Name_Linker_Alias                   : constant Name_Id := N + 181; -- GNAT
   Name_Linker_Options                 : constant Name_Id := N + 182;
   Name_Linker_Section                 : constant Name_Id := N + 183; -- GNAT
   Name_List                           : constant Name_Id := N + 184;
   Name_Machine_Attribute              : constant Name_Id := N + 185; -- GNAT
   Name_Main                           : constant Name_Id := N + 186; -- GNAT
   Name_Main_Storage                   : constant Name_Id := N + 187; -- GNAT
   Name_Memory_Size                    : constant Name_Id := N + 188; -- Ada 83
   Name_No_Return                      : constant Name_Id := N + 189; -- GNAT
   Name_Optimize                       : constant Name_Id := N + 190;
   Name_Pack                           : constant Name_Id := N + 191;
   Name_Page                           : constant Name_Id := N + 192;
   Name_Passive                        : constant Name_Id := N + 193; -- GNAT
   Name_Preelaborate                   : constant Name_Id := N + 194;
   Name_Priority                       : constant Name_Id := N + 195;
   Name_Psect_Object                   : constant Name_Id := N + 196; -- VMS
   Name_Pure                           : constant Name_Id := N + 197;
   Name_Pure_Function                  : constant Name_Id := N + 198; -- GNAT
   Name_Remote_Call_Interface          : constant Name_Id := N + 199;
   Name_Remote_Types                   : constant Name_Id := N + 200;
   Name_Share_Generic                  : constant Name_Id := N + 201; -- GNAT
   Name_Shared                         : constant Name_Id := N + 202; -- Ada 83
   Name_Shared_Passive                 : constant Name_Id := N + 203;

   --  Note: Storage_Size is not in this list because its name matches the
   --  name of the corresponding attribute. However, it is included in the
   --  definition of the type Attribute_Id, and the functions Get_Pragma_Id
   --  and Check_Pragma_Id correctly recognize and process Name_Storage_Size.

   --  Note: Storage_Unit is also omitted from the list because of a clash
   --  with an attribute name, and is treated similarly.

   Name_Source_Reference               : constant Name_Id := N + 204; -- GNAT
   Name_Stream_Convert                 : constant Name_Id := N + 205; -- GNAT
   Name_Subtitle                       : constant Name_Id := N + 206; -- GNAT
   Name_Suppress_All                   : constant Name_Id := N + 207; -- GNAT
   Name_Suppress_Debug_Info            : constant Name_Id := N + 208; -- GNAT
   Name_Suppress_Initialization        : constant Name_Id := N + 209; -- GNAT
   Name_System_Name                    : constant Name_Id := N + 210; -- Ada 83
   Name_Task_Info                      : constant Name_Id := N + 211; -- GNAT
   Name_Task_Name                      : constant Name_Id := N + 212; -- GNAT
   Name_Task_Storage                   : constant Name_Id := N + 213; -- VMS
   Name_Time_Slice                     : constant Name_Id := N + 214; -- GNAT
   Name_Title                          : constant Name_Id := N + 215; -- GNAT
   Name_Unchecked_Union                : constant Name_Id := N + 216; -- GNAT
   Name_Unimplemented_Unit             : constant Name_Id := N + 217; -- GNAT
   Name_Unreserve_All_Interrupts       : constant Name_Id := N + 218; -- GNAT
   Name_Volatile                       : constant Name_Id := N + 219;
   Name_Volatile_Components            : constant Name_Id := N + 220;
   Name_Weak_External                  : constant Name_Id := N + 221; -- GNAT
   Last_Pragma_Name                    : constant Name_Id := N + 221;

   --  Language convention names for pragma Convention/Export/Import/Interface
   --  Note that Name_C is not included in this list, since it was already
   --  declared earlier in the context of one-character identifier names
   --  (where the order is critical to the fast look up process).

   --  Note: there are no convention names corresponding to the conventions
   --  Entry and Protected, this is because these conventions cannot be
   --  specified by a pragma.

   --  Note: The convention name C_Pass_By_Copy is treated as entirely
   --  equivalent to C except when it is specified on a record type. In
   --  this case the convention of the record type is set to C, but in
   --  addition the flag C_Pass_By_Copy is set on the record type.

   First_Convention_Name               : constant Name_Id := N + 222;
   Name_Ada                            : constant Name_Id := N + 222;
   Name_Asm                            : constant Name_Id := N + 223;
   Name_Assembler                      : constant Name_Id := N + 224;
   Name_COBOL                          : constant Name_Id := N + 225;
   Name_CPP                            : constant Name_Id := N + 226;
   Name_DLL                            : constant Name_Id := N + 227;
   Name_Fortran                        : constant Name_Id := N + 228;
   Name_Intrinsic                      : constant Name_Id := N + 229;
   Name_Java                           : constant Name_Id := N + 230;
   Name_Stdcall                        : constant Name_Id := N + 231;
   Name_Stubbed                        : constant Name_Id := N + 232;
   Name_Win32                          : constant Name_Id := N + 233;
   Last_Convention_Name                : constant Name_Id := N + 233;

   --  Other special names used in processing pragma arguments

   Name_As_Is                          : constant Name_Id := N + 234;
   Name_Body_File_Name                 : constant Name_Id := N + 235;
   Name_Casing                         : constant Name_Id := N + 236;
   Name_Code                           : constant Name_Id := N + 237;
   Name_Component                      : constant Name_Id := N + 238;
   Name_Component_Size_4               : constant Name_Id := N + 239;
   Name_Copy                           : constant Name_Id := N + 240;
   Name_D_Float                        : constant Name_Id := N + 241;
   Name_Descriptor                     : constant Name_Id := N + 242;
   Name_Default                        : constant Name_Id := N + 243;
   Name_Dot_Replacement                : constant Name_Id := N + 244;
   Name_Dynamic                        : constant Name_Id := N + 245;
   Name_Entity                         : constant Name_Id := N + 246;
   Name_External_Name                  : constant Name_Id := N + 247;
   Name_First_Optional_Parameter       : constant Name_Id := N + 248;
   Name_Form                           : constant Name_Id := N + 249;
   Name_G_Float                        : constant Name_Id := N + 250;
   Name_Gcc                            : constant Name_Id := N + 251;
   Name_Gnat                           : constant Name_Id := N + 252;
   Name_GPL                            : constant Name_Id := N + 253;
   Name_IEEE_Float                     : constant Name_Id := N + 254;
   Name_Internal                       : constant Name_Id := N + 255;
   Name_Link_Name                      : constant Name_Id := N + 256;
   Name_Lowercase                      : constant Name_Id := N + 257;
   Name_Max_Size                       : constant Name_Id := N + 258;
   Name_Mechanism                      : constant Name_Id := N + 259;
   Name_Mixedcase                      : constant Name_Id := N + 260;
   Name_Modified_GPL                   : constant Name_Id := N + 261;
   Name_Name                           : constant Name_Id := N + 262;
   Name_NCA                            : constant Name_Id := N + 263;
   Name_No                             : constant Name_Id := N + 264;
   Name_On                             : constant Name_Id := N + 265;
   Name_Parameter_Types                : constant Name_Id := N + 266;
   Name_Reference                      : constant Name_Id := N + 267;
   Name_Restricted                     : constant Name_Id := N + 268;
   Name_Result_Mechanism               : constant Name_Id := N + 269;
   Name_Result_Type                    : constant Name_Id := N + 270;
   Name_SB                             : constant Name_Id := N + 271;
   Name_Section                        : constant Name_Id := N + 272;
   Name_Semaphore                      : constant Name_Id := N + 273;
   Name_Spec_File_Name                 : constant Name_Id := N + 274;
   Name_Static                         : constant Name_Id := N + 275;
   Name_Stack_Size                     : constant Name_Id := N + 276;
   Name_Subunit_File_Name              : constant Name_Id := N + 277;
   Name_Task_Stack_Size_Default        : constant Name_Id := N + 278;
   Name_Task_Type                      : constant Name_Id := N + 279;
   Name_Time_Slicing_Enabled           : constant Name_Id := N + 280;
   Name_Top_Guard                      : constant Name_Id := N + 281;
   Name_UBA                            : constant Name_Id := N + 282;
   Name_UBS                            : constant Name_Id := N + 283;
   Name_UBSB                           : constant Name_Id := N + 284;
   Name_Unit_Name                      : constant Name_Id := N + 285;
   Name_Unknown                        : constant Name_Id := N + 286;
   Name_Unrestricted                   : constant Name_Id := N + 287;
   Name_Uppercase                      : constant Name_Id := N + 288;
   Name_VAX_Float                      : constant Name_Id := N + 289;
   Name_VMS                            : constant Name_Id := N + 290;
   Name_Working_Storage                : constant Name_Id := N + 291;

   --  Names of recognized attributes. The entries with the comment "Ada 83"
   --  are attributes that are defined in Ada 83, but not in Ada 95. These
   --  attributes are implemented in both Ada 83 and Ada 95 modes in GNAT.

   --  The entries marked GNAT are attributes that are defined by GNAT
   --  and implemented in both Ada 83 and Ada 95 modes. Full descriptions
   --  of these implementation dependent attributes may be found in the
   --  appropriate section in package Sem_Attr in file sem-attr.ads.

   --  The entries marked VMS are recognized only in OpenVMS implementations
   --  of GNAT, and are treated as illegal in all other contexts.

   First_Attribute_Name                : constant Name_Id := N + 292;
   Name_Abort_Signal                   : constant Name_Id := N + 292;  -- GNAT
   Name_Access                         : constant Name_Id := N + 293;
   Name_Address                        : constant Name_Id := N + 294;
   Name_Address_Size                   : constant Name_Id := N + 295;  -- GNAT
   Name_Aft                            : constant Name_Id := N + 296;
   Name_Alignment                      : constant Name_Id := N + 297;
   Name_Asm_Input                      : constant Name_Id := N + 298;  -- GNAT
   Name_Asm_Output                     : constant Name_Id := N + 299;  -- GNAT
   Name_AST_Entry                      : constant Name_Id := N + 300;  -- VMS
   Name_Bit                            : constant Name_Id := N + 301;  -- GNAT
   Name_Bit_Order                      : constant Name_Id := N + 302;
   Name_Bit_Position                   : constant Name_Id := N + 303;  -- GNAT
   Name_Body_Version                   : constant Name_Id := N + 304;
   Name_Callable                       : constant Name_Id := N + 305;
   Name_Caller                         : constant Name_Id := N + 306;
   Name_Code_Address                   : constant Name_Id := N + 307;  -- GNAT
   Name_Component_Size                 : constant Name_Id := N + 308;
   Name_Compose                        : constant Name_Id := N + 309;
   Name_Constrained                    : constant Name_Id := N + 310;
   Name_Count                          : constant Name_Id := N + 311;
   Name_Default_Bit_Order              : constant Name_Id := N + 312; -- GNAT
   Name_Definite                       : constant Name_Id := N + 313;
   Name_Delta                          : constant Name_Id := N + 314;
   Name_Denorm                         : constant Name_Id := N + 315;
   Name_Digits                         : constant Name_Id := N + 316;
   Name_Elaborated                     : constant Name_Id := N + 317; -- GNAT
   Name_Emax                           : constant Name_Id := N + 318; -- Ada 83
   Name_Enum_Rep                       : constant Name_Id := N + 319; -- GNAT
   Name_Epsilon                        : constant Name_Id := N + 320; -- Ada 83
   Name_Exponent                       : constant Name_Id := N + 321;
   Name_External_Tag                   : constant Name_Id := N + 322;
   Name_First                          : constant Name_Id := N + 323;
   Name_First_Bit                      : constant Name_Id := N + 324;
   Name_Fixed_Value                    : constant Name_Id := N + 325; -- GNAT
   Name_Fore                           : constant Name_Id := N + 326;
   Name_Has_Discriminants              : constant Name_Id := N + 327; -- GNAT
   Name_Identity                       : constant Name_Id := N + 328;
   Name_Img                            : constant Name_Id := N + 329; -- GNAT
   Name_Integer_Value                  : constant Name_Id := N + 330; -- GNAT
   Name_Large                          : constant Name_Id := N + 331; -- Ada 83
   Name_Last                           : constant Name_Id := N + 332;
   Name_Last_Bit                       : constant Name_Id := N + 333;
   Name_Leading_Part                   : constant Name_Id := N + 334;
   Name_Length                         : constant Name_Id := N + 335;
   Name_Machine_Emax                   : constant Name_Id := N + 336;
   Name_Machine_Emin                   : constant Name_Id := N + 337;
   Name_Machine_Mantissa               : constant Name_Id := N + 338;
   Name_Machine_Overflows              : constant Name_Id := N + 339;
   Name_Machine_Radix                  : constant Name_Id := N + 340;
   Name_Machine_Rounds                 : constant Name_Id := N + 341;
   Name_Machine_Size                   : constant Name_Id := N + 342; -- GNAT
   Name_Mantissa                       : constant Name_Id := N + 343; -- Ada 83
   Name_Max_Interrupt_Priority         : constant Name_Id := N + 344; -- GNAT
   Name_Max_Priority                   : constant Name_Id := N + 345; -- GNAT
   Name_Max_Size_In_Storage_Elements   : constant Name_Id := N + 346;
   Name_Maximum_Alignment              : constant Name_Id := N + 347; -- GNAT
   Name_Mechanism_Code                 : constant Name_Id := N + 348; -- GNAT
   Name_Model_Emin                     : constant Name_Id := N + 349;
   Name_Model_Epsilon                  : constant Name_Id := N + 350;
   Name_Model_Mantissa                 : constant Name_Id := N + 351;
   Name_Model_Small                    : constant Name_Id := N + 352;
   Name_Modulus                        : constant Name_Id := N + 353;
   Name_Null_Parameter                 : constant Name_Id := N + 354; -- GNAT
   Name_Object_Size                    : constant Name_Id := N + 355; -- GNAT
   Name_Partition_ID                   : constant Name_Id := N + 356;
   Name_Passed_By_Reference            : constant Name_Id := N + 357; -- GNAT
   Name_Pos                            : constant Name_Id := N + 358;
   Name_Position                       : constant Name_Id := N + 359;
   Name_Range                          : constant Name_Id := N + 360;
   Name_Range_Length                   : constant Name_Id := N + 361; -- GNAT
   Name_Round                          : constant Name_Id := N + 362;
   Name_Safe_Emax                      : constant Name_Id := N + 363; -- Ada 83
   Name_Safe_First                     : constant Name_Id := N + 364;
   Name_Safe_Large                     : constant Name_Id := N + 365; -- Ada 83
   Name_Safe_Last                      : constant Name_Id := N + 366;
   Name_Safe_Small                     : constant Name_Id := N + 367; -- Ada 83
   Name_Scale                          : constant Name_Id := N + 368;
   Name_Scaling                        : constant Name_Id := N + 369;
   Name_Signed_Zeros                   : constant Name_Id := N + 370;
   Name_Size                           : constant Name_Id := N + 371;
   Name_Small                          : constant Name_Id := N + 372;
   Name_Storage_Size                   : constant Name_Id := N + 373;
   Name_Storage_Unit                   : constant Name_Id := N + 374; -- GNAT
   Name_Tag                            : constant Name_Id := N + 375;
   Name_Terminated                     : constant Name_Id := N + 376;
   Name_Tick                           : constant Name_Id := N + 377; -- GNAT
   Name_To_Address                     : constant Name_Id := N + 378; -- GNAT
   Name_Type_Class                     : constant Name_Id := N + 379; -- GNAT
   Name_UET_Address                    : constant Name_Id := N + 380; -- GNAT
   Name_Unbiased_Rounding              : constant Name_Id := N + 381;
   Name_Unchecked_Access               : constant Name_Id := N + 382;
   Name_Universal_Literal_String       : constant Name_Id := N + 383; -- GNAT
   Name_Unrestricted_Access            : constant Name_Id := N + 384; -- GNAT
   Name_VADS_Size                      : constant Name_Id := N + 385; -- GNAT
   Name_Val                            : constant Name_Id := N + 386;
   Name_Valid                          : constant Name_Id := N + 387;
   Name_Value_Size                     : constant Name_Id := N + 388; -- GNAT
   Name_Version                        : constant Name_Id := N + 389;
   Name_Wchar_T_Size                   : constant Name_Id := N + 390; -- GNAT
   Name_Wide_Width                     : constant Name_Id := N + 391;
   Name_Width                          : constant Name_Id := N + 392;
   Name_Word_Size                      : constant Name_Id := N + 393; -- GNAT

   --  Attributes that designate attributes returning renamable functions,
   --  i.e. functions that return other than a universal value.

   First_Renamable_Function_Attribute  : constant Name_Id := N + 394;
   Name_Adjacent                       : constant Name_Id := N + 394;
   Name_Ceiling                        : constant Name_Id := N + 395;
   Name_Copy_Sign                      : constant Name_Id := N + 396;
   Name_Floor                          : constant Name_Id := N + 397;
   Name_Fraction                       : constant Name_Id := N + 398;
   Name_Image                          : constant Name_Id := N + 399;
   Name_Input                          : constant Name_Id := N + 400;
   Name_Machine                        : constant Name_Id := N + 401;
   Name_Max                            : constant Name_Id := N + 402;
   Name_Min                            : constant Name_Id := N + 403;
   Name_Model                          : constant Name_Id := N + 404;
   Name_Pred                           : constant Name_Id := N + 405;
   Name_Remainder                      : constant Name_Id := N + 406;
   Name_Rounding                       : constant Name_Id := N + 407;
   Name_Succ                           : constant Name_Id := N + 408;
   Name_Truncation                     : constant Name_Id := N + 409;
   Name_Value                          : constant Name_Id := N + 410;
   Name_Wide_Image                     : constant Name_Id := N + 411;
   Name_Wide_Value                     : constant Name_Id := N + 412;
   Last_Renamable_Function_Attribute   : constant Name_Id := N + 412;

   --  Attributes that designate procedures

   First_Procedure_Attribute           : constant Name_Id := N + 413;
   Name_Output                         : constant Name_Id := N + 413;
   Name_Read                           : constant Name_Id := N + 414;
   Name_Write                          : constant Name_Id := N + 415;
   Last_Procedure_Attribute            : constant Name_Id := N + 415;

   --  Remaining attributes are ones that return entities

   First_Entity_Attribute_Name         : constant Name_Id := N + 416;
   Name_Elab_Body                      : constant Name_Id := N + 416; -- GNAT
   Name_Elab_Spec                      : constant Name_Id := N + 417; -- GNAT
   Name_Storage_Pool                   : constant Name_Id := N + 418;

   --  These attributes are the ones that return types

   First_Type_Attribute_Name           : constant Name_Id := N + 419;
   Name_Base                           : constant Name_Id := N + 419;
   Name_Class                          : constant Name_Id := N + 420;
   Last_Type_Attribute_Name            : constant Name_Id := N + 420;
   Last_Entity_Attribute_Name          : constant Name_Id := N + 420;
   Last_Attribute_Name                 : constant Name_Id := N + 420;

   --  Names of recognized locking policy identifiers

   --  Note: policies are identified by the first character of the
   --  name (e.g. C for Ceiling_Locking). If new policy names are added,
   --  the first character must be distinct.

   First_Locking_Policy_Name           : constant Name_Id := N + 421;
   Name_Ceiling_Locking                : constant Name_Id := N + 421;
   Name_Inheritance_Locking            : constant Name_Id := N + 422;
   Last_Locking_Policy_Name            : constant Name_Id := N + 422;

   --  Names of recognized queuing policy identifiers.

   --  Note: policies are identified by the first character of the
   --  name (e.g. F for FIFO_Queuing). If new policy names are added,
   --  the first character must be distinct.

   First_Queuing_Policy_Name           : constant Name_Id := N + 423;
   Name_FIFO_Queuing                   : constant Name_Id := N + 423;
   Name_Priority_Queuing               : constant Name_Id := N + 424;
   Last_Queuing_Policy_Name            : constant Name_Id := N + 424;

   --  Names of recognized task dispatching policy identifiers

   --  Note: policies are identified by the first character of the
   --  name (e.g. F for FIFO_WIthinn_Priorities). If new policy names
   --  are added, the first character must be distinct.

   First_Task_Dispatching_Policy_Name  : constant Name_Id := N + 425;
   Name_Fifo_Within_Priorities         : constant Name_Id := N + 425;
   Last_Task_Dispatching_Policy_Name   : constant Name_Id := N + 425;

   --  Names of recognized checks for pragma Suppress

   First_Check_Name                    : constant Name_Id := N + 426;
   Name_Access_Check                   : constant Name_Id := N + 426;
   Name_Accessibility_Check            : constant Name_Id := N + 427;
   Name_Discriminant_Check             : constant Name_Id := N + 428;
   Name_Division_Check                 : constant Name_Id := N + 429;
   Name_Elaboration_Check              : constant Name_Id := N + 430;
   Name_Index_Check                    : constant Name_Id := N + 431;
   Name_Length_Check                   : constant Name_Id := N + 432;
   Name_Overflow_Check                 : constant Name_Id := N + 433;
   Name_Range_Check                    : constant Name_Id := N + 434;
   Name_Storage_Check                  : constant Name_Id := N + 435;
   Name_Tag_Check                      : constant Name_Id := N + 436;
   Name_All_Checks                     : constant Name_Id := N + 437;
   Last_Check_Name                     : constant Name_Id := N + 437;

   --  Names corresponding to reserved keywords, excluding those already
   --  declared in the attribute list (Access, Delta, Digits, Range).

   Name_Abort                          : constant Name_Id := N + 438;
   Name_Abs                            : constant Name_Id := N + 439;
   Name_Accept                         : constant Name_Id := N + 440;
   Name_And                            : constant Name_Id := N + 441;
   Name_All                            : constant Name_Id := N + 442;
   Name_Array                          : constant Name_Id := N + 443;
   Name_At                             : constant Name_Id := N + 444;
   Name_Begin                          : constant Name_Id := N + 445;
   Name_Body                           : constant Name_Id := N + 446;
   Name_Case                           : constant Name_Id := N + 447;
   Name_Constant                       : constant Name_Id := N + 448;
   Name_Declare                        : constant Name_Id := N + 449;
   Name_Delay                          : constant Name_Id := N + 450;
   Name_Do                             : constant Name_Id := N + 451;
   Name_Else                           : constant Name_Id := N + 452;
   Name_Elsif                          : constant Name_Id := N + 453;
   Name_End                            : constant Name_Id := N + 454;
   Name_Entry                          : constant Name_Id := N + 455;
   Name_Exception                      : constant Name_Id := N + 456;
   Name_Exit                           : constant Name_Id := N + 457;
   Name_For                            : constant Name_Id := N + 458;
   Name_Function                       : constant Name_Id := N + 459;
   Name_Generic                        : constant Name_Id := N + 460;
   Name_Goto                           : constant Name_Id := N + 461;
   Name_If                             : constant Name_Id := N + 462;
   Name_In                             : constant Name_Id := N + 463;
   Name_Is                             : constant Name_Id := N + 464;
   Name_Limited                        : constant Name_Id := N + 465;
   Name_Loop                           : constant Name_Id := N + 466;
   Name_Mod                            : constant Name_Id := N + 467;
   Name_New                            : constant Name_Id := N + 468;
   Name_Not                            : constant Name_Id := N + 469;
   Name_Null                           : constant Name_Id := N + 470;
   Name_Of                             : constant Name_Id := N + 471;
   Name_Or                             : constant Name_Id := N + 472;
   Name_Others                         : constant Name_Id := N + 473;
   Name_Out                            : constant Name_Id := N + 474;
   Name_Package                        : constant Name_Id := N + 475;
   Name_Pragma                         : constant Name_Id := N + 476;
   Name_Private                        : constant Name_Id := N + 477;
   Name_Procedure                      : constant Name_Id := N + 478;
   Name_Raise                          : constant Name_Id := N + 479;
   Name_Record                         : constant Name_Id := N + 480;
   Name_Rem                            : constant Name_Id := N + 481;
   Name_Renames                        : constant Name_Id := N + 482;
   Name_Return                         : constant Name_Id := N + 483;
   Name_Reverse                        : constant Name_Id := N + 484;
   Name_Select                         : constant Name_Id := N + 485;
   Name_Separate                       : constant Name_Id := N + 486;
   Name_Subtype                        : constant Name_Id := N + 487;
   Name_Task                           : constant Name_Id := N + 488;
   Name_Terminate                      : constant Name_Id := N + 489;
   Name_Then                           : constant Name_Id := N + 490;
   Name_Type                           : constant Name_Id := N + 491;
   Name_Use                            : constant Name_Id := N + 492;
   Name_When                           : constant Name_Id := N + 493;
   Name_While                          : constant Name_Id := N + 494;
   Name_With                           : constant Name_Id := N + 495;
   Name_Xor                            : constant Name_Id := N + 496;

   --  Names of intrinsic subprograms

   --  Note: Asm is missing from this list, since Asm is a legitimate
   --  convention name.

   First_Intrinsic_Name                : constant Name_Id := N + 497;
   Name_Divide                         : constant Name_Id := N + 497;
   Name_Enclosing_Entity               : constant Name_Id := N + 498;
   Name_Exception_Information          : constant Name_Id := N + 499;
   Name_Exception_Message              : constant Name_Id := N + 500;
   Name_Exception_Name                 : constant Name_Id := N + 501;
   Name_File                           : constant Name_Id := N + 502;
   Name_Import_Address                 : constant Name_Id := N + 503;
   Name_Import_Largest_Value           : constant Name_Id := N + 504;
   Name_Import_Value                   : constant Name_Id := N + 505;
   Name_Is_Negative                    : constant Name_Id := N + 506;
   Name_Line                           : constant Name_Id := N + 507;
   Name_Rotate_Left                    : constant Name_Id := N + 508;
   Name_Rotate_Right                   : constant Name_Id := N + 509;
   Name_Shift_Left                     : constant Name_Id := N + 510;
   Name_Shift_Right                    : constant Name_Id := N + 511;
   Name_Shift_Right_Arithmetic         : constant Name_Id := N + 512;
   Name_Source_Location                : constant Name_Id := N + 513;
   Name_Unchecked_Conversion           : constant Name_Id := N + 514;
   Name_Unchecked_Deallocation         : constant Name_Id := N + 515;
   Last_Intrinsic_Name                 : constant Name_Id := N + 515;

   --  Reserved words used only in Ada 95

   First_95_Reserved_Word              : constant Name_Id := N + 516;
   Name_Abstract                       : constant Name_Id := N + 516;
   Name_Aliased                        : constant Name_Id := N + 517;
   Name_Protected                      : constant Name_Id := N + 518;
   Name_Until                          : constant Name_Id := N + 519;
   Name_Requeue                        : constant Name_Id := N + 520;
   Name_Tagged                         : constant Name_Id := N + 521;
   Last_95_Reserved_Word               : constant Name_Id := N + 521;

   subtype Ada_95_Reserved_Words is
     Name_Id range First_95_Reserved_Word .. Last_95_Reserved_Word;

   --  Miscellaneous names used in semantic checking

   Name_Raise_Exception                : constant Name_Id := N + 522;

   --  Additional reserved words in GNAT Project Files
   --  Note that Name_External is already previously declared

   Name_Binder                         : constant Name_Id := N + 523;
   Name_Builder                        : constant Name_Id := N + 524;
   Name_Compiler                       : constant Name_Id := N + 525;
   Name_Cross_Reference                : constant Name_Id := N + 526;
   Name_Default_Switches               : constant Name_Id := N + 527;
   Name_Exec_Dir                       : constant Name_Id := N + 528;
   Name_Extends                        : constant Name_Id := N + 529;
   Name_Finder                         : constant Name_Id := N + 530;
   Name_Gnatls                         : constant Name_Id := N + 531;
   Name_Gnatstub                       : constant Name_Id := N + 532;
   Name_Implementation                 : constant Name_Id := N + 533;
   Name_Implementation_Exceptions      : constant Name_Id := N + 534;
   Name_Implementation_Suffix          : constant Name_Id := N + 535;
   Name_Languages                      : constant Name_Id := N + 536;
   Name_Library_Dir                    : constant Name_Id := N + 537;
   Name_Library_Elaboration            : constant Name_Id := N + 538;
   Name_Library_Kind                   : constant Name_Id := N + 539;
   Name_Library_Name                   : constant Name_Id := N + 540;
   Name_Library_Version                : constant Name_Id := N + 541;
   Name_Linker                         : constant Name_Id := N + 542;
   Name_Naming                         : constant Name_Id := N + 543;
   Name_Object_Dir                     : constant Name_Id := N + 544;
   Name_Project                        : constant Name_Id := N + 545;
   Name_Separate_Suffix                : constant Name_Id := N + 546;
   Name_Source_Dirs                    : constant Name_Id := N + 547;
   Name_Source_Files                   : constant Name_Id := N + 548;
   Name_Source_List_File               : constant Name_Id := N + 549;
   Name_Specification                  : constant Name_Id := N + 550;
   Name_Specification_Exceptions       : constant Name_Id := N + 551;
   Name_Specification_Suffix           : constant Name_Id := N + 552;
   Name_Switches                       : constant Name_Id := N + 553;
   --  Mark last defined name for consistency check in Snames body

   Last_Predefined_Name                : constant Name_Id := N + 553;

   subtype Any_Operator_Name is Name_Id range
     First_Operator_Name .. Last_Operator_Name;

   ------------------------------
   -- Attribute ID Definitions --
   ------------------------------

   type Attribute_Id is (
      Attribute_Abort_Signal,
      Attribute_Access,
      Attribute_Address,
      Attribute_Address_Size,
      Attribute_Aft,
      Attribute_Alignment,
      Attribute_Asm_Input,
      Attribute_Asm_Output,
      Attribute_AST_Entry,
      Attribute_Bit,
      Attribute_Bit_Order,
      Attribute_Bit_Position,
      Attribute_Body_Version,
      Attribute_Callable,
      Attribute_Caller,
      Attribute_Code_Address,
      Attribute_Component_Size,
      Attribute_Compose,
      Attribute_Constrained,
      Attribute_Count,
      Attribute_Default_Bit_Order,
      Attribute_Definite,
      Attribute_Delta,
      Attribute_Denorm,
      Attribute_Digits,
      Attribute_Elaborated,
      Attribute_Emax,
      Attribute_Enum_Rep,
      Attribute_Epsilon,
      Attribute_Exponent,
      Attribute_External_Tag,
      Attribute_First,
      Attribute_First_Bit,
      Attribute_Fixed_Value,
      Attribute_Fore,
      Attribute_Has_Discriminants,
      Attribute_Identity,
      Attribute_Img,
      Attribute_Integer_Value,
      Attribute_Large,
      Attribute_Last,
      Attribute_Last_Bit,
      Attribute_Leading_Part,
      Attribute_Length,
      Attribute_Machine_Emax,
      Attribute_Machine_Emin,
      Attribute_Machine_Mantissa,
      Attribute_Machine_Overflows,
      Attribute_Machine_Radix,
      Attribute_Machine_Rounds,
      Attribute_Machine_Size,
      Attribute_Mantissa,
      Attribute_Max_Interrupt_Priority,
      Attribute_Max_Priority,
      Attribute_Max_Size_In_Storage_Elements,
      Attribute_Maximum_Alignment,
      Attribute_Mechanism_Code,
      Attribute_Model_Emin,
      Attribute_Model_Epsilon,
      Attribute_Model_Mantissa,
      Attribute_Model_Small,
      Attribute_Modulus,
      Attribute_Null_Parameter,
      Attribute_Object_Size,
      Attribute_Partition_ID,
      Attribute_Passed_By_Reference,
      Attribute_Pos,
      Attribute_Position,
      Attribute_Range,
      Attribute_Range_Length,
      Attribute_Round,
      Attribute_Safe_Emax,
      Attribute_Safe_First,
      Attribute_Safe_Large,
      Attribute_Safe_Last,
      Attribute_Safe_Small,
      Attribute_Scale,
      Attribute_Scaling,
      Attribute_Signed_Zeros,
      Attribute_Size,
      Attribute_Small,
      Attribute_Storage_Size,
      Attribute_Storage_Unit,
      Attribute_Tag,
      Attribute_Terminated,
      Attribute_Tick,
      Attribute_To_Address,
      Attribute_Type_Class,
      Attribute_UET_Address,
      Attribute_Unbiased_Rounding,
      Attribute_Unchecked_Access,
      Attribute_Universal_Literal_String,
      Attribute_Unrestricted_Access,
      Attribute_VADS_Size,
      Attribute_Val,
      Attribute_Valid,
      Attribute_Value_Size,
      Attribute_Version,
      Attribute_Wchar_T_Size,
      Attribute_Wide_Width,
      Attribute_Width,
      Attribute_Word_Size,

      --  Attributes designating renamable functions

      Attribute_Adjacent,
      Attribute_Ceiling,
      Attribute_Copy_Sign,
      Attribute_Floor,
      Attribute_Fraction,
      Attribute_Image,
      Attribute_Input,
      Attribute_Machine,
      Attribute_Max,
      Attribute_Min,
      Attribute_Model,
      Attribute_Pred,
      Attribute_Remainder,
      Attribute_Rounding,
      Attribute_Succ,
      Attribute_Truncation,
      Attribute_Value,
      Attribute_Wide_Image,
      Attribute_Wide_Value,

      --  Attributes designating procedures

      Attribute_Output,
      Attribute_Read,
      Attribute_Write,

      --  Entity attributes (includes type attributes)

      Attribute_Elab_Body,
      Attribute_Elab_Spec,
      Attribute_Storage_Pool,

      --  Type attributes

      Attribute_Base,
      Attribute_Class);

   -------------------------------
   -- Check Name ID Definitions --
   -------------------------------

   type Check_Id is (
      Access_Check,
      Accessibility_Check,
      Discriminant_Check,
      Division_Check,
      Elaboration_Check,
      Index_Check,
      Length_Check,
      Overflow_Check,
      Range_Check,
      Storage_Check,
      Tag_Check,
      All_Checks);

   ------------------------------------
   -- Convention Name ID Definitions --
   ------------------------------------

   type Convention_Id is (

      --  The conventions that are defined by the RM come first

      Convention_Ada,
      Convention_Intrinsic,
      Convention_Entry,
      Convention_Protected,

      --  The remaining conventions are foreign language conventions

      Convention_Assembler,
      Convention_C,
      Convention_COBOL,
      Convention_CPP,
      Convention_Fortran,
      Convention_Java,
      Convention_Stdcall,
      Convention_Stubbed);

      --  Note: Conventions C_Pass_By_Copy, External, and Default are all
      --  treated as synonyms for convention C (with an appropriate flag
      --  being set in a record type in the case of C_Pass_By_Copy). See
      --  processing in Sem_Prag for details.

      --  Note: convention Win32 has the same effect as convention Stdcall
      --  and as a special exception to normal rules is considered to be
      --  conformant with convention Stdcall. Therefore if the convention
      --  Win32 is encountered, it is translated into Convention_Stdcall.

   for Convention_Id'Size use 8;
   --  Plenty of space for expansion

   subtype Foreign_Convention is
     Convention_Id range Convention_Assembler .. Convention_Stdcall;

   -----------------------------------
   -- Locking Policy ID Definitions --
   -----------------------------------

   type Locking_Policy_Id is (
      Locking_Policy_Inheritance_Locking,
      Locking_Policy_Ceiling_Locking);

   ---------------------------
   -- Pragma ID Definitions --
   ---------------------------

   type Pragma_Id is (

      --  Configuration pragmas

      Pragma_Ada_83,
      Pragma_Ada_95,
      Pragma_C_Pass_By_Copy,
      Pragma_Component_Alignment,
      Pragma_Discard_Names,
      Pragma_Elaboration_Checks,
      Pragma_Eliminate,
      Pragma_Extend_System,
      Pragma_Extensions_Allowed,
      Pragma_External_Name_Casing,
      Pragma_Float_Representation,
      Pragma_Initialize_Scalars,
      Pragma_License,
      Pragma_Locking_Policy,
      Pragma_Long_Float,
      Pragma_No_Run_Time,
      Pragma_Normalize_Scalars,
      Pragma_Polling,
      Pragma_Propagate_Exceptions,
      Pragma_Queuing_Policy,
      Pragma_Ravenscar,
      Pragma_Restricted_Run_Time,
      Pragma_Restrictions,
      Pragma_Reviewable,
      Pragma_Source_File_Name,
      Pragma_Style_Checks,
      Pragma_Suppress,
      Pragma_Task_Dispatching_Policy,
      Pragma_Unsuppress,
      Pragma_Use_VADS_Size,
      Pragma_Warnings,
      Pragma_Validity_Checks,

      --  Remaining (non-configuration) pragmas

      Pragma_Abort_Defer,
      Pragma_All_Calls_Remote,
      Pragma_Annotate,
      Pragma_Assert,
      Pragma_Asynchronous,
      Pragma_Atomic,
      Pragma_Atomic_Components,
      Pragma_Attach_Handler,
      Pragma_Comment,
      Pragma_Common_Object,
      Pragma_Complex_Representation,
      Pragma_Controlled,
      Pragma_Convention,
      Pragma_CPP_Class,
      Pragma_CPP_Constructor,
      Pragma_CPP_Virtual,
      Pragma_CPP_Vtable,
      Pragma_Debug,
      Pragma_Elaborate,
      Pragma_Elaborate_All,
      Pragma_Elaborate_Body,
      Pragma_Export,
      Pragma_Export_Exception,
      Pragma_Export_Function,
      Pragma_Export_Object,
      Pragma_Export_Procedure,
      Pragma_Export_Valued_Procedure,
      Pragma_External,
      Pragma_Finalize_Storage_Only,
      Pragma_Ident,
      Pragma_Import,
      Pragma_Import_Exception,
      Pragma_Import_Function,
      Pragma_Import_Object,
      Pragma_Import_Procedure,
      Pragma_Import_Valued_Procedure,
      Pragma_Inline,
      Pragma_Inline_Always,
      Pragma_Inline_Generic,
      Pragma_Inspection_Point,
      Pragma_Interface,
      Pragma_Interface_Name,
      Pragma_Interrupt_Handler,
      Pragma_Interrupt_Priority,
      Pragma_Java_Constructor,
      Pragma_Java_Interface,
      Pragma_Link_With,
      Pragma_Linker_Alias,
      Pragma_Linker_Options,
      Pragma_Linker_Section,
      Pragma_List,
      Pragma_Machine_Attribute,
      Pragma_Main,
      Pragma_Main_Storage,
      Pragma_Memory_Size,
      Pragma_No_Return,
      Pragma_Optimize,
      Pragma_Pack,
      Pragma_Page,
      Pragma_Passive,
      Pragma_Preelaborate,
      Pragma_Priority,
      Pragma_Psect_Object,
      Pragma_Pure,
      Pragma_Pure_Function,
      Pragma_Remote_Call_Interface,
      Pragma_Remote_Types,
      Pragma_Share_Generic,
      Pragma_Shared,
      Pragma_Shared_Passive,
      Pragma_Source_Reference,
      Pragma_Stream_Convert,
      Pragma_Subtitle,
      Pragma_Suppress_All,
      Pragma_Suppress_Debug_Info,
      Pragma_Suppress_Initialization,
      Pragma_System_Name,
      Pragma_Task_Info,
      Pragma_Task_Name,
      Pragma_Task_Storage,
      Pragma_Time_Slice,
      Pragma_Title,
      Pragma_Unchecked_Union,
      Pragma_Unimplemented_Unit,
      Pragma_Unreserve_All_Interrupts,
      Pragma_Volatile,
      Pragma_Volatile_Components,
      Pragma_Weak_External,

      --  The following pragmas are on their own, out of order, because of
      --  the special processing required to deal with the fact that their
      --  names match existing attribute names.

      Pragma_AST_Entry,
      Pragma_Storage_Size,
      Pragma_Storage_Unit);

   -----------------------------------
   -- Queuing Policy ID definitions --
   -----------------------------------

   type Queuing_Policy_Id is (
      Queuing_Policy_FIFO_Queuing,
      Queuing_Policy_Priority_Queuing);

   --------------------------------------------
   -- Task Dispatching Policy ID definitions --
   --------------------------------------------

   type Task_Dispatching_Policy_Id is (
      Task_Dispatching_FIFO_Within_Priorities);
   --  Id values used to identify task dispatching policies

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Called to initialize the preset names in the names table.

   function Is_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized attribute

   function Is_Entity_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized entity attribute,
   --  i.e. an attribute reference that returns an entity.

   function Is_Procedure_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized attribute that
   --  designates a procedure (and can therefore appear as a statement).

   function Is_Function_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized attribute
   --  that designates a renameable function, and can therefore appear in
   --  a renaming statement. Note that not all attributes designating
   --  functions are renamable, in particular, thos returning a universal
   --  value cannot be renamed.

   function Is_Type_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized type attribute,
   --  i.e. an attribute reference that returns a type

   function Is_Check_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized suppress check
   --  as required by pragma Suppress.

   function Is_Convention_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of one of the recognized language
   --  conventions, as required by pragma Convention, Import, Export, Interface

   function Is_Locking_Policy_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized locking policy

   function Is_Operator_Symbol_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of an operator symbol

   function Is_Pragma_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized pragma. Note
   --  that pragmas AST_Entry, Storage_Size, and Storage_Unit are recognized
   --  as pragmas by this function even though their names are separate from
   --  the other pragma names.

   function Is_Queuing_Policy_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized queuing policy

   function Is_Task_Dispatching_Policy_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized
   --  task dispatching policy

   function Get_Attribute_Id (N : Name_Id) return Attribute_Id;
   --  Returns Id of attribute corresponding to given name. It is an error to
   --  call this function with a name that is not the name of a attribute.

   function Get_Convention_Id (N : Name_Id) return Convention_Id;
   --  Returns Id of language convention corresponding to given name. It is an
   --  to call this function with a name that is not the name of a check.

   function Get_Check_Id (N : Name_Id) return Check_Id;
   --  Returns Id of suppress check corresponding to given name. It is an error
   --  to call this function with a name that is not the name of a check.

   function Get_Locking_Policy_Id (N : Name_Id) return Locking_Policy_Id;
   --  Returns Id of locking policy corresponding to given name. It is an error
   --  to call this function with a name that is not the name of a check.

   function Get_Pragma_Id (N : Name_Id) return Pragma_Id;
   --  Returns Id of pragma corresponding to given name. It is an error to
   --  call this function with a name that is not the name of a pragma. Note
   --  that the function also works correctly for names of pragmas that are
   --  not in the main list of pragma Names (AST_Entry, Storage_Size, and
   --  Storage_Unit (e.g. Name_Storage_Size returns Pragma_Storage_Size).

   function Get_Queuing_Policy_Id (N : Name_Id) return Queuing_Policy_Id;
   --  Returns Id of queuing policy corresponding to given name. It is an error
   --  to call this function with a name that is not the name of a check.

   function Get_Task_Dispatching_Policy_Id
     (N    : Name_Id)
      return Task_Dispatching_Policy_Id;
   --  Returns Id of task dispatching policy corresponding to given name. It
   --  is an error to call this function with a name that is not the name
   --  of a check.

private
   pragma Inline (Is_Attribute_Name);
   pragma Inline (Is_Entity_Attribute_Name);
   pragma Inline (Is_Type_Attribute_Name);
   pragma Inline (Is_Check_Name);
   pragma Inline (Is_Convention_Name);
   pragma Inline (Is_Locking_Policy_Name);
   pragma Inline (Is_Operator_Symbol_Name);
   pragma Inline (Is_Queuing_Policy_Name);
   pragma Inline (Is_Pragma_Name);
   pragma Inline (Is_Task_Dispatching_Policy_Name);

end Snames;
