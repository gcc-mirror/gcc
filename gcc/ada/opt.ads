------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  O P T                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.4 $
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

--  This package contains global switches set by the initialization
--  routine from the command line and referenced throughout the compiler,
--  the binder or gnatmake. The comments indicate which options are used by
--  which programs (GNAT, GNATBIND, GNATMAKE).

with Hostparm;       use Hostparm;
with Types;          use Types;
with System.WCh_Con; use System.WCh_Con;

package Opt is

   ----------------------------------------------
   -- Settings of Modes for Current Processing --
   ----------------------------------------------

   --  The following mode values represent the current state of processing.
   --  The values set here are the default values. Unless otherwise noted,
   --  the value may be reset in Switch with an appropropiate switch. In
   --  some cases, the values can also be modified by pragmas, and in the
   --  case of some binder variables, Gnatbind.Scan_Bind_Arg may modify
   --  the default values.

   Ada_Bind_File : Boolean := True;
   --  GNATBIND
   --  Set True if binder file to be generated in Ada rather than C

   Ada_95 : Boolean := True;
   --  GNAT
   --  Set True if operating in Ada 95 mode
   --  Set False if operating in Ada 83 mode

   Ada_83 : Boolean := False;
   --  GNAT
   --  Set True if operating in Ada 83 mode
   --  Set False if operating in Ada 95 mode

   Ada_Final_Suffix : constant String := "final";
   --  GNATBIND
   --  The suffix of the name of the finalization procedure. This variable
   --  may be modified by Gnatbind.Scan_Bind_Arg.

   Ada_Final_Name : String_Ptr := new String'("ada" & Ada_Final_Suffix);
   --  GNATBIND
   --  The name of the procedure that performs the finalization at the end of
   --  execution. This variable may be modified by Gnatbind.Scan_Bind_Arg.

   Ada_Init_Suffix : constant String := "init";
   --  GNATBIND
   --  The suffix of the name of the initialization procedure. This variable
   --  may be modified by Gnatbind.Scan_Bind_Arg.

   Ada_Init_Name : String_Ptr := new String'("ada" & Ada_Init_Suffix);
   --  GNATBIND
   --  The name of the procedure that performs initialization at the start
   --  of execution. This variable may be modified by Gnatbind.Scan_Bind_Arg.

   Ada_Main_Name_Suffix : constant String := "main";
   --  GNATBIND
   --  The suffix for Ada_Main_Name. Defined as a constant here so that it
   --  can be referenced in a uniform manner to create either the default
   --  value of Ada_Main_Name (declared below), or the non-default name
   --  set by Gnatbind.Scan_Bind_Arg.

   Ada_Main_Name : String_Ptr := new String'("ada_" & Ada_Main_Name_Suffix);
   --  GNATBIND
   --  The name of the Ada package generated by the binder (when in Ada mode).
   --  This variable may be modified by Gnatbind.Scan_Bind_Arg.

   Address_Clause_Overlay_Warnings : Boolean := True;
   --  GNAT
   --  Set False to disable address clause warnings

   All_Errors_Mode : Boolean := False;
   --  GNAT
   --  Flag set to force display of multiple errors on a single line and
   --  also repeated error messages for references to undefined identifiers
   --  and certain other repeated error messages.

   All_Sources : Boolean := False;
   --  GNATBIND
   --  Set to True to require all source files to be present. This flag is
   --  directly modified by gnatmake to affect the shared binder routines.

   Alternate_Main_Name : String_Ptr := null;
   --  Set to non null when Bind_Alternate_Main_Name is True. This value
   --  is modified as needed by Gnatbind.Scan_Bind_Arg.

   Assertions_Enabled : Boolean := False;
   --  GNAT
   --  Enable assertions made using pragma Assert.

   Back_Annotate_Rep_Info : Boolean := False;
   --  GNAT
   --  If set True (by use of -gnatB), enables back annotation of
   --  representation information by gigi, even in -gnatc mode.

   Bind_Alternate_Main_Name : Boolean := False;
   --  GNATBIND
   --  Set to True if main should be called Alternate_Main_Name.all. This
   --  variable may be set to True by Gnatbind.Scan_Bind_Arg.

   Bind_Main_Program : Boolean := True;
   --  GNATBIND
   --  Set to False if not binding main Ada program.

   Bind_For_Library : Boolean := False;
   --  GNATBIND
   --  Set to True if the binder needs to generate a file designed for
   --  building a library. May be set to True by Gnatbind.Scan_Bind_Arg.

   Bind_Only : Boolean := False;
   --  GNATMAKE
   --  Set to True to skip compile and link steps
   --  (except when Compile_Only and/or Link_Only are True).

   Brief_Output : Boolean := False;
   --  GNAT, GNATBIND
   --  Force brief error messages to standard error, even if verbose mode is
   --  set (so that main error messages go to standard output).

   Check_Object_Consistency : Boolean := False;
   --  GNATBIND, GNATMAKE
   --  Set to True to check whether every object file is consistent with
   --  with its corresponding ada library information (ali) file. An object
   --  file is inconsistent with the corresponding ali file if the object
   --  file does not exist or if it has an older time stamp than the ali file.
   --  Default above is for GNATBIND. GNATMAKE overrides this default to
   --  True (see Make.Initialize) since we do not need to check source
   --  consistencies in gnatmake in this sense.

   Check_Only : Boolean := False;
   --  GNATBIND
   --  Set to True to do checks only, no output of binder file.

   Check_Readonly_Files : Boolean := False;
   --  GNATMAKE
   --  Set to True to check readonly files during the make process.

   Check_Source_Files : Boolean := True;
   --  GNATBIND
   --  Set to True to enable consistency checking for any source files that
   --  are present (i.e. date must match the date in the library info file).
   --  Set to False for object file consistency check only. This flag is
   --  directly modified by gnatmake, to affect the shared binder routines.

   Check_Switches : Boolean := False;
   --  GNATMAKE
   --  Set to True to check compiler options during the make process.

   Check_Unreferenced : Boolean := False;
   --  GNAT
   --  Set to True to enable checking for unreferenced variables

   Check_Withs : Boolean := False;
   --  GNAT
   --  Set to True to enable checking for unused withs, and also the case
   --  of withing a package and using none of the entities in the package.

   Compile_Only : Boolean := False;
   --  GNATMAKE
   --  Set to True to skip bind and link steps (except when Bind_Only is True)

   Compress_Debug_Names : Boolean := False;
   --  GNATMAKE
   --  Set to True if the option to compress debug information is set (-gnatC)

   Config_File : Boolean := True;
   --  GNAT
   --  Set to False to inhibit reading and processing of gnat.adc file

   Config_File_Name : String_Ptr := null;
   --  GNAT
   --  File name of configuration pragmas file (given by switch -gnatec)

   Constant_Condition_Warnings : Boolean := False;
   --  GNAT
   --  Set to True to activate warnings on constant conditions

   subtype Debug_Level_Value is Nat range 0 .. 3;
   Debugger_Level : Debug_Level_Value := 0;
   --  GNATBIND
   --  The value given to the -g parameter.
   --  The default value for -g with no value is 2
   --  This is usually ignored by GNATBIND, except in the VMS version
   --  where it is passed as an argument to __gnat_initialize to trigger
   --  the activation of the remote debugging interface (is this true???).

   Debug_Generated_Code : Boolean := False;
   --  GNAT
   --  Set True (-gnatD switch) to debug generated expanded code instead
   --  of the original source code. Causes debugging information to be
   --  written with respect to the generated code file that is written.

   Display_Compilation_Progress : Boolean := False;
   --  GNATMAKE
   --  Set True (-d switch) to display information on progress while compiling
   --  files. Internal switch to be used in conjunction with an IDE such as
   --  Glide.

   type Distribution_Stub_Mode_Type is
   --  GNAT
     (No_Stubs,
      --  Normal mode, no generation/compilation of distribution stubs

      Generate_Receiver_Stub_Body,
      --  The unit being compiled is the RCI body, and the compiler will
      --  generate the body for the receiver stubs and compile it.

      Generate_Caller_Stub_Body);
      --  The unit being compiled is the RCI spec, and the compiler will
      --  generate the body for the caller stubs and compile it.

   Distribution_Stub_Mode : Distribution_Stub_Mode_Type := No_Stubs;
   --  GNAT
   --  This enumeration variable indicates the five states of distribution
   --  annex stub generation/compilation.

   Do_Not_Execute : Boolean := False;
   --  GNATMAKE
   --  Set to True if no actual compilations should be undertaken.

   Dynamic_Elaboration_Checks : Boolean := False;
   --  GNAT
   --  Set True for dynamic elaboration checking mode, as set by the -gnatE
   --  switch or by the use of pragma Elaboration_Checks (Dynamic).

   Elab_Dependency_Output : Boolean := False;
   --  GNATBIND
   --  Set to True to output complete list of elaboration constraints

   Elab_Order_Output : Boolean := False;
   --  GNATBIND
   --  Set to True to output chosen elaboration order

   Elab_Warnings : Boolean := False;
   --  GNAT
   --  Set to True to generate full elaboration warnings (-gnatwl)

   type Exception_Mechanism_Type is (Setjmp_Longjmp, Front_End_ZCX, GCC_ZCX);
   Exception_Mechanism : Exception_Mechanism_Type := Setjmp_Longjmp;
   --  GNAT
   --  Set to the appropriate value depending on the default as given in
   --  system.ads (ZCX_By_Default, GCC_ZCX_Support, Front_End_ZCX_Support)
   --  and the use of -gnatL -gnatZ (and -gnatdX)

   Exception_Tracebacks : Boolean := False;
   --  GNATBIND
   --  Set to True to store tracebacks in exception occurrences (-E)

   Extensions_Allowed : Boolean := False;
   --  GNAT

   type External_Casing_Type is (
     As_Is,       -- External names cased as they appear in the Ada source
     Uppercase,   -- External names forced to all uppercase letters
     Lowercase);  -- External names forced to all lowercase letters

   External_Name_Imp_Casing : External_Casing_Type := Lowercase;
   --  The setting of this switch determines the casing of external names
   --  when the name is implicitly derived from an entity name (i.e. either
   --  no explicit External_Name or Link_Name argument is used, or, in the
   --  case of extended DEC pragmas, the external name is given using an
   --  identifier. The As_Is setting is not permitted here (since this would
   --  create Ada source programs that were case sensitive).

   External_Name_Exp_Casing : External_Casing_Type := As_Is;
   --  The setting of this switch determines the casing of an external name
   --  specified explicitly with a string literal. As_Is means the string
   --  literal is used as given with no modification to the casing. If
   --  Lowercase or Uppercase is set, then the string is forced to all
   --  lowercase or all uppercase letters as appropriate. Note that this
   --  setting has no effect if the external name is given using an identifier
   --  in the case of extended DEC import/export pragmas (in this case the
   --  casing is controlled by External_Name_Imp_Casing), and also has no
   --  effect if an explicit Link_Name is supplied (a link name is always
   --  used exactly as given).

   Float_Format : Character := ' ';
   --  GNAT
   --  A non-blank value indicates that a Float_Format pragma has been
   --  processed, in which case this variable is set to 'I' for IEEE or
   --  to 'V' for VAX. The setting of 'V' is only possible on OpenVMS
   --  versions of GNAT.

   Float_Format_Long : Character := ' ';
   --  GNAT
   --  A non-blank value indicates that a Long_Float pragma has been
   --  processed (this pragma is recognized only in OpenVMS versions
   --  of GNAT), in which case this variable is set to D or G for
   --  D_Float or G_Float.

   Force_ALI_Tree_File : Boolean := False;
   --  GNAT
   --  Force generation of ali file even if errors are encountered.
   --  Also forces generation of tree file if -gnatt is also set.

   Force_Compilations : Boolean := False;
   --  GNATMAKE
   --  Set to force recompilations even when the objects are up-to-date.

   Force_RM_Elaboration_Order : Boolean := False;
   --  GNATBIND
   --  True if binding with forced RM elaboration order (-f switch set)
   --  Note: this is considered an obsolescent option, to be removed in
   --  some future release. it is no longer documented. The proper way
   --  to get this effect is to use -gnatE and suppress elab checks.

   Full_List : Boolean := False;
   --  GNAT
   --  Set True to generate full source listing with embedded errors

   Global_Discard_Names : Boolean := False;
   --  GNAT
   --  Set true if a pragma Discard_Names applies to the current unit

   GNAT_Mode : Boolean := False;
   --  GNAT
   --  True if compiling in GNAT system mode (-g switch set)

   HLO_Active : Boolean := False;
   --  GNAT
   --  True if High Level Optimizer is activated

   Implementation_Unit_Warnings : Boolean := True;
   --  GNAT
   --  Set True to active warnings for use of implementation internal units.
   --  Can be controlled by use of -gnatwi/-gnatwI.

   Identifier_Character_Set : Character;
   --  GNAT
   --  This variable indicates the character set to be used for identifiers.
   --  The possible settings are:
   --    '1'  Latin-1
   --    '2'  Latin-2
   --    '3'  Latin-3
   --    '4'  Latin-4
   --    'p'  PC (US, IBM page 437)
   --    '8'  PC (European, IBM page 850)
   --    'f'  Full upper set (all distinct)
   --    'n'  No upper characters (Ada/83 rules)
   --    'w'  Latin-1 plus wide characters allowed in identifiers
   --
   --  The setting affects the set of letters allowed in identifiers and the
   --  upper/lower case equivalences. It does not affect the interpretation of
   --  character and string literals, which are always stored using the actual
   --  coding in the source program. This variable is initialized to the
   --  default value appropriate to the system (in Osint.Initialize), and then
   --  reset if a command line switch is used to change the setting.

   Ineffective_Inline_Warnings : Boolean := False;
   --  GNAT
   --  Set True to activate warnings if front-end inlining (-gnatN) is not
   --  able to actually inline a particular call (or all calls). Can be
   --  controlled by use of -gnatwp/-gnatwP.

   Init_Or_Norm_Scalars : Boolean := False;
   --  GNAT
   --  Set True if a pragma Initialize_Scalars applies to the current unit.
   --  Also set True if a pragma Normalize_Scalars applies.

   Initialize_Scalars : Boolean := False;
   --  GNAT
   --  Set True if a pragma Initialize_Scalars applies to the current unit.
   --  Note that Init_Or_Norm_Scalars is also set to True if this is True.

   Initialize_Scalars_Mode : Character := 'I';
   --  GNATBIND
   --  Set to 'I' for -Sin (default), 'L' for -Slo, 'H' for -Shi, 'X' for -Sxx

   Initialize_Scalars_Val : String (1 .. 2);
   --  GNATBIND
   --  Valid only if Initialize_Scalars_Mode is set to 'X' (-Shh). Contains
   --  the two hex bytes from the -Shh switch.

   Inline_Active : Boolean := False;
   --  GNAT
   --  Set True to activate pragma Inline processing across modules. Default
   --  for now is not to inline across module boundaries.

   Front_End_Inlining : Boolean := False;
   --  GNAT
   --  Set True to activate inlining by front-end expansion.

   Inline_Processing_Required : Boolean := False;
   --  GNAT
   --  Set True if inline processing is required. Inline processing is
   --  required if an active Inline pragma is processed. The flag is set
   --  for a pragma Inline or Inline_Always that is actually active.

   In_Place_Mode : Boolean := False;
   --  GNATMAKE
   --  Set True to store ALI and object files in place ie in the object
   --  directory if these files already exist or in the source directory
   --  if not.

   Keep_Going : Boolean := False;
   --  GNATMAKE
   --  When True signals gnatmake to ignore compilation errors and keep
   --  processing sources until there is no more work.

   Link_Only : Boolean := False;
   --  GNATMAKE
   --  Set to True to skip compile and bind steps
   --  (except when Bind_Only is set to True).

   List_Units : Boolean := False;
   --  GNAT
   --  List units in the active library

   List_Dependencies : Boolean := False;
   --  GNATMAKE
   --  When True gnatmake verifies that the objects are up to date and
   --  outputs the list of object dependencies. This list can be used
   --  directly in a Makefile.

   List_Representation_Info : Int range 0 .. 3 := 0;
   --  GNAT
   --  Set true by -gnatR switch to list representation information.
   --  The settings are as follows:
   --
   --    0 = no listing of representation information (default as above)
   --    1 = list rep info for user defined record and array types
   --    2 = list rep info for all user defined types and objects
   --    3 = like 2, but variable fields are decoded symbolically

   Locking_Policy : Character := ' ';
   --  GNAT
   --  Set to ' ' for the default case (no locking policy specified).
   --  Reset to first character (uppercase) of locking policy name if a
   --  valid pragma Locking_Policy is encountered.

   Look_In_Primary_Dir : Boolean := True;
   --  GNAT, GNATBIND, GNATMAKE
   --  Set to False if a -I- was present on the command line.
   --  When True we are allowed to look in the primary directory to locate
   --  other source or library files.

   Mapping_File_Name : String_Ptr := null;
   --  GNAT
   --  File name of mapping between unit names, file names and path names.
   --  (given by switch -gnatem)

   Maximum_Errors : Int := 9999;
   --  GNAT, GNATBIND
   --  Maximum number of errors before compilation is terminated

   Maximum_File_Name_Length : Int;
   --  GNAT, GNATBIND
   --  Maximum number of characters allowed in a file name, not counting the
   --  extension, as set by the appropriate switch. If no switch is given,
   --  then this value is initialized by Osint to the appropriate value.

   Maximum_Processes : Positive := 1;
   --  GNATMAKE
   --  Maximum number of processes that should be spawned to carry out
   --  compilations.

   Minimal_Recompilation : Boolean := False;
   --  GNATMAKE
   --  Set to True if minimal recompilation mode requested.

   No_Stdlib : Boolean := False;
   --  GNATMAKE
   --  Set to True if no default library search dirs added to search list.

   No_Stdinc : Boolean := False;
   --  GNATMAKE
   --  Set to True if no default source search dirs added to search list.

   No_Main_Subprogram : Boolean := False;
   --  GNATMAKE, GNATBIND
   --  Set to True if compilation/binding of a program without main
   --  subprogram requested.

   Normalize_Scalars : Boolean := False;
   --  GNAT
   --  Set True if a pragma Normalize_Scalars applies to the current unit.
   --  Note that Init_Or_Norm_Scalars is also set to True if this is True.

   No_Run_Time : Boolean := False;
   --  GNAT
   --  Set True if a valid pragma No_Run_Time is processed or if the
   --  flag Targparm.High_Integrity_Mode_On_Target is set True.

   type Operating_Mode_Type is (Check_Syntax, Check_Semantics, Generate_Code);
   Operating_Mode : Operating_Mode_Type := Generate_Code;
   --  GNAT
   --  Indicates the operating mode of the compiler. The default is generate
   --  code, which runs the parser, semantics and backend. Switches can be
   --  used to set syntax checking only mode, or syntax and semantics checking
   --  only mode. Operating_Mode can also be modified as a result of detecting
   --  errors during the compilation process. In particular if any error is
   --  detected then this flag is reset from Generate_Code to Check_Semantics
   --  after generating an error message.

   Output_File_Name_Present : Boolean := False;
   --  GNATBIND, GNAT
   --  Set to True when the output C file name is given with option -o
   --  for GNATBIND or when the object file name is given with option
   --  -gnatO for GNAT.

   Output_Linker_Option_List : Boolean := False;
   --  GNATBIND
   --  True if output of list of linker options is requested (-K switch set)

   Output_Object_List : Boolean := False;
   --  GNATBIND
   --  True if output of list of objects is requested (-O switch set)

   Pessimistic_Elab_Order : Boolean := False;
   --  GNATBIND
   --  True if pessimistic elaboration order is to be chosen (-p switch set)

   Polling_Required : Boolean := False;
   --  GNAT
   --  Set to True if polling for asynchronous abort is enabled by using
   --  the -gnatP option for GNAT.

   Print_Generated_Code : Boolean := False;
   --  GNAT
   --  Set to True to enable output of generated code in source form. This
   --  flag is set by the -gnatG switch.

   Propagate_Exceptions : Boolean := False;
   --  GNAT
   --  Indicates if subprogram descriptor exception tables should be
   --  built for imported subprograms. Set True if a Propagate_Exceptions
   --  pragma applies to the extended main unit.

   Queuing_Policy : Character := ' ';
   --  GNAT
   --  Set to ' ' for the default case (no queuing policy specified). Reset to
   --  Reset to first character (uppercase) of locking policy name if a valid
   --  Queuing_Policy pragma is encountered.

   Quiet_Output : Boolean := False;
   --  GNATMAKE
   --  Set to True if the list of compilation commands should not be output.

   Shared_Libgnat : Boolean;
   --  GNATBIND
   --  Set to True if a shared libgnat is requested by using the -shared
   --  option for GNATBIND and to False when using the -static option. The
   --  value of this switch is set by Gnatbind.Scan_Bind_Arg.

   Software_Overflow_Checking : Boolean;
   --  GNAT
   --  Set to True by Osint.Initialize if the target requires the software
   --  approach to integer arithmetic overflow checking (i.e. the use of
   --  double length arithmetic followed by a range check). Set to False
   --  if the target implements hardware overflow checking.

   Stack_Checking_Enabled : Boolean;
   --  GNAT
   --  Set to indicate if -fstack-check switch is set for the compilation.
   --  True means that the switch is set, so that stack checking is enabled.
   --  False means that the switch is not set (no stack checking). This
   --  value is obtained from the external imported value flag_stack_check
   --  in the gcc backend (see Frontend) and may be referenced throughout
   --  the compilation phases.

   Strict_Math : aliased Boolean := False;
   --  GNAT
   --  This switch is set True if the current unit is to be compiled in
   --  strict math mode. The effect is to cause certain library file name
   --  substitutions to implement strict math semantics. See the routine
   --  Adjust_File_Name_For_Configuration, and also the configuration
   --  in the body of Opt.
   --
   --  Note: currently this switch is always False. Eventually it will be
   --  settable by a switch and a configuration pragma.

   Style_Check : Boolean := False;
   --  GNAT
   --  Set True to perform style checks. Activates checks carried out
   --  in package Style (see body of this package for details of checks)
   --  This flag is set True by either the -gnatg or -gnaty switches.

   System_Extend_Pragma_Arg : Node_Id := Empty;
   --  GNAT
   --  Set non-empty if and only if a correct Extend_System pragma was present
   --  in which case it points to the argument of the pragma, and the name can
   --  be located as Chars (Expression (System_Extend_Pragma_Arg)).

   Subunits_Missing : Boolean := False;
   --  This flag is set true if missing subunits are detected with code
   --  generation active. This causes code generation to be skipped.

   Suppress_Options : Suppress_Record;
   --  GNAT
   --  Flags set True to suppress corresponding check, i.e. add an implicit
   --  pragma Suppress at the outer level of each unit compiled. Note that
   --  these suppress actions can be overridden by the use of the Unsuppress
   --  pragma. This variable is initialized by Osint.Initialize.

   Table_Factor : Int := 1;
   --  Factor by which all initial table sizes set in Alloc are multiplied.
   --  Used in Table to calculate initial table sizes (the initial table
   --  size is the value in Alloc, used as the Table_Initial parameter
   --  value, multiplied by the factor given here. The default value is
   --  used if no -gnatT switch appears.

   Task_Dispatching_Policy : Character := ' ';
   --  GNAT
   --  Set to ' ' for the default case (no task dispatching policy specified).
   --  Reset to first character (uppercase) of task dispatching policy name
   --  if a valid Task_Dispatching_Policy pragma is encountered.

   Tasking_Used : Boolean := False;
   --  Set True if any tasking construct is encountered. Used to activate the
   --  output of the Q, L and T lines in ali files.

   Time_Slice_Set : Boolean := False;
   --  Set True if a pragma Time_Slice is processed in the main unit, or
   --  if the T switch is present to set a time slice value.

   Time_Slice_Value : Nat;
   --  Time slice value. Valid only if Time_Slice_Set is True, i.e. if a
   --  Time_Slice pragma has been processed. Set to the time slice value
   --  in microseconds. Negative values are stored as zero, and the value
   --  is not larger than 1_000_000_000 (1000 seconds). Values larger than
   --  this are reset to this maximum.

   Tolerate_Consistency_Errors : Boolean := False;
   --  GNATBIND
   --  Tolerate time stamp and other consistency errors. If this switch is
   --  set true, then inconsistencies result in warnings rather than errors.

   Tree_Output : Boolean := False;
   --  GNAT
   --  Set True to generate output tree file

   Try_Semantics : Boolean := False;
   --  GNAT
   --  Flag set to force attempt at semantic analysis, even if parser errors
   --  occur. This will probably cause blowups at this stage in the game. On
   --  the other hand, most such blowups will be caught cleanly and simply
   --  say compilation abandoned.

   Unique_Error_Tag : Boolean := Tag_Errors;
   --  GNAT
   --  Indicates if error messages are to be prefixed by the string error:
   --  Initialized from Tag_Errors, can be forced on with the -gnatU switch.

   Unreserve_All_Interrupts : Boolean := False;
   --  GNAT, GNATBIND
   --  Normally set False, set True if a valid Unreserve_All_Interrupts
   --  pragma appears anywhere in the main unit for GNAT, or if any ALI
   --  file has the corresponding attribute set in GNATBIND.

   Upper_Half_Encoding : Boolean := False;
   --  GNAT
   --  Normally set False, indicating that upper half ASCII characters are
   --  used in the normal way to represent themselves. If the wide character
   --  encoding method uses the upper bit for this encoding, then this flag
   --  is set True, and upper half characters in the source indicate the
   --  start of a wide character sequence.

   Usage_Requested : Boolean := False;
   --  GNAT, GNATBIND, GNATMAKE
   --  Set to True if h switch encountered requesting usage information

   Use_VADS_Size : Boolean := False;
   --  GNAT
   --  Set to True if a valid pragma Use_VADS_Size is processed

   Validity_Checks_On  : Boolean := True;
   --  This flag determines if validity checking is on or off. The initial
   --  state is on, and the required default validity checks are active. The
   --  actual set of checks that is performed if Validity_Checks_On is set
   --  is defined by the switches in package Sem_Val. The Validity_Checks_On
   --  switch is controlled by pragma Validity_Checks (On | Off), and also
   --  some generated compiler code (typically code that has to do with
   --  validity check generation) is compiled with this switch set to False.

   Verbose_Mode : Boolean := False;
   --  GNAT, GNATBIND
   --  Set to True to get verbose mode (full error message text and location
   --  information sent to standard output, also header, copyright and summary)

   Warn_On_Biased_Rounding : Boolean := False;
   --  GNAT
   --  Set to True to generate warnings for static constants that are rounded
   --  in a manner inconsistent with unbiased rounding (round to even). Can
   --  be modified by use of -gnatwb/B.

   Warn_On_Hiding : Boolean := False;
   --  GNAT
   --  Set to True to generate warnings if a declared entity hides another
   --  entity. The default is that this warning is suppressed.

   Warn_On_Redundant_Constructs : Boolean := False;
   --  GNAT
   --  Set to True to generate warnings for redundant constructs (e.g. useless
   --  assignments/conversions). The default is that this warning is disabled.

   type Warning_Mode_Type is (Suppress, Normal, Treat_As_Error);
   Warning_Mode : Warning_Mode_Type := Normal;
   --  GNAT, GNATBIND
   --  Controls treatment of warning messages. If set to Suppress, warning
   --  messages are not generated at all. In Normal mode, they are generated
   --  but do not count as errors. In Treat_As_Error mode, warning messages
   --  are generated and are treated as errors.

   Wide_Character_Encoding_Method : WC_Encoding_Method := WCEM_Brackets;
   --  GNAT
   --  Method used for encoding wide characters in the source program. See
   --  description of type in unit System.WCh_Con for a list of the methods
   --  that are currently supported. Note that brackets notation is always
   --  recognized in source programs regardless of the setting of this
   --  variable. The default setting causes only the brackets notation
   --  to be recognized. If this is the main unit, this setting also
   --  controls the output of the W=? parameter in the ali file, which
   --  is used to provide the default for Wide_Text_IO files.

   Xref_Active : Boolean := True;
   --  GNAT
   --  Set if cross-referencing is enabled (i.e. xref info in ali files)

   Zero_Cost_Exceptions_Val : Boolean;
   Zero_Cost_Exceptions_Set : Boolean := False;
   --  GNAT
   --  These values are to record the setting of the zero cost exception
   --  handling mode set by argument switches (-gnatZ/-gnatL). If the
   --  value is set by one of these switches, then Zero_Cost_Exceptions_Set
   --  is set to True, and Zero_Cost_Exceptions_Val indicates the setting.
   --  This value is used to reset ZCX_By_Default_On_Target.

   ----------------------------
   -- Configuration Settings --
   ----------------------------

   --  These are settings that are used to establish the mode at the start
   --  of each unit. The values defined below can be affected either by
   --  command line switches, or by the use of appropriate configuration
   --  pragmas in the gnat.adc file.

   Ada_83_Config : Boolean;
   --  GNAT
   --  This is the value of the configuration switch for Ada 83 mode, as set
   --  by the command line switch -gnat83, and possibly modified by the use
   --  of configuration pragmas Ada_95 and Ada_83 in the gnat.adc file. This
   --  switch is used to set the initial value for Ada_83 mode at the start
   --  of analysis of a unit. Note however, that the setting of this switch
   --  is ignored for internal and predefined units (which are always compiled
   --  in Ada 95 mode).

   Dynamic_Elaboration_Checks_Config : Boolean := False;
   --  GNAT
   --  Set True for dynamic elaboration checking mode, as set by the -gnatE
   --  switch or by the use of pragma Elaboration_Checking (Dynamic).

   Extensions_Allowed_Config : Boolean;
   --  GNAT
   --  This is the switch that indicates whether extensions are allowed.
   --  It can be set True either by use of the -gnatX switch, or by use
   --  of the configuration pragma Extensions_Allowed (On). It is always
   --  set to True for internal GNAT units, since extensions are always
   --  permitted in such units.

   External_Name_Exp_Casing_Config : External_Casing_Type;
   --  GNAT
   --  This is the value of the configuration switch that controls casing
   --  of external symbols for which an explicit external name is given. It
   --  can be set to Uppercase by the command line switch -gnatF, and further
   --  modified by the use of the configuration pragma External_Name_Casing
   --  in the gnat.adc file. This switch is used to set the initial value
   --  for External_Name_Exp_Casing at the start of analyzing each unit.
   --  Note however that the setting of this switch is ignored for internal
   --  and predefined units (which are always compiled with As_Is mode).

   External_Name_Imp_Casing_Config : External_Casing_Type;
   --  GNAT
   --  This is the value of the configuration switch that controls casing
   --  of external symbols where the external name is implicitly given. It
   --  can be set to Uppercase by the command line switch -gnatF, and further
   --  modified by the use of the configuration pragma External_Name_Casing
   --  in the gnat.adc file. This switch is used to set the initial value
   --  for External_Name_Imp_Casing at the start of analyzing each unit.
   --  Note however that the setting of this switch is ignored for internal
   --  and predefined units (which are always compiled with Lowercase mode).

   Polling_Required_Config : Boolean;
   --  GNAT
   --  This is the value of the configuration switch that controls polling
   --  mode. It can be set True by the command line switch -gnatP, and then
   --  further modified by the use of pragma Polling in the gnat.adc file.
   --  This switch is used to set the initial value for Polling_Required
   --  at the start of analyzing each unit.

   Use_VADS_Size_Config : Boolean;
   --  GNAT
   --  This is the value of the configuration switch that controls the use
   --  of VADS_Size instead of Size whereever the attribute Size is used.
   --  It can be set True by the use of the pragma Use_VADS_Size in the
   --  gnat.adc file. This switch is used to set the initial value for
   --  Use_VADS_Size at the start of analyzing each unit. Note however that
   --  the setting of this switch is ignored for internal and predefined
   --  units (which are always compiled with the standard Size semantics).

   type Config_Switches_Type is private;
   --  Type used to save values of the switches set from Config values

   procedure Save_Opt_Config_Switches (Save : out Config_Switches_Type);
   --  This procedure saves the current values of the switches which are
   --  initialized from the above Config values, and then resets these
   --  switches according to the Config value settings.

   procedure Set_Opt_Config_Switches (Internal_Unit : Boolean);
   --  This procedure sets the switches to the appropriate initial values.
   --  The parameter Internal_Unit is True for an internal or predefined
   --  unit, and affects the way the switches are set (see above).

   procedure Restore_Opt_Config_Switches (Save : Config_Switches_Type);
   --  This procedure restores a set of switch values previously saved
   --  by a call to Save_Opt_Switches.

   procedure Register_Opt_Config_Switches;
   --  This procedure is called after processing the gnat.adc file to record
   --  the values of the Config switches, as possibly modified by the use
   --  of command line switches and configuration pragmas.

   ------------------------
   -- Other Global Flags --
   ------------------------

   Expander_Active : Boolean := False;
   --  A flag that indicates if expansion is active (True) or deactivated
   --  (False). When expansion is deactivated all calls to expander routines
   --  have no effect. Note that the initial setting of False is merely to
   --  prevent saving of an undefined value for an initial call to the
   --  Expander_Mode_Save_And_Set procedure. For more information on the
   --  use of this flag, see package Expander. Indeed this flag might more
   --  logically be in the spec of Expander, but it is referenced by Errout,
   --  and it really seems wrong for Errout to depend on Expander.

   -----------------------
   -- Tree I/O Routines --
   -----------------------

   procedure Tree_Read;
   --  Reads switch settings from current tree file using Tree_Read

   procedure Tree_Write;
   --  Writes out switch settings to current tree file using Tree_Write

private

   type Config_Switches_Type is record
      Ada_83                     : Boolean;
      Dynamic_Elaboration_Checks : Boolean;
      Extensions_Allowed         : Boolean;
      External_Name_Exp_Casing   : External_Casing_Type;
      External_Name_Imp_Casing   : External_Casing_Type;
      Polling_Required           : Boolean;
      Use_VADS_Size              : Boolean;
   end record;

end Opt;
