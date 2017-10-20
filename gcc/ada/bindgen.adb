------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D G E N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

with ALI;      use ALI;
with Casing;   use Casing;
with Fname;    use Fname;
with Gnatvsn;  use Gnatvsn;
with Hostparm;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.B;  use Osint.B;
with Output;   use Output;
with Rident;   use Rident;
with Stringt;  use Stringt;
with Table;
with Targparm; use Targparm;
with Types;    use Types;

with System.OS_Lib;
with System.WCh_Con; use System.WCh_Con;

with GNAT.Heap_Sort_A; use GNAT.Heap_Sort_A;
with GNAT.HTable;

package body Bindgen is
   Statement_Buffer : String (1 .. 1000);
   --  Buffer used for constructing output statements

   Stm_Last : Natural := 0;
   --  Stm_Last location in Statement_Buffer currently set

   With_GNARL : Boolean := False;
   --  Flag which indicates whether the program uses the GNARL library
   --  (presence of the unit System.OS_Interface)

   Num_Elab_Calls : Nat := 0;
   --  Number of generated calls to elaboration routines

   Num_Primary_Stacks : Int := 0;
   --  Number of default-sized primary stacks the binder needs to allocate for
   --  task objects declared in the program.

   Num_Sec_Stacks : Int := 0;
   --  Number of default-sized primary stacks the binder needs to allocate for
   --  task objects declared in the program.

   System_Restrictions_Used : Boolean := False;
   --  Flag indicating whether the unit System.Restrictions is in the closure
   --  of the partition. This is set by Resolve_Binder_Options, and is used
   --  to determine whether or not to initialize the restrictions information
   --  in the body of the binder generated file (we do not want to do this
   --  unconditionally, since it drags in the System.Restrictions unit
   --  unconditionally, which is unpleasand, especially for ZFP etc.)

   Dispatching_Domains_Used : Boolean := False;
   --  Flag indicating whether multiprocessor dispatching domains are used in
   --  the closure of the partition. This is set by Resolve_Binder_Options, and
   --  is used to call the routine to disallow the creation of new dispatching
   --  domains just before calling the main procedure from the environment
   --  task.

   System_Secondary_Stack_Used : Boolean := False;
   --  Flag indicating whether the unit System.Secondary_Stack is in the
   --  closure of the partition. This is set by Resolve_Binder_Options, and
   --  is used to initialize the package in cases where the run-time brings
   --  in package but the secondary stack is not used.

   System_Tasking_Restricted_Stages_Used : Boolean := False;
   --  Flag indicating whether the unit System.Tasking.Restricted.Stages is in
   --  the closure of the partition. This is set by Resolve_Binder_Options,
   --  and it used to call a routine to active all the tasks at the end of
   --  the elaboration when partition elaboration policy is sequential.

   System_Interrupts_Used : Boolean := False;
   --  Flag indicating whether the unit System.Interrups is in the closure of
   --  the partition. This is set by Resolve_Binder_Options, and it used to
   --  attach interrupt handlers at the end of the elaboration when partition
   --  elaboration policy is sequential.

   System_BB_CPU_Primitives_Multiprocessors_Used : Boolean := False;
   --  Flag indicating whether unit System.BB.CPU_Primitives.Multiprocessors
   --  is in the closure of the partition. This is set by procedure
   --  Resolve_Binder_Options, and it is used to call a procedure that starts
   --  slave processors.

   System_Version_Control_Used : Boolean := False;
   --  Flag indicating whether unit System.Version_Control is in the closure.
   --  This unit is implicitly withed by the compiler when Version or
   --  Body_Version attributes are used. If the package is not in the closure,
   --  the version definitions can be removed.

   Lib_Final_Built : Boolean := False;
   --  Flag indicating whether the finalize_library rountine has been built

   Bind_Env_String_Built : Boolean := False;
   --  Flag indicating whether a bind environment string has been built

   CodePeer_Wrapper_Name : constant String := "call_main_subprogram";
   --  For CodePeer, introduce a wrapper subprogram which calls the
   --  user-defined main subprogram.

   ----------------------------------
   -- Interface_State Pragma Table --
   ----------------------------------

   --  This table assembles the interface state pragma information from
   --  all the units in the partition. Note that Bcheck has already checked
   --  that the information is consistent across units. The entries
   --  in this table are n/u/r/s for not set/user/runtime/system.

   package IS_Pragma_Settings is new Table.Table
     (Table_Component_Type => Character,
      Table_Index_Type     => Int,
      Table_Low_Bound      => 0,
      Table_Initial        => 100,
      Table_Increment      => 200,
      Table_Name           => "IS_Pragma_Settings");

   --  This table assembles the Priority_Specific_Dispatching pragma
   --  information from all the units in the partition. Note that Bcheck has
   --  already checked that the information is consistent across units.
   --  The entries in this table are the upper case first character of the
   --  policy name, e.g. 'F' for FIFO_Within_Priorities.

   package PSD_Pragma_Settings is new Table.Table
     (Table_Component_Type => Character,
      Table_Index_Type     => Int,
      Table_Low_Bound      => 0,
      Table_Initial        => 100,
      Table_Increment      => 200,
      Table_Name           => "PSD_Pragma_Settings");

   ----------------------------
   -- Bind_Environment Table --
   ----------------------------

   subtype Header_Num is Int range 0 .. 36;

   function Hash (Nam : Name_Id) return Header_Num;

   package Bind_Environment is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Name_Id,
      No_Element => No_Name,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");

   ----------------------
   -- Run-Time Globals --
   ----------------------

   --  This section documents the global variables that are set from the
   --  generated binder file.

   --     Main_Priority                 : Integer;
   --     Time_Slice_Value              : Integer;
   --     Heap_Size                     : Natural;
   --     WC_Encoding                   : Character;
   --     Locking_Policy                : Character;
   --     Queuing_Policy                : Character;
   --     Task_Dispatching_Policy       : Character;
   --     Priority_Specific_Dispatching : System.Address;
   --     Num_Specific_Dispatching      : Integer;
   --     Restrictions                  : System.Address;
   --     Interrupt_States              : System.Address;
   --     Num_Interrupt_States          : Integer;
   --     Unreserve_All_Interrupts      : Integer;
   --     Exception_Tracebacks          : Integer;
   --     Exception_Tracebacks_Symbolic : Integer;
   --     Detect_Blocking               : Integer;
   --     Default_Stack_Size            : Integer;
   --     Default_Secondary_Stack_Size  : System.Parameters.Size_Type;
   --     Leap_Seconds_Support          : Integer;
   --     Main_CPU                      : Integer;
   --     Default_Sized_SS_Pool         : System.Address;
   --     Binder_Sec_Stacks_Count       : Natural;

   --  Main_Priority is the priority value set by pragma Priority in the main
   --  program. If no such pragma is present, the value is -1.

   --  Time_Slice_Value is the time slice value set by pragma Time_Slice in the
   --  main program, or by the use of a -Tnnn parameter for the binder (if both
   --  are present, the binder value overrides). The value is in milliseconds.
   --  A value of zero indicates that time slicing should be suppressed. If no
   --  pragma is present, and no -T switch was used, the value is -1.

   --  WC_Encoding shows the wide character encoding method used for the main
   --  program. This is one of the encoding letters defined in
   --  System.WCh_Con.WC_Encoding_Letters.

   --  Locking_Policy is a space if no locking policy was specified for the
   --  partition. If a locking policy was specified, the value is the upper
   --  case first character of the locking policy name, for example, 'C' for
   --  Ceiling_Locking.

   --  Queuing_Policy is a space if no queuing policy was specified for the
   --  partition. If a queuing policy was specified, the value is the upper
   --  case first character of the queuing policy name for example, 'F' for
   --  FIFO_Queuing.

   --  Task_Dispatching_Policy is a space if no task dispatching policy was
   --  specified for the partition. If a task dispatching policy was specified,
   --  the value is the upper case first character of the policy name, e.g. 'F'
   --  for FIFO_Within_Priorities.

   --  Priority_Specific_Dispatching is the address of a string used to store
   --  the task dispatching policy specified for the different priorities in
   --  the partition. The length of this string is determined by the last
   --  priority for which such a pragma applies (the string will be a null
   --  string if no specific dispatching policies were used). If pragma were
   --  present, the entries apply to the priorities in sequence from the first
   --  priority. The value stored is the upper case first character of the
   --  policy name, or 'F' (for FIFO_Within_Priorities) as the default value
   --  for those priority ranges not specified.

   --  Num_Specific_Dispatching is length of the Priority_Specific_Dispatching
   --  string. It will be set to zero if no Priority_Specific_Dispatching
   --  pragmas are present.

   --  Restrictions is the address of a null-terminated string specifying the
   --  restrictions information for the partition. The format is identical to
   --  that of the parameter string found on R lines in ali files (see Lib.Writ
   --  spec in lib-writ.ads for full details). The difference is that in this
   --  context the values are the cumulative ones for the entire partition.

   --  Interrupt_States is the address of a string used to specify the
   --  cumulative results of Interrupt_State pragmas used in the partition.
   --  The length of this string is determined by the last interrupt for which
   --  such a pragma is given (the string will be a null string if no pragmas
   --  were used). If pragma were present the entries apply to the interrupts
   --  in sequence from the first interrupt, and are set to one of four
   --  possible settings: 'n' for not specified, 'u' for user, 'r' for run
   --  time, 's' for system, see description of Interrupt_State pragma for
   --  further details.

   --  Num_Interrupt_States is the length of the Interrupt_States string. It
   --  will be set to zero if no Interrupt_State pragmas are present.

   --  Unreserve_All_Interrupts is set to one if at least one unit in the
   --  partition had a pragma Unreserve_All_Interrupts, and zero otherwise.

   --  Exception_Tracebacks is set to one if the -Ea or -E parameter was
   --  present in the bind and to zero otherwise. Note that on some targets
   --  exception tracebacks are provided by default, so a value of zero for
   --  this parameter does not necessarily mean no trace backs are available.

   --  Exception_Tracebacks_Symbolic is set to one if the -Es parameter was
   --  present in the bind and to zero otherwise.

   --  Detect_Blocking indicates whether pragma Detect_Blocking is active or
   --  not. A value of zero indicates that the pragma is not present, while a
   --  value of 1 signals its presence in the partition.

   --  Default_Stack_Size is the default stack size used when creating an Ada
   --  task with no explicit Storage_Size clause.

   --  Default_Secondary_Stack_Size is the default secondary stack size used
   --  when creating an Ada task with no explicit Secondary_Stack_Size clause.

   --  Leap_Seconds_Support denotes whether leap seconds have been enabled or
   --  disabled. A value of zero indicates that leap seconds are turned "off",
   --  while a value of one signifies "on" status.

   --  Main_CPU is the processor set by pragma CPU in the main program. If no
   --  such pragma is present, the value is -1.

   --  Default_Sized_SS_Pool is set to the address of the default-sized
   --  secondary stacks array generated by the binder. This pool of stacks is
   --  generated when either the restriction No_Implicit_Heap_Allocations
   --  or No_Implicit_Task_Allocations is active.

   --  Binder_Sec_Stacks_Count is the number of generated secondary stacks in
   --  the Default_Sized_SS_Pool.

   procedure WBI (Info : String) renames Osint.B.Write_Binder_Info;
   --  Convenient shorthand used throughout

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Gen_Adainit (Elab_Order : Unit_Id_Array);
   --  Generates the Adainit procedure

   procedure Gen_Adafinal;
   --  Generate the Adafinal procedure

   procedure Gen_Bind_Env_String;
   --  Generate the bind environment buffer

   procedure Gen_CodePeer_Wrapper;
   --  For CodePeer, generate wrapper which calls user-defined main subprogram

   procedure Gen_Elab_Calls (Elab_Order : Unit_Id_Array);
   --  Generate sequence of elaboration calls

   procedure Gen_Elab_Externals (Elab_Order : Unit_Id_Array);
   --  Generate sequence of external declarations for elaboration

   procedure Gen_Elab_Order (Elab_Order : Unit_Id_Array);
   --  Generate comments showing elaboration order chosen

   procedure Gen_Finalize_Library (Elab_Order : Unit_Id_Array);
   --  Generate a sequence of finalization calls to elaborated packages

   procedure Gen_Main;
   --  Generate procedure main

   procedure Gen_Object_Files_Options (Elab_Order : Unit_Id_Array);
   --  Output comments containing a list of the full names of the object
   --  files to be linked and the list of linker options supplied by
   --  Linker_Options pragmas in the source.

   procedure Gen_Output_File_Ada
     (Filename   : String;
      Elab_Order : Unit_Id_Array);
   --  Generate Ada output file

   procedure Gen_Restrictions;
   --  Generate initialization of restrictions variable

   procedure Gen_Versions;
   --  Output series of definitions for unit versions

   function Get_Ada_Main_Name return String;
   --  This function is used for the Ada main output to compute a usable name
   --  for the generated main program. The normal main program name is
   --  Ada_Main, but this won't work if the user has a unit with this name.
   --  This function tries Ada_Main first, and if there is such a clash, then
   --  it tries Ada_Name_01, Ada_Name_02 ... Ada_Name_99 in sequence.

   function Get_Main_Unit_Name (S : String) return String;
   --  Return the main unit name corresponding to S by replacing '.' with '_'

   function Get_Main_Name return String;
   --  This function is used in the main output case to compute the correct
   --  external main program. It is "main" by default, unless the flag
   --  Use_Ada_Main_Program_Name_On_Target is set, in which case it is the name
   --  of the Ada main name without the "_ada". This default can be overridden
   --  explicitly using the -Mname binder switch.

   function Get_WC_Encoding return Character;
   --  Return wide character encoding method to set as WC_Encoding in output.
   --  If -W has been used, returns the specified encoding, otherwise returns
   --  the encoding method used for the main program source. If there is no
   --  main program source (-z switch used), returns brackets ('b').

   function Has_Finalizer (Elab_Order : Unit_Id_Array) return Boolean;
   --  Determine whether the current unit has at least one library-level
   --  finalizer.

   function Lt_Linker_Option (Op1 : Natural; Op2 : Natural) return Boolean;
   --  Compare linker options, when sorting, first according to
   --  Is_Internal_File (internal files come later) and then by
   --  elaboration order position (latest to earliest).

   procedure Move_Linker_Option (From : Natural; To : Natural);
   --  Move routine for sorting linker options

   procedure Resolve_Binder_Options (Elab_Order : Unit_Id_Array);
   --  Set the value of With_GNARL

   procedure Set_Char (C : Character);
   --  Set given character in Statement_Buffer at the Stm_Last + 1 position
   --  and increment Stm_Last by one to reflect the stored character.

   procedure Set_Int (N : Int);
   --  Set given value in decimal in Statement_Buffer with no spaces starting
   --  at the Stm_Last + 1 position, and updating Stm_Last past the value. A
   --  minus sign is output for a negative value.

   procedure Set_Boolean (B : Boolean);
   --  Set given boolean value in Statement_Buffer at the Stm_Last + 1 position
   --  and update Stm_Last past the value.

   procedure Set_IS_Pragma_Table;
   --  Initializes contents of IS_Pragma_Settings table from ALI table

   procedure Set_Main_Program_Name;
   --  Given the main program name in Name_Buffer (length in Name_Len) generate
   --  the name of the routine to be used in the call. The name is generated
   --  starting at Stm_Last + 1, and Stm_Last is updated past it.

   procedure Set_Name_Buffer;
   --  Set the value stored in positions 1 .. Name_Len of the Name_Buffer

   procedure Set_PSD_Pragma_Table;
   --  Initializes contents of PSD_Pragma_Settings table from ALI table

   procedure Set_String (S : String);
   --  Sets characters of given string in Statement_Buffer, starting at the
   --  Stm_Last + 1 position, and updating last past the string value.

   procedure Set_String_Replace (S : String);
   --  Replaces the last S'Length characters in the Statement_Buffer with the
   --  characters of S. The caller must ensure that these characters do in fact
   --  exist in the Statement_Buffer.

   procedure Set_Unit_Name;
   --  Given a unit name in the Name_Buffer, copy it into Statement_Buffer,
   --  starting at the Stm_Last + 1 position and update Stm_Last past the
   --  value. Each dot (.) will be qualified into double underscores (__).

   procedure Set_Unit_Number (U : Unit_Id);
   --  Sets unit number (first unit is 1, leading zeroes output to line up all
   --  output unit numbers nicely as required by the value, and by the total
   --  number of units.

   procedure Write_Statement_Buffer;
   --  Write out contents of statement buffer up to Stm_Last, and reset
   --  Stm_Last to 0.

   procedure Write_Statement_Buffer (S : String);
   --  First writes its argument (using Set_String (S)), then writes out the
   --  contents of statement buffer up to Stm_Last, and resets Stm_Last to 0.

   procedure Write_Bind_Line (S : String);
   --  Write S (an LF-terminated string) to the binder file (for use with
   --  Set_Special_Output).

   ------------------
   -- Gen_Adafinal --
   ------------------

   procedure Gen_Adafinal is
   begin
      WBI ("   procedure " & Ada_Final_Name.all & " is");

      --  Call s_stalib_adafinal to await termination of tasks and so on. We
      --  want to do this if there is a main program, either in Ada or in some
      --  other language. (Note that Bind_Main_Program is True for Ada mains,
      --  but False for mains in other languages.) We do not want to do this if
      --  we're binding a library.

      if not Bind_For_Library and not CodePeer_Mode then
         WBI ("      procedure s_stalib_adafinal;");
         Set_String ("      pragma Import (C, s_stalib_adafinal, ");
         Set_String ("""system__standard_library__adafinal"");");
         Write_Statement_Buffer;
      end if;

      WBI ("");
      WBI ("      procedure Runtime_Finalize;");
      WBI ("      pragma Import (C, Runtime_Finalize, " &
             """__gnat_runtime_finalize"");");
      WBI ("");
      WBI ("   begin");

      if not CodePeer_Mode then
         WBI ("      if not Is_Elaborated then");
         WBI ("         return;");
         WBI ("      end if;");
         WBI ("      Is_Elaborated := False;");
      end if;

      WBI ("      Runtime_Finalize;");

      --  By default (real targets), finalization is done differently depending
      --  on whether this is the main program or a library.

      if not CodePeer_Mode then
         if not Bind_For_Library then
            WBI ("      s_stalib_adafinal;");
         elsif Lib_Final_Built then
            WBI ("      finalize_library;");
         else
            WBI ("      null;");
         end if;

      --  Pragma Import C cannot be used on virtual targets, therefore call the
      --  runtime finalization routine directly in CodePeer mode, where
      --  imported functions are ignored.

      else
         WBI ("      System.Standard_Library.Adafinal;");
      end if;

      WBI ("   end " & Ada_Final_Name.all & ";");
      WBI ("");
   end Gen_Adafinal;

   -----------------
   -- Gen_Adainit --
   -----------------

   procedure Gen_Adainit (Elab_Order : Unit_Id_Array) is
      Main_Priority : Int renames ALIs.Table (ALIs.First).Main_Priority;
      Main_CPU      : Int renames ALIs.Table (ALIs.First).Main_CPU;

   begin
      --  Declare the access-to-subprogram type used for initialization of
      --  of __gnat_finalize_library_objects. This is declared at library
      --  level for compatibility with the type used in System.Soft_Links.
      --  The import of the soft link which performs library-level object
      --  finalization does not work for CodePeer, so regular Ada is used in
      --  that case. For restricted run-time libraries (ZFP and Ravenscar)
      --  tasks are non-terminating, so we do not want finalization.

      if not Suppress_Standard_Library_On_Target
        and then not CodePeer_Mode
        and then not Configurable_Run_Time_On_Target
      then
         WBI ("   type No_Param_Proc is access procedure;");
         WBI ("");
      end if;

      WBI ("   procedure " & Ada_Init_Name.all & " is");

      --  In CodePeer mode, simplify adainit procedure by only calling
      --  elaboration procedures.

      if CodePeer_Mode then
         WBI ("   begin");

      --  If the standard library is suppressed, then the only global variables
      --  that might be needed (by the Ravenscar profile) are the priority and
      --  the processor for the environment task.

      elsif Suppress_Standard_Library_On_Target then
         if Main_Priority /= No_Main_Priority then
            WBI ("      Main_Priority : Integer;");
            WBI ("      pragma Import (C, Main_Priority," &
                 " ""__gl_main_priority"");");
            WBI ("");
         end if;

         if Main_CPU /= No_Main_CPU then
            WBI ("      Main_CPU : Integer;");
            WBI ("      pragma Import (C, Main_CPU," &
                 " ""__gl_main_cpu"");");
            WBI ("");
         end if;

         if System_Interrupts_Used
           and then Partition_Elaboration_Policy_Specified = 'S'
         then
            WBI ("      procedure Install_Restricted_Handlers_Sequential;");
            WBI ("      pragma Import (C," &
                 "Install_Restricted_Handlers_Sequential," &
                 " ""__gnat_attach_all_handlers"");");
            WBI ("");
         end if;

         if System_Tasking_Restricted_Stages_Used
           and then Partition_Elaboration_Policy_Specified = 'S'
         then
            WBI ("      Partition_Elaboration_Policy : Character;");
            WBI ("      pragma Import (C, Partition_Elaboration_Policy," &
                 " ""__gnat_partition_elaboration_policy"");");
            WBI ("");
            WBI ("      procedure Activate_All_Tasks_Sequential;");
            WBI ("      pragma Import (C, Activate_All_Tasks_Sequential," &
                 " ""__gnat_activate_all_tasks"");");
            WBI ("");
         end if;

         if System_BB_CPU_Primitives_Multiprocessors_Used then
            WBI ("      procedure Start_Slave_CPUs;");
            WBI ("      pragma Import (C, Start_Slave_CPUs," &
                 " ""__gnat_start_slave_cpus"");");
            WBI ("");
         end if;

         --  A restricted run-time may attempt to initialize the main task's
         --  secondary stack even if the stack is not used. Consequently,
         --  the binder needs to initialize Binder_Sec_Stacks_Count anytime
         --  System.Secondary_Stack is in the enclosure of the partition.

         if System_Secondary_Stack_Used then
            WBI ("      Binder_Sec_Stacks_Count : Natural;");
            WBI ("      pragma Import (Ada, Binder_Sec_Stacks_Count, " &
                 """__gnat_binder_ss_count"");");
            WBI ("");
         end if;

         if Sec_Stack_Used then
            WBI ("      Default_Secondary_Stack_Size : " &
                 "System.Parameters.Size_Type;");
            WBI ("      pragma Import (C, Default_Secondary_Stack_Size, " &
                 """__gnat_default_ss_size"");");

            WBI ("      Default_Sized_SS_Pool : System.Address;");
            WBI ("      pragma Import (Ada, Default_Sized_SS_Pool, " &
                 """__gnat_default_ss_pool"");");

            WBI ("");
         end if;

         WBI ("   begin");

         if Main_Priority /= No_Main_Priority then
            Set_String ("      Main_Priority := ");
            Set_Int    (Main_Priority);
            Set_Char   (';');
            Write_Statement_Buffer;
         end if;

         if Main_CPU /= No_Main_CPU then
            Set_String ("      Main_CPU := ");
            Set_Int    (Main_CPU);
            Set_Char   (';');
            Write_Statement_Buffer;
         end if;

         if System_Tasking_Restricted_Stages_Used
           and then Partition_Elaboration_Policy_Specified = 'S'
         then
            Set_String ("      Partition_Elaboration_Policy := '");
            Set_Char   (Partition_Elaboration_Policy_Specified);
            Set_String ("';");
            Write_Statement_Buffer;
         end if;

         if Main_Priority = No_Main_Priority
           and then Main_CPU = No_Main_CPU
           and then not System_Tasking_Restricted_Stages_Used
         then
            WBI ("      null;");
         end if;

         --  Generate default-sized secondary stack pool and set secondary
         --  stack globals.

         if Sec_Stack_Used then

            --  Elaborate the body of the binder to initialize the default-
            --  sized secondary stack pool.

            WBI ("");
            WBI ("      " & Get_Ada_Main_Name & "'Elab_Body;");

            --  Generate the default-sized secondary stack pool and set the
            --  related secondary stack globals.

            Set_String ("      Default_Secondary_Stack_Size := ");

            if Opt.Default_Sec_Stack_Size /= Opt.No_Stack_Size then
               Set_Int (Opt.Default_Sec_Stack_Size);
            else
               Set_String ("System.Parameters.Runtime_Default_Sec_Stack_Size");
            end if;

            Set_Char (';');
            Write_Statement_Buffer;

            Set_String ("      Binder_Sec_Stacks_Count := ");
            Set_Int (Num_Sec_Stacks);
            Set_Char (';');
            Write_Statement_Buffer;

            WBI ("      Default_Sized_SS_Pool := " &
                   "Sec_Default_Sized_Stacks'Address;");
            WBI ("");

         --  When a restricted run-time initializes the main task's secondary
         --  stack but the program does not use it, no secondary stack is
         --  generated. Binder_Sec_Stacks_Count is set to zero so the run-time
         --  is aware that the lack of pre-allocated secondary stack is
         --  expected.

         elsif System_Secondary_Stack_Used then
            WBI ("      Binder_Sec_Stacks_Count := 0;");
         end if;

      --  Normal case (standard library not suppressed). Set all global values
      --  used by the run time.

      else
         WBI ("      Main_Priority : Integer;");
         WBI ("      pragma Import (C, Main_Priority, " &
              """__gl_main_priority"");");
         WBI ("      Time_Slice_Value : Integer;");
         WBI ("      pragma Import (C, Time_Slice_Value, " &
              """__gl_time_slice_val"");");
         WBI ("      WC_Encoding : Character;");
         WBI ("      pragma Import (C, WC_Encoding, ""__gl_wc_encoding"");");
         WBI ("      Locking_Policy : Character;");
         WBI ("      pragma Import (C, Locking_Policy, " &
              """__gl_locking_policy"");");
         WBI ("      Queuing_Policy : Character;");
         WBI ("      pragma Import (C, Queuing_Policy, " &
              """__gl_queuing_policy"");");
         WBI ("      Task_Dispatching_Policy : Character;");
         WBI ("      pragma Import (C, Task_Dispatching_Policy, " &
              """__gl_task_dispatching_policy"");");
         WBI ("      Priority_Specific_Dispatching : System.Address;");
         WBI ("      pragma Import (C, Priority_Specific_Dispatching, " &
              """__gl_priority_specific_dispatching"");");
         WBI ("      Num_Specific_Dispatching : Integer;");
         WBI ("      pragma Import (C, Num_Specific_Dispatching, " &
              """__gl_num_specific_dispatching"");");
         WBI ("      Main_CPU : Integer;");
         WBI ("      pragma Import (C, Main_CPU, " &
              """__gl_main_cpu"");");

         WBI ("      Interrupt_States : System.Address;");
         WBI ("      pragma Import (C, Interrupt_States, " &
              """__gl_interrupt_states"");");
         WBI ("      Num_Interrupt_States : Integer;");
         WBI ("      pragma Import (C, Num_Interrupt_States, " &
              """__gl_num_interrupt_states"");");
         WBI ("      Unreserve_All_Interrupts : Integer;");
         WBI ("      pragma Import (C, Unreserve_All_Interrupts, " &
              """__gl_unreserve_all_interrupts"");");

         if Exception_Tracebacks or Exception_Tracebacks_Symbolic then
            WBI ("      Exception_Tracebacks : Integer;");
            WBI ("      pragma Import (C, Exception_Tracebacks, " &
                 """__gl_exception_tracebacks"");");

            if Exception_Tracebacks_Symbolic then
               WBI ("      Exception_Tracebacks_Symbolic : Integer;");
               WBI ("      pragma Import (C, Exception_Tracebacks_Symbolic, " &
                    """__gl_exception_tracebacks_symbolic"");");
            end if;
         end if;

         WBI ("      Detect_Blocking : Integer;");
         WBI ("      pragma Import (C, Detect_Blocking, " &
              """__gl_detect_blocking"");");
         WBI ("      Default_Stack_Size : Integer;");
         WBI ("      pragma Import (C, Default_Stack_Size, " &
              """__gl_default_stack_size"");");
         WBI ("      Default_Secondary_Stack_Size : " &
              "System.Parameters.Size_Type;");
         WBI ("      pragma Import (C, Default_Secondary_Stack_Size, " &
              """__gnat_default_ss_size"");");
         WBI ("      Leap_Seconds_Support : Integer;");
         WBI ("      pragma Import (C, Leap_Seconds_Support, " &
              """__gl_leap_seconds_support"");");
         WBI ("      Bind_Env_Addr : System.Address;");
         WBI ("      pragma Import (C, Bind_Env_Addr, " &
              """__gl_bind_env_addr"");");

         --  Import entry point for elaboration time signal handler
         --  installation, and indication of if it's been called previously.

         WBI ("");
         WBI ("      procedure Runtime_Initialize " &
              "(Install_Handler : Integer);");
         WBI ("      pragma Import (C, Runtime_Initialize, " &
              """__gnat_runtime_initialize"");");

         --  Import handlers attach procedure for sequential elaboration policy

         if System_Interrupts_Used
           and then Partition_Elaboration_Policy_Specified = 'S'
         then
            WBI ("      procedure Install_Restricted_Handlers_Sequential;");
            WBI ("      pragma Import (C," &
                 "Install_Restricted_Handlers_Sequential," &
                 " ""__gnat_attach_all_handlers"");");
            WBI ("");
         end if;

         --  Import task activation procedure for sequential elaboration
         --  policy.

         if System_Tasking_Restricted_Stages_Used
           and then Partition_Elaboration_Policy_Specified = 'S'
         then
            WBI ("      Partition_Elaboration_Policy : Character;");
            WBI ("      pragma Import (C, Partition_Elaboration_Policy," &
                 " ""__gnat_partition_elaboration_policy"");");
            WBI ("");
            WBI ("      procedure Activate_All_Tasks_Sequential;");
            WBI ("      pragma Import (C, Activate_All_Tasks_Sequential," &
                 " ""__gnat_activate_all_tasks"");");
         end if;

         --  Import procedure to start slave cpus for bareboard runtime

         if System_BB_CPU_Primitives_Multiprocessors_Used then
            WBI ("      procedure Start_Slave_CPUs;");
            WBI ("      pragma Import (C, Start_Slave_CPUs," &
                 " ""__gnat_start_slave_cpus"");");
         end if;

         --  For restricted run-time libraries (ZFP and Ravenscar)
         --  tasks are non-terminating, so we do not want finalization.

         if not Configurable_Run_Time_On_Target then
            WBI ("");
            WBI ("      Finalize_Library_Objects : No_Param_Proc;");
            WBI ("      pragma Import (C, Finalize_Library_Objects, " &
                 """__gnat_finalize_library_objects"");");
         end if;

         --  Initialize stack limit variable of the environment task if the
         --  stack check method is stack limit and stack check is enabled.

         if Stack_Check_Limits_On_Target
           and then (Stack_Check_Default_On_Target or Stack_Check_Switch_Set)
         then
            WBI ("");
            WBI ("      procedure Initialize_Stack_Limit;");
            WBI ("      pragma Import (C, Initialize_Stack_Limit, " &
                 """__gnat_initialize_stack_limit"");");
         end if;

         --  When dispatching domains are used then we need to signal it
         --  before calling the main procedure.

         if Dispatching_Domains_Used then
            WBI ("      procedure Freeze_Dispatching_Domains;");
            WBI ("      pragma Import");
            WBI ("        (Ada, Freeze_Dispatching_Domains, "
                 & """__gnat_freeze_dispatching_domains"");");
         end if;

         --  Secondary stack global variables

         WBI ("      Binder_Sec_Stacks_Count : Natural;");
         WBI ("      pragma Import (Ada, Binder_Sec_Stacks_Count, " &
              """__gnat_binder_ss_count"");");

         WBI ("      Default_Sized_SS_Pool : System.Address;");
         WBI ("      pragma Import (Ada, Default_Sized_SS_Pool, " &
              """__gnat_default_ss_pool"");");

         WBI ("");

         --  Start of processing for Adainit

         WBI ("   begin");
         WBI ("      if Is_Elaborated then");
         WBI ("         return;");
         WBI ("      end if;");
         WBI ("      Is_Elaborated := True;");

         --  Call System.Elaboration_Allocators.Mark_Start_Of_Elaboration if
         --  restriction No_Standard_Allocators_After_Elaboration is active.

         if Cumulative_Restrictions.Set
              (No_Standard_Allocators_After_Elaboration)
         then
            WBI ("      System.Elaboration_Allocators."
                 & "Mark_Start_Of_Elaboration;");
         end if;

         --  Generate assignments to initialize globals

         Set_String ("      Main_Priority := ");
         Set_Int    (Main_Priority);
         Set_Char   (';');
         Write_Statement_Buffer;

         Set_String ("      Time_Slice_Value := ");

         if Task_Dispatching_Policy_Specified = 'F'
           and then ALIs.Table (ALIs.First).Time_Slice_Value = -1
         then
            Set_Int (0);
         else
            Set_Int (ALIs.Table (ALIs.First).Time_Slice_Value);
         end if;

         Set_Char   (';');
         Write_Statement_Buffer;

         Set_String ("      WC_Encoding := '");
         Set_Char   (Get_WC_Encoding);

         Set_String ("';");
         Write_Statement_Buffer;

         Set_String ("      Locking_Policy := '");
         Set_Char   (Locking_Policy_Specified);
         Set_String ("';");
         Write_Statement_Buffer;

         Set_String ("      Queuing_Policy := '");
         Set_Char   (Queuing_Policy_Specified);
         Set_String ("';");
         Write_Statement_Buffer;

         Set_String ("      Task_Dispatching_Policy := '");
         Set_Char   (Task_Dispatching_Policy_Specified);
         Set_String ("';");
         Write_Statement_Buffer;

         if System_Tasking_Restricted_Stages_Used
           and then Partition_Elaboration_Policy_Specified = 'S'
         then
            Set_String ("      Partition_Elaboration_Policy := '");
            Set_Char   (Partition_Elaboration_Policy_Specified);
            Set_String ("';");
            Write_Statement_Buffer;
         end if;

         Gen_Restrictions;

         WBI ("      Priority_Specific_Dispatching :=");
         WBI ("        Local_Priority_Specific_Dispatching'Address;");

         Set_String ("      Num_Specific_Dispatching := ");
         Set_Int (PSD_Pragma_Settings.Last + 1);
         Set_Char (';');
         Write_Statement_Buffer;

         Set_String ("      Main_CPU := ");
         Set_Int    (Main_CPU);
         Set_Char   (';');
         Write_Statement_Buffer;

         WBI ("      Interrupt_States := Local_Interrupt_States'Address;");

         Set_String ("      Num_Interrupt_States := ");
         Set_Int (IS_Pragma_Settings.Last + 1);
         Set_Char (';');
         Write_Statement_Buffer;

         Set_String ("      Unreserve_All_Interrupts := ");

         if Unreserve_All_Interrupts_Specified then
            Set_String ("1");
         else
            Set_String ("0");
         end if;

         Set_Char (';');
         Write_Statement_Buffer;

         if Exception_Tracebacks or Exception_Tracebacks_Symbolic then
            WBI ("      Exception_Tracebacks := 1;");

            if Exception_Tracebacks_Symbolic then
               WBI ("      Exception_Tracebacks_Symbolic := 1;");
            end if;
         end if;

         Set_String ("      Detect_Blocking := ");

         if Detect_Blocking then
            Set_Int (1);
         else
            Set_Int (0);
         end if;

         Set_String (";");
         Write_Statement_Buffer;

         Set_String ("      Default_Stack_Size := ");
         Set_Int (Default_Stack_Size);
         Set_String (";");
         Write_Statement_Buffer;

         Set_String ("      Leap_Seconds_Support := ");

         if Leap_Seconds_Support then
            Set_Int (1);
         else
            Set_Int (0);
         end if;

         Set_String (";");
         Write_Statement_Buffer;

         if Bind_Env_String_Built then
            WBI ("      Bind_Env_Addr := Bind_Env'Address;");
         end if;

         WBI ("");

         --  Generate default-sized secondary stack pool and set secondary
         --  stack globals.

         if Sec_Stack_Used then

            --  Elaborate the body of the binder to initialize the default-
            --  sized secondary stack pool.

            WBI ("      " & Get_Ada_Main_Name & "'Elab_Body;");

            --  Generate the default-sized secondary stack pool and set the
            --  related secondary stack globals.

            Set_String ("      Default_Secondary_Stack_Size := ");

            if Opt.Default_Sec_Stack_Size /= Opt.No_Stack_Size then
               Set_Int (Opt.Default_Sec_Stack_Size);
            else
               Set_String ("System.Parameters.Runtime_Default_Sec_Stack_Size");
            end if;

            Set_Char (';');
            Write_Statement_Buffer;

            Set_String ("      Binder_Sec_Stacks_Count := ");
            Set_Int (Num_Sec_Stacks);
            Set_Char (';');
            Write_Statement_Buffer;

            Set_String ("      Default_Sized_SS_Pool := ");

            if Num_Sec_Stacks > 0 then
               Set_String ("Sec_Default_Sized_Stacks'Address;");
            else
               Set_String ("System.Null_Address;");
            end if;

            Write_Statement_Buffer;
            WBI ("");
         end if;

         --  Generate call to Runtime_Initialize

         WBI ("      Runtime_Initialize (1);");
      end if;

      --  Generate call to set Initialize_Scalar values if active

      if Initialize_Scalars_Used then
         WBI ("");
         Set_String ("      System.Scalar_Values.Initialize ('");
         Set_Char (Initialize_Scalars_Mode1);
         Set_String ("', '");
         Set_Char (Initialize_Scalars_Mode2);
         Set_String ("');");
         Write_Statement_Buffer;
      end if;

      --  Initialize stack limit variable of the environment task if the stack
      --  check method is stack limit and stack check is enabled.

      if Stack_Check_Limits_On_Target
        and then (Stack_Check_Default_On_Target or Stack_Check_Switch_Set)
      then
         WBI ("");
         WBI ("      Initialize_Stack_Limit;");
      end if;

      --  On CodePeer, the finalization of library objects is not relevant

      if CodePeer_Mode then
         null;

      --  If this is the main program case, attach finalize_library to the soft
      --  link. Do it only when not using a restricted run time, in which case
      --  tasks are non-terminating, so we do not want library-level
      --  finalization.

      elsif not Bind_For_Library
        and then not Configurable_Run_Time_On_Target
        and then not Suppress_Standard_Library_On_Target
      then
         WBI ("");

         if Lib_Final_Built then
            Set_String ("      Finalize_Library_Objects := ");
            Set_String ("finalize_library'access;");
         else
            Set_String ("      Finalize_Library_Objects := null;");
         end if;

         Write_Statement_Buffer;
      end if;

      --  Generate elaboration calls

      if not CodePeer_Mode then
         WBI ("");
      end if;

      Gen_Elab_Calls (Elab_Order);

      if not CodePeer_Mode then

         --  Call System.Elaboration_Allocators.Mark_Start_Of_Elaboration if
         --  restriction No_Standard_Allocators_After_Elaboration is active.

         if Cumulative_Restrictions.Set
              (No_Standard_Allocators_After_Elaboration)
         then
            WBI
              ("      System.Elaboration_Allocators.Mark_End_Of_Elaboration;");
         end if;

         --  From this point, no new dispatching domain can be created

         if Dispatching_Domains_Used then
            WBI ("      Freeze_Dispatching_Domains;");
         end if;

         --  Sequential partition elaboration policy

         if Partition_Elaboration_Policy_Specified = 'S' then
            if System_Interrupts_Used then
               WBI ("      Install_Restricted_Handlers_Sequential;");
            end if;

            if System_Tasking_Restricted_Stages_Used then
               WBI ("      Activate_All_Tasks_Sequential;");
            end if;
         end if;

         if System_BB_CPU_Primitives_Multiprocessors_Used then
            WBI ("      Start_Slave_CPUs;");
         end if;
      end if;

      WBI ("   end " & Ada_Init_Name.all & ";");
      WBI ("");
   end Gen_Adainit;

   -------------------------
   -- Gen_Bind_Env_String --
   -------------------------

   procedure Gen_Bind_Env_String is
      procedure Write_Name_With_Len (Nam : Name_Id);
      --  Write Nam as a string literal, prefixed with one
      --  character encoding Nam's length.

      -------------------------
      -- Write_Name_With_Len --
      -------------------------

      procedure Write_Name_With_Len (Nam : Name_Id) is
      begin
         Get_Name_String (Nam);

         Start_String;
         Store_String_Char (Character'Val (Name_Len));
         Store_String_Chars (Name_Buffer (1 .. Name_Len));

         Write_String_Table_Entry (End_String);
      end Write_Name_With_Len;

      --  Local variables

      Amp : Character;
      KN  : Name_Id := No_Name;
      VN  : Name_Id := No_Name;

   --  Start of processing for Gen_Bind_Env_String

   begin
      Bind_Environment.Get_First (KN, VN);

      if VN = No_Name then
         return;
      end if;

      Set_Special_Output (Write_Bind_Line'Access);

      WBI ("   Bind_Env : aliased constant String :=");
      Amp := ' ';
      while VN /= No_Name loop
         Write_Str ("     " & Amp & ' ');
         Write_Name_With_Len (KN);
         Write_Str (" & ");
         Write_Name_With_Len (VN);
         Write_Eol;

         Bind_Environment.Get_Next (KN, VN);
         Amp := '&';
      end loop;
      WBI ("     & ASCII.NUL;");

      Set_Special_Output (null);

      Bind_Env_String_Built := True;
   end Gen_Bind_Env_String;

   --------------------------
   -- Gen_CodePeer_Wrapper --
   --------------------------

   procedure Gen_CodePeer_Wrapper is
      Callee_Name : constant String := "Ada_Main_Program";

   begin
      if ALIs.Table (ALIs.First).Main_Program = Proc then
         WBI ("   procedure " & CodePeer_Wrapper_Name & " is ");
         WBI ("   begin");
         WBI ("      " & Callee_Name & ";");

      else
         WBI ("   function " & CodePeer_Wrapper_Name & " return Integer is");
         WBI ("   begin");
         WBI ("      return " & Callee_Name & ";");
      end if;

      WBI ("   end " & CodePeer_Wrapper_Name & ";");
      WBI ("");
   end Gen_CodePeer_Wrapper;

   --------------------
   -- Gen_Elab_Calls --
   --------------------

   procedure Gen_Elab_Calls (Elab_Order : Unit_Id_Array) is
      Check_Elab_Flag : Boolean;

   begin
      --  Loop through elaboration order entries

      for E in Elab_Order'Range loop
         declare
            Unum : constant Unit_Id := Elab_Order (E);
            U    : Unit_Record renames Units.Table (Unum);

            Unum_Spec : Unit_Id;
            --  This is the unit number of the spec that corresponds to
            --  this entry. It is the same as Unum except when the body
            --  and spec are different and we are currently processing
            --  the body, in which case it is the spec (Unum + 1).

         begin
            if U.Utype = Is_Body then
               Unum_Spec := Unum + 1;
            else
               Unum_Spec := Unum;
            end if;

            --  Nothing to do if predefined unit in no run time mode

            if No_Run_Time_Mode and then Is_Predefined_File_Name (U.Sfile) then
               null;

            --  Likewise if this is an interface to a stand alone library

            elsif U.SAL_Interface then
               null;

            --  Case of no elaboration code

            elsif U.No_Elab

              --  In CodePeer mode, we special case subprogram bodies which
              --  are handled in the 'else' part below, and lead to a call
              --  to <subp>'Elab_Subp_Body.

              and then (not CodePeer_Mode

                         --  Test for spec

                         or else U.Utype = Is_Spec
                         or else U.Utype = Is_Spec_Only
                         or else U.Unit_Kind /= 's')
            then
               --  In the case of a body with a separate spec, where the
               --  separate spec has an elaboration entity defined, this is
               --  where we increment the elaboration entity if one exists.

               --  Likewise for lone specs with an elaboration entity defined
               --  despite No_Elaboration_Code, e.g. when requested to preserve
               --  control flow.

               if (U.Utype = Is_Body or else U.Utype = Is_Spec_Only)
                 and then Units.Table (Unum_Spec).Set_Elab_Entity
                 and then not CodePeer_Mode
               then
                  Set_String ("      E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" := E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" + 1;");
                  Write_Statement_Buffer;
               end if;

            --  Here if elaboration code is present. If binding a library
            --  or if there is a non-Ada main subprogram then we generate:

            --    if uname_E = 0 then
            --       uname'elab_[spec|body];
            --    end if;
            --    uname_E := uname_E + 1;

            --  Otherwise, elaboration routines are called unconditionally:

            --    uname'elab_[spec|body];
            --    uname_E := uname_E + 1;

            --  The uname_E increment is skipped if this is a separate spec,
            --  since it will be done when we process the body.

            --  In CodePeer mode, we do not generate any reference to xxx_E
            --  variables, only calls to 'Elab* subprograms.

            else
               --  Check incompatibilities with No_Multiple_Elaboration

               if not CodePeer_Mode
                 and then Cumulative_Restrictions.Set (No_Multiple_Elaboration)
               then
                  --  Force_Checking_Of_Elaboration_Flags (-F) not allowed

                  if Force_Checking_Of_Elaboration_Flags then
                     Osint.Fail
                       ("-F (force elaboration checks) switch not allowed "
                        & "with restriction No_Multiple_Elaboration active");

                  --  Interfacing of libraries not allowed

                  elsif Interface_Library_Unit then
                     Osint.Fail
                       ("binding of interfaced libraries not allowed "
                        & "with restriction No_Multiple_Elaboration active");

                  --  Non-Ada main program not allowed

                  elsif not Bind_Main_Program then
                     Osint.Fail
                       ("non-Ada main program not allowed "
                        & "with restriction No_Multiple_Elaboration active");
                  end if;
               end if;

               --  OK, see if we need to test elaboration flag

               Check_Elab_Flag :=
                 Units.Table (Unum_Spec).Set_Elab_Entity
                   and then not CodePeer_Mode
                   and then (Force_Checking_Of_Elaboration_Flags
                              or Interface_Library_Unit
                              or not Bind_Main_Program);

               if Check_Elab_Flag then
                  Set_String ("      if E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" = 0 then");
                  Write_Statement_Buffer;
                  Set_String ("   ");
               end if;

               Set_String ("      ");
               Get_Decoded_Name_String_With_Brackets (U.Uname);

               if Name_Buffer (Name_Len) = 's' then
                  Name_Buffer (Name_Len - 1 .. Name_Len + 8) :=
                    "'elab_spec";
                  Name_Len := Name_Len + 8;

               --  Special case in CodePeer mode for subprogram bodies
               --  which correspond to CodePeer 'Elab_Subp_Body special
               --  init procedure.

               elsif U.Unit_Kind = 's' and CodePeer_Mode then
                  Name_Buffer (Name_Len - 1 .. Name_Len + 13) :=
                    "'elab_subp_body";
                  Name_Len := Name_Len + 13;

               else
                  Name_Buffer (Name_Len - 1 .. Name_Len + 8) :=
                    "'elab_body";
                  Name_Len := Name_Len + 8;
               end if;

               Set_Casing (U.Icasing);
               Set_Name_Buffer;
               Set_Char (';');
               Write_Statement_Buffer;

               if Check_Elab_Flag then
                  WBI ("      end if;");
               end if;

               if U.Utype /= Is_Spec
                 and then not CodePeer_Mode
                 and then Units.Table (Unum_Spec).Set_Elab_Entity
               then
                  Set_String ("      E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" := E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" + 1;");
                  Write_Statement_Buffer;
               end if;
            end if;
         end;
      end loop;
   end Gen_Elab_Calls;

   ------------------------
   -- Gen_Elab_Externals --
   ------------------------

   procedure Gen_Elab_Externals (Elab_Order : Unit_Id_Array) is
   begin
      if CodePeer_Mode then
         return;
      end if;

      for E in Elab_Order'Range loop
         declare
            Unum : constant Unit_Id := Elab_Order (E);
            U    : Unit_Record renames Units.Table (Unum);

         begin
            --  Check for Elab_Entity to be set for this unit

            if U.Set_Elab_Entity

              --  Don't generate reference for stand alone library

              and then not U.SAL_Interface

              --  Don't generate reference for predefined file in No_Run_Time
              --  mode, since we don't include the object files in this case

              and then not
                (No_Run_Time_Mode
                  and then Is_Predefined_File_Name (U.Sfile))
            then
               Get_Name_String (U.Sfile);
               Set_String ("   ");
               Set_String ("E");
               Set_Unit_Number (Unum);
               Set_String (" : Short_Integer; pragma Import (Ada, E");
               Set_Unit_Number (Unum);
               Set_String (", """);
               Get_Name_String (U.Uname);
               Set_Unit_Name;
               Set_String ("_E"");");
               Write_Statement_Buffer;
            end if;
         end;
      end loop;

      WBI ("");
   end Gen_Elab_Externals;

   --------------------
   -- Gen_Elab_Order --
   --------------------

   procedure Gen_Elab_Order (Elab_Order : Unit_Id_Array) is
   begin
      WBI ("");
      WBI ("   --  BEGIN ELABORATION ORDER");

      for J in Elab_Order'Range loop
         Set_String ("   --  ");
         Get_Name_String (Units.Table (Elab_Order (J)).Uname);
         Set_Name_Buffer;
         Write_Statement_Buffer;
      end loop;

      WBI ("   --  END ELABORATION ORDER");
   end Gen_Elab_Order;

   --------------------------
   -- Gen_Finalize_Library --
   --------------------------

   procedure Gen_Finalize_Library (Elab_Order : Unit_Id_Array) is
      procedure Gen_Header;
      --  Generate the header of the finalization routine

      ----------------
      -- Gen_Header --
      ----------------

      procedure Gen_Header is
      begin
         WBI ("   procedure finalize_library is");
         WBI ("   begin");
      end Gen_Header;

      --  Local variables

      Count : Int := 1;
      U     : Unit_Record;
      Uspec : Unit_Record;
      Unum  : Unit_Id;

   --  Start of processing for Gen_Finalize_Library

   begin
      if CodePeer_Mode then
         return;
      end if;

      for E in reverse Elab_Order'Range loop
         Unum := Elab_Order (E);
         U    := Units.Table (Unum);

         --  Dealing with package bodies is a little complicated. In such
         --  cases we must retrieve the package spec since it contains the
         --  spec of the body finalizer.

         if U.Utype = Is_Body then
            Unum  := Unum + 1;
            Uspec := Units.Table (Unum);
         else
            Uspec := U;
         end if;

         Get_Name_String (Uspec.Uname);

         --  We are only interested in non-generic packages

         if U.Unit_Kind /= 'p' or else U.Is_Generic then
            null;

         --  That aren't an interface to a stand alone library

         elsif U.SAL_Interface then
            null;

         --  Case of no finalization

         elsif not U.Has_Finalizer then

            --  The only case in which we have to do something is if this
            --  is a body, with a separate spec, where the separate spec
            --  has a finalizer. In that case, this is where we decrement
            --  the elaboration entity.

            if U.Utype = Is_Body and then Uspec.Has_Finalizer then
               if not Lib_Final_Built then
                  Gen_Header;
                  Lib_Final_Built := True;
               end if;

               Set_String ("      E");
               Set_Unit_Number (Unum);
               Set_String (" := E");
               Set_Unit_Number (Unum);
               Set_String (" - 1;");
               Write_Statement_Buffer;
            end if;

         else
            if not Lib_Final_Built then
               Gen_Header;
               Lib_Final_Built := True;
            end if;

            --  Generate:
            --    declare
            --       procedure F<Count>;

            Set_String ("      declare");
            Write_Statement_Buffer;

            Set_String ("         procedure F");
            Set_Int    (Count);
            Set_Char   (';');
            Write_Statement_Buffer;

            --  Generate:
            --    pragma Import (Ada, F<Count>,
            --                  "xx__yy__finalize_[body|spec]");

            Set_String ("         pragma Import (Ada, F");
            Set_Int (Count);
            Set_String (", """);

            --  Perform name construction

            Set_Unit_Name;
            Set_String ("__finalize_");

            --  Package spec processing

            if U.Utype = Is_Spec
              or else U.Utype = Is_Spec_Only
            then
               Set_String ("spec");

            --  Package body processing

            else
               Set_String ("body");
            end if;

            Set_String (""");");
            Write_Statement_Buffer;

            --  If binding a library or if there is a non-Ada main subprogram
            --  then we generate:

            --    begin
            --       uname_E := uname_E - 1;
            --       if uname_E = 0 then
            --          F<Count>;
            --       end if;
            --    end;

            --  Otherwise, finalization routines are called unconditionally:

            --    begin
            --       uname_E := uname_E - 1;
            --       F<Count>;
            --    end;

            --  The uname_E decrement is skipped if this is a separate spec,
            --  since it will be done when we process the body.

            WBI ("      begin");

            if U.Utype /= Is_Spec then
               Set_String ("         E");
               Set_Unit_Number (Unum);
               Set_String (" := E");
               Set_Unit_Number (Unum);
               Set_String (" - 1;");
               Write_Statement_Buffer;
            end if;

            if Interface_Library_Unit or not Bind_Main_Program then
               Set_String ("         if E");
               Set_Unit_Number (Unum);
               Set_String (" = 0 then");
               Write_Statement_Buffer;
               Set_String ("   ");
            end if;

            Set_String ("         F");
            Set_Int    (Count);
            Set_Char   (';');
            Write_Statement_Buffer;

            if Interface_Library_Unit or not Bind_Main_Program then
               WBI ("         end if;");
            end if;

            WBI ("      end;");

            Count := Count + 1;
         end if;
      end loop;

      if Lib_Final_Built then

         --  It is possible that the finalization of a library-level object
         --  raised an exception. In that case import the actual exception
         --  and the routine necessary to raise it.

         WBI ("      declare");
         WBI ("         procedure Reraise_Library_Exception_If_Any;");

         Set_String ("            pragma Import (Ada, ");
         Set_String ("Reraise_Library_Exception_If_Any, ");
         Set_String ("""__gnat_reraise_library_exception_if_any"");");
         Write_Statement_Buffer;

         WBI ("      begin");
         WBI ("         Reraise_Library_Exception_If_Any;");
         WBI ("      end;");
         WBI ("   end finalize_library;");
         WBI ("");
      end if;
   end Gen_Finalize_Library;

   --------------
   -- Gen_Main --
   --------------

   procedure Gen_Main is
   begin
      if not No_Main_Subprogram then

         --  To call the main program, we declare it using a pragma Import
         --  Ada with the right link name.

         --  It might seem more obvious to "with" the main program, and call
         --  it in the normal Ada manner. We do not do this for three
         --  reasons:

         --    1. It is more efficient not to recompile the main program
         --    2. We are not entitled to assume the source is accessible
         --    3. We don't know what options to use to compile it

         --  It is really reason 3 that is most critical (indeed we used
         --  to generate the "with", but several regression tests failed).

         if ALIs.Table (ALIs.First).Main_Program = Func then
            WBI ("   function Ada_Main_Program return Integer;");
         else
            WBI ("   procedure Ada_Main_Program;");
         end if;

         Set_String ("   pragma Import (Ada, Ada_Main_Program, """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (""");");

         Write_Statement_Buffer;
         WBI ("");

         --  For CodePeer, declare a wrapper for the user-defined main program

         if CodePeer_Mode then
            Gen_CodePeer_Wrapper;
         end if;
      end if;

      if Exit_Status_Supported_On_Target then
         Set_String ("   function ");
      else
         Set_String ("   procedure ");
      end if;

      Set_String (Get_Main_Name);

      if Command_Line_Args_On_Target then
         Write_Statement_Buffer;
         WBI ("     (argc : Integer;");
         WBI ("      argv : System.Address;");
         WBI ("      envp : System.Address)");

         if Exit_Status_Supported_On_Target then
            WBI ("      return Integer");
         end if;

         WBI ("   is");

      else
         if Exit_Status_Supported_On_Target then
            Set_String (" return Integer is");
         else
            Set_String (" is");
         end if;

         Write_Statement_Buffer;
      end if;

      if Opt.Default_Exit_Status /= 0
        and then Bind_Main_Program
        and then not Configurable_Run_Time_Mode
      then
         WBI ("      procedure Set_Exit_Status (Status : Integer);");
         WBI ("      pragma Import (C, Set_Exit_Status, " &
                     """__gnat_set_exit_status"");");
         WBI ("");
      end if;

      --  Initialize and Finalize

      if not CodePeer_Mode
        and then not Cumulative_Restrictions.Set (No_Finalization)
      then
         WBI ("      procedure Initialize (Addr : System.Address);");
         WBI ("      pragma Import (C, Initialize, ""__gnat_initialize"");");
         WBI ("");
         WBI ("      procedure Finalize;");
         WBI ("      pragma Import (C, Finalize, ""__gnat_finalize"");");
      end if;

      --  If we want to analyze the stack, we must import corresponding symbols

      if Dynamic_Stack_Measurement then
         WBI ("");
         WBI ("      procedure Output_Results;");
         WBI ("      pragma Import (C, Output_Results, " &
              """__gnat_stack_usage_output_results"");");

         WBI ("");
         WBI ("      " &
              "procedure Initialize_Stack_Analysis (Buffer_Size : Natural);");
         WBI ("      pragma Import (C, Initialize_Stack_Analysis, " &
              """__gnat_stack_usage_initialize"");");
      end if;

      --  Deal with declarations for main program case

      if not No_Main_Subprogram then
         if ALIs.Table (ALIs.First).Main_Program = Func then
            WBI ("      Result : Integer;");
            WBI ("");
         end if;

         if Bind_Main_Program
           and not Suppress_Standard_Library_On_Target
           and not CodePeer_Mode
         then
            WBI ("      SEH : aliased array (1 .. 2) of Integer;");
            WBI ("");
         end if;
      end if;

      --  Generate a reference to Ada_Main_Program_Name. This symbol is not
      --  referenced elsewhere in the generated program, but is needed by
      --  the debugger (that's why it is generated in the first place). The
      --  reference stops Ada_Main_Program_Name from being optimized away by
      --  smart linkers, such as the AiX linker.

      --  Because this variable is unused, we make this variable "aliased"
      --  with a pragma Volatile in order to tell the compiler to preserve
      --  this variable at any level of optimization.

      if Bind_Main_Program and not CodePeer_Mode then
         WBI ("      Ensure_Reference : aliased System.Address := " &
              "Ada_Main_Program_Name'Address;");
         WBI ("      pragma Volatile (Ensure_Reference);");
         WBI ("");
      end if;

      WBI ("   begin");

      --  Acquire command line arguments if present on target

      if CodePeer_Mode then
         null;

      elsif Command_Line_Args_On_Target then
         WBI ("      gnat_argc := argc;");
         WBI ("      gnat_argv := argv;");
         WBI ("      gnat_envp := envp;");
         WBI ("");

      --  If configurable run time and no command line args, then nothing needs
      --  to be done since the gnat_argc/argv/envp variables are suppressed in
      --  this case.

      elsif Configurable_Run_Time_On_Target then
         null;

      --  Otherwise set dummy values (to be filled in by some other unit?)

      else
         WBI ("      gnat_argc := 0;");
         WBI ("      gnat_argv := System.Null_Address;");
         WBI ("      gnat_envp := System.Null_Address;");
      end if;

      if Opt.Default_Exit_Status /= 0
        and then Bind_Main_Program
        and then not Configurable_Run_Time_Mode
      then
         Set_String ("      Set_Exit_Status (");
         Set_Int (Opt.Default_Exit_Status);
         Set_String (");");
         Write_Statement_Buffer;
      end if;

      if Dynamic_Stack_Measurement then
         Set_String ("      Initialize_Stack_Analysis (");
         Set_Int (Dynamic_Stack_Measurement_Array_Size);
         Set_String (");");
         Write_Statement_Buffer;
      end if;

      if not Cumulative_Restrictions.Set (No_Finalization)
        and then not CodePeer_Mode
      then
         if not No_Main_Subprogram
           and then Bind_Main_Program
           and then not Suppress_Standard_Library_On_Target
         then
            WBI ("      Initialize (SEH'Address);");
         else
            WBI ("      Initialize (System.Null_Address);");
         end if;
      end if;

      WBI ("      " & Ada_Init_Name.all & ";");

      if not No_Main_Subprogram then
         if CodePeer_Mode then
            if ALIs.Table (ALIs.First).Main_Program = Proc then
               WBI ("      " & CodePeer_Wrapper_Name & ";");
            else
               WBI ("      Result := " & CodePeer_Wrapper_Name & ";");
            end if;

         elsif ALIs.Table (ALIs.First).Main_Program = Proc then
            WBI ("      Ada_Main_Program;");

         else
            WBI ("      Result := Ada_Main_Program;");
         end if;
      end if;

      --  Adafinal call is skipped if no finalization

      if not Cumulative_Restrictions.Set (No_Finalization) then
         WBI ("      adafinal;");
      end if;

      --  Prints the result of static stack analysis

      if Dynamic_Stack_Measurement then
         WBI ("      Output_Results;");
      end if;

      --  Finalize is only called if we have a run time

      if not Cumulative_Restrictions.Set (No_Finalization)
        and then not CodePeer_Mode
      then
         WBI ("      Finalize;");
      end if;

      --  Return result

      if Exit_Status_Supported_On_Target then
         if No_Main_Subprogram
           or else ALIs.Table (ALIs.First).Main_Program = Proc
         then
            WBI ("      return (gnat_exit_status);");
         else
            WBI ("      return (Result);");
         end if;
      end if;

      WBI ("   end;");
      WBI ("");
   end Gen_Main;

   ------------------------------
   -- Gen_Object_Files_Options --
   ------------------------------

   procedure Gen_Object_Files_Options (Elab_Order : Unit_Id_Array) is
      Lgnat : Natural;
      --  This keeps track of the position in the sorted set of entries in the
      --  Linker_Options table of where the first entry from an internal file
      --  appears.

      Linker_Option_List_Started : Boolean := False;
      --  Set to True when "LINKER OPTION LIST" is displayed

      procedure Write_Linker_Option;
      --  Write binder info linker option

      -------------------------
      -- Write_Linker_Option --
      -------------------------

      procedure Write_Linker_Option is
         Start : Natural;
         Stop  : Natural;

      begin
         --  Loop through string, breaking at null's

         Start := 1;
         while Start < Name_Len loop

            --  Find null ending this section

            Stop := Start + 1;
            while Name_Buffer (Stop) /= ASCII.NUL
              and then Stop <= Name_Len loop
               Stop := Stop + 1;
            end loop;

            --  Process section if non-null

            if Stop > Start then
               if Output_Linker_Option_List then
                  if not Zero_Formatting then
                     if not Linker_Option_List_Started then
                        Linker_Option_List_Started := True;
                        Write_Eol;
                        Write_Str ("     LINKER OPTION LIST");
                        Write_Eol;
                        Write_Eol;
                     end if;

                     Write_Str ("   ");
                  end if;

                  Write_Str (Name_Buffer (Start .. Stop - 1));
                  Write_Eol;
               end if;
               WBI ("   --   " & Name_Buffer (Start .. Stop - 1));
            end if;

            Start := Stop + 1;
         end loop;
      end Write_Linker_Option;

   --  Start of processing for Gen_Object_Files_Options

   begin
      WBI ("--  BEGIN Object file/option list");

      if Object_List_Filename /= null then
         Set_List_File (Object_List_Filename.all);
      end if;

      for E in Elab_Order'Range loop

         --  If not spec that has an associated body, then generate a comment
         --  giving the name of the corresponding object file.

         if not Units.Table (Elab_Order (E)).SAL_Interface
           and then Units.Table (Elab_Order (E)).Utype /= Is_Spec
         then
            Get_Name_String
              (ALIs.Table
                (Units.Table (Elab_Order (E)).My_ALI).Ofile_Full_Name);

            --  If the presence of an object file is necessary or if it exists,
            --  then use it.

            if not Hostparm.Exclude_Missing_Objects
              or else
                System.OS_Lib.Is_Regular_File (Name_Buffer (1 .. Name_Len))
            then
               WBI ("   --   " & Name_Buffer (1 .. Name_Len));

               if Output_Object_List then
                  Write_Str (Name_Buffer (1 .. Name_Len));
                  Write_Eol;
               end if;
            end if;
         end if;
      end loop;

      if Object_List_Filename /= null then
         Close_List_File;
      end if;

      --  Add a "-Ldir" for each directory in the object path

      for J in 1 .. Nb_Dir_In_Obj_Search_Path loop
         declare
            Dir : constant String_Ptr := Dir_In_Obj_Search_Path (J);

         begin
            Name_Len := 0;
            Add_Str_To_Name_Buffer ("-L");
            Add_Str_To_Name_Buffer (Dir.all);
            Write_Linker_Option;
         end;
      end loop;

      if not (Opt.No_Run_Time_Mode or Opt.No_Stdlib) then
         Name_Len := 0;

         if Opt.Shared_Libgnat then
            Add_Str_To_Name_Buffer ("-shared");
         else
            Add_Str_To_Name_Buffer ("-static");
         end if;

         --  Write directly to avoid inclusion in -K output as -static and
         --  -shared are not usually specified linker options.

         WBI ("   --   " & Name_Buffer (1 .. Name_Len));
      end if;

      --  Sort linker options

      --  This sort accomplishes two important purposes:

      --    a) All application files are sorted to the front, and all GNAT
      --       internal files are sorted to the end. This results in a well
      --       defined dividing line between the two sets of files, for the
      --       purpose of inserting certain standard library references into
      --       the linker arguments list.

      --    b) Given two different units, we sort the linker options so that
      --       those from a unit earlier in the elaboration order comes later
      --       in the list. This is a heuristic designed to create a more
      --       friendly order of linker options when the operations appear in
      --       separate units. The idea is that if unit A must be elaborated
      --       before unit B, then it is more likely that B references
      --       libraries included by A, than vice versa, so we want libraries
      --       included by A to come after libraries included by B.

      --  These two criteria are implemented by function Lt_Linker_Option. Note
      --  that a special case of b) is that specs are elaborated before bodies,
      --  so linker options from specs come after linker options for bodies,
      --  and again, the assumption is that libraries used by the body are more
      --  likely to reference libraries used by the spec, than vice versa.

      Sort
        (Linker_Options.Last,
         Move_Linker_Option'Access,
         Lt_Linker_Option'Access);

      --  Write user linker options, i.e. the set of linker options that come
      --  from all files other than GNAT internal files, Lgnat is left set to
      --  point to the first entry from a GNAT internal file, or past the end
      --  of the entries if there are no internal files.

      Lgnat := Linker_Options.Last + 1;

      for J in 1 .. Linker_Options.Last loop
         if not Linker_Options.Table (J).Internal_File then
            Get_Name_String (Linker_Options.Table (J).Name);
            Write_Linker_Option;
         else
            Lgnat := J;
            exit;
         end if;
      end loop;

      --  Now we insert standard linker options that must appear after the
      --  entries from user files, and before the entries from GNAT run-time
      --  files. The reason for this decision is that libraries referenced
      --  by internal routines may reference these standard library entries.

      --  Note that we do not insert anything when pragma No_Run_Time has
      --  been specified or when the standard libraries are not to be used,
      --  otherwise on some platforms, we may get duplicate symbols when
      --  linking (not clear if this is still the case, but it is harmless).

      if not (Opt.No_Run_Time_Mode or else Opt.No_Stdlib) then
         if With_GNARL then
            Name_Len := 0;

            if Opt.Shared_Libgnat then
               Add_Str_To_Name_Buffer (Shared_Lib ("gnarl"));
            else
               Add_Str_To_Name_Buffer ("-lgnarl");
            end if;

            Write_Linker_Option;
         end if;

         Name_Len := 0;

         if Opt.Shared_Libgnat then
            Add_Str_To_Name_Buffer (Shared_Lib ("gnat"));
         else
            Add_Str_To_Name_Buffer ("-lgnat");
         end if;

         Write_Linker_Option;
      end if;

      --  Write linker options from all internal files

      for J in Lgnat .. Linker_Options.Last loop
         Get_Name_String (Linker_Options.Table (J).Name);
         Write_Linker_Option;
      end loop;

      if Output_Linker_Option_List and then not Zero_Formatting then
         Write_Eol;
      end if;

      WBI ("--  END Object file/option list   ");
   end Gen_Object_Files_Options;

   ---------------------
   -- Gen_Output_File --
   ---------------------

   procedure Gen_Output_File
     (Filename   : String;
      Elab_Order : Unit_Id_Array)
   is
   begin
      --  Acquire settings for Interrupt_State pragmas

      Set_IS_Pragma_Table;

      --  Acquire settings for Priority_Specific_Dispatching pragma

      Set_PSD_Pragma_Table;

      --  Override time slice value if -T switch is set

      if Time_Slice_Set then
         ALIs.Table (ALIs.First).Time_Slice_Value := Opt.Time_Slice_Value;
      end if;

      --  Count number of elaboration calls

      for E in Elab_Order'Range loop
         if Units.Table (Elab_Order (E)).No_Elab then
            null;
         else
            Num_Elab_Calls := Num_Elab_Calls + 1;
         end if;
      end loop;

      --  Count the number of statically allocated stacks to be generated by
      --  the binder. If the user has specified the number of default-sized
      --  secondary stacks, use that number. Otherwise start the count at one
      --  as the binder is responsible for creating a secondary stack for the
      --  main task.

      if Opt.Quantity_Of_Default_Size_Sec_Stacks /= -1 then
         Num_Sec_Stacks := Quantity_Of_Default_Size_Sec_Stacks;
      elsif Sec_Stack_Used then
         Num_Sec_Stacks := 1;
      end if;

      for J in Units.First .. Units.Last loop
         Num_Primary_Stacks :=
           Num_Primary_Stacks + Units.Table (J).Primary_Stack_Count;

         Num_Sec_Stacks :=
           Num_Sec_Stacks + Units.Table (J).Sec_Stack_Count;
      end loop;

      --  Generate output file in appropriate language

      Gen_Output_File_Ada (Filename, Elab_Order);
   end Gen_Output_File;

   -------------------------
   -- Gen_Output_File_Ada --
   -------------------------

   procedure Gen_Output_File_Ada
     (Filename : String; Elab_Order : Unit_Id_Array)
   is
      Ada_Main : constant String := Get_Ada_Main_Name;
      --  Name to be used for generated Ada main program. See the body of
      --  function Get_Ada_Main_Name for details on the form of the name.

      Needs_Library_Finalization : constant Boolean :=
        not Configurable_Run_Time_On_Target
        and then Has_Finalizer (Elab_Order);
      --  For restricted run-time libraries (ZFP and Ravenscar) tasks are
      --  non-terminating, so we do not want finalization.

      Bfiles : Name_Id;
      --  Name of generated bind file (spec)

      Bfileb : Name_Id;
      --  Name of generated bind file (body)

   begin
      --  Create spec first

      Create_Binder_Output (Filename, 's', Bfiles);

      --  We always compile the binder file in Ada 95 mode so that we properly
      --  handle use of Ada 2005 keywords as identifiers in Ada 95 mode. None
      --  of the Ada 2005 or Ada 2012 constructs are needed by the binder file.

      WBI ("pragma Warnings (Off);");
      WBI ("pragma Ada_95;");

      --  If we are operating in Restrictions (No_Exception_Handlers) mode,
      --  then we need to make sure that the binder program is compiled with
      --  the same restriction, so that no exception tables are generated.

      if Cumulative_Restrictions.Set (No_Exception_Handlers) then
         WBI ("pragma Restrictions (No_Exception_Handlers);");
      end if;

      --  Same processing for Restrictions (No_Exception_Propagation)

      if Cumulative_Restrictions.Set (No_Exception_Propagation) then
         WBI ("pragma Restrictions (No_Exception_Propagation);");
      end if;

      --  Same processing for pragma No_Run_Time

      if No_Run_Time_Mode then
         WBI ("pragma No_Run_Time;");
      end if;

      --  Generate with of System so we can reference System.Address

      WBI ("with System;");

      --  Generate with of System.Initialize_Scalars if active

      if Initialize_Scalars_Used then
         WBI ("with System.Scalar_Values;");
      end if;

      --  Generate withs of System.Secondary_Stack and System.Parameters to
      --  allow the generation of the default-sized secondary stack pool.

      if Sec_Stack_Used then
         WBI ("with System.Parameters;");
         WBI ("with System.Secondary_Stack;");
      end if;

      Resolve_Binder_Options (Elab_Order);

      --  Generate standard with's

      if not Suppress_Standard_Library_On_Target then
         if CodePeer_Mode then
            WBI ("with System.Standard_Library;");
         end if;
      end if;

      WBI ("package " & Ada_Main & " is");

      --  Main program case

      if Bind_Main_Program then
         --  Generate argc/argv stuff unless suppressed

         if Command_Line_Args_On_Target
           or not Configurable_Run_Time_On_Target
         then
            WBI ("");
            WBI ("   gnat_argc : Integer;");
            WBI ("   gnat_argv : System.Address;");
            WBI ("   gnat_envp : System.Address;");

            --  If the standard library is not suppressed, these variables
            --  are in the run-time data area for easy run time access.

            if not Suppress_Standard_Library_On_Target then
               WBI ("");
               WBI ("   pragma Import (C, gnat_argc);");
               WBI ("   pragma Import (C, gnat_argv);");
               WBI ("   pragma Import (C, gnat_envp);");
            end if;
         end if;

         --  Define exit status. Again in normal mode, this is in the run-time
         --  library, and is initialized there, but in the configurable
         --  run-time case, the variable is declared and initialized in this
         --  file.

         WBI ("");

         if Configurable_Run_Time_Mode then
            if Exit_Status_Supported_On_Target then
               WBI ("   gnat_exit_status : Integer := 0;");
            end if;

         else
            WBI ("   gnat_exit_status : Integer;");
            WBI ("   pragma Import (C, gnat_exit_status);");
         end if;

         --  Generate the GNAT_Version and Ada_Main_Program_Name info only for
         --  the main program. Otherwise, it can lead under some circumstances
         --  to a symbol duplication during the link (for instance when a C
         --  program uses two Ada libraries). Also zero terminate the string
         --  so that its end can be found reliably at run time.

         WBI ("");
         WBI ("   GNAT_Version : constant String :=");
         WBI ("                    """ & Ver_Prefix &
                                   Gnat_Version_String &
                                   """ & ASCII.NUL;");
         WBI ("   pragma Export (C, GNAT_Version, ""__gnat_version"");");

         WBI ("");
         Set_String ("   Ada_Main_Program_Name : constant String := """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         Set_Main_Program_Name;
         Set_String (""" & ASCII.NUL;");

         Write_Statement_Buffer;

         WBI
           ("   pragma Export (C, Ada_Main_Program_Name, " &
            """__gnat_ada_main_program_name"");");
      end if;

      WBI ("");
      WBI ("   procedure " & Ada_Init_Name.all & ";");
      WBI ("   pragma Export (C, " & Ada_Init_Name.all & ", """ &
           Ada_Init_Name.all & """);");

      --  If -a has been specified use pragma Linker_Constructor for the init
      --  procedure and pragma Linker_Destructor for the final procedure.

      if Use_Pragma_Linker_Constructor then
         WBI ("   pragma Linker_Constructor (" & Ada_Init_Name.all & ");");
      end if;

      if not Cumulative_Restrictions.Set (No_Finalization) then
         WBI ("");
         WBI ("   procedure " & Ada_Final_Name.all & ";");
         WBI ("   pragma Export (C, " & Ada_Final_Name.all & ", """ &
              Ada_Final_Name.all & """);");

         if Use_Pragma_Linker_Constructor then
            WBI ("   pragma Linker_Destructor (" & Ada_Final_Name.all & ");");
         end if;
      end if;

      if Bind_Main_Program then

         WBI ("");

         if Exit_Status_Supported_On_Target then
            Set_String ("   function ");
         else
            Set_String ("   procedure ");
         end if;

         Set_String (Get_Main_Name);

         --  Generate argument list if present

         if Command_Line_Args_On_Target then
            Write_Statement_Buffer;
            WBI ("     (argc : Integer;");
            WBI ("      argv : System.Address;");
            Set_String
                ("      envp : System.Address)");

            if Exit_Status_Supported_On_Target then
               Write_Statement_Buffer;
               WBI ("      return Integer;");
            else
               Write_Statement_Buffer (";");
            end if;

         else
            if Exit_Status_Supported_On_Target then
               Write_Statement_Buffer (" return Integer;");
            else
               Write_Statement_Buffer (";");
            end if;
         end if;

         WBI ("   pragma Export (C, " & Get_Main_Name & ", """ &
           Get_Main_Name & """);");
      end if;

      --  Generate version numbers for units, only if needed. Be very safe on
      --  the condition.

      if not Configurable_Run_Time_On_Target
        or else System_Version_Control_Used
        or else not Bind_Main_Program
      then
         Gen_Versions;
      end if;

      Gen_Elab_Order (Elab_Order);

      --  Spec is complete

      WBI ("");
      WBI ("end " & Ada_Main & ";");
      Close_Binder_Output;

      --  Prepare to write body

      Create_Binder_Output (Filename, 'b', Bfileb);

      --  We always compile the binder file in Ada 95 mode so that we properly
      --  handle use of Ada 2005 keywords as identifiers in Ada 95 mode. None
      --  of the Ada 2005/2012 constructs are needed by the binder file.

      WBI ("pragma Warnings (Off);");
      WBI ("pragma Ada_95;");

      --  Output Source_File_Name pragmas which look like

      --    pragma Source_File_Name (Ada_Main, Spec_File_Name => "sss");
      --    pragma Source_File_Name (Ada_Main, Body_File_Name => "bbb");

      --  where sss/bbb are the spec/body file names respectively

      Get_Name_String (Bfiles);
      Name_Buffer (Name_Len + 1 .. Name_Len + 3) := """);";

      WBI ("pragma Source_File_Name (" &
           Ada_Main &
           ", Spec_File_Name => """ &
           Name_Buffer (1 .. Name_Len + 3));

      Get_Name_String (Bfileb);
      Name_Buffer (Name_Len + 1 .. Name_Len + 3) := """);";

      WBI ("pragma Source_File_Name (" &
           Ada_Main &
           ", Body_File_Name => """ &
           Name_Buffer (1 .. Name_Len + 3));

      --  Generate pragma Suppress (Overflow_Check). This is needed for recent
      --  versions of the compiler which have overflow checks on by default.
      --  We do not want overflow checking enabled for the increments of the
      --  elaboration variables (since this can cause an unwanted reference to
      --  the last chance exception handler for limited run-times).

      WBI ("pragma Suppress (Overflow_Check);");

      --  Generate with of System.Restrictions to initialize
      --  Run_Time_Restrictions.

      if System_Restrictions_Used
        and not Suppress_Standard_Library_On_Target
      then
         WBI ("");
         WBI ("with System.Restrictions;");
      end if;

      --  Generate with of Ada.Exceptions if needs library finalization

      if Needs_Library_Finalization then
         WBI ("with Ada.Exceptions;");
      end if;

      --  Generate with of System.Elaboration_Allocators if the restriction
      --  No_Standard_Allocators_After_Elaboration was present.

      if Cumulative_Restrictions.Set
           (No_Standard_Allocators_After_Elaboration)
      then
         WBI ("with System.Elaboration_Allocators;");
      end if;

      --  Generate start of package body

      WBI ("");
      WBI ("package body " & Ada_Main & " is");
      WBI ("");

      --  Generate externals for elaboration entities

      Gen_Elab_Externals (Elab_Order);

      --  Generate default-sized secondary stacks pool. At least one stack is
      --  created and assigned to the environment task if secondary stacks are
      --  used by the program.

      if Sec_Stack_Used then
         Set_String ("   Sec_Default_Sized_Stacks");
         Set_String (" : array (1 .. ");
         Set_Int (Num_Sec_Stacks);
         Set_String (") of aliased System.Secondary_Stack.SS_Stack (");

         if Opt.Default_Sec_Stack_Size /= No_Stack_Size then
            Set_Int (Opt.Default_Sec_Stack_Size);
         else
            Set_String ("System.Parameters.Runtime_Default_Sec_Stack_Size");
         end if;

         Set_String (");");
         Write_Statement_Buffer;
         WBI ("");
      end if;

      --  Generate reference

      if not CodePeer_Mode then
         if not Suppress_Standard_Library_On_Target then

            --  Generate Priority_Specific_Dispatching pragma string

            Set_String
              ("   Local_Priority_Specific_Dispatching : " &
               "constant String := """);

            for J in 0 .. PSD_Pragma_Settings.Last loop
               Set_Char (PSD_Pragma_Settings.Table (J));
            end loop;

            Set_String (""";");
            Write_Statement_Buffer;

            --  Generate Interrupt_State pragma string

            Set_String ("   Local_Interrupt_States : constant String := """);

            for J in 0 .. IS_Pragma_Settings.Last loop
               Set_Char (IS_Pragma_Settings.Table (J));
            end loop;

            Set_String (""";");
            Write_Statement_Buffer;
            WBI ("");
         end if;

         if not Suppress_Standard_Library_On_Target then

            --  The B.1(39) implementation advice says that the adainit and
            --  adafinal routines should be idempotent. Generate a flag to
            --  ensure that. This is not needed if we are suppressing the
            --  standard library since it would never be referenced.

            WBI ("   Is_Elaborated : Boolean := False;");

            --  Generate bind environment string

            Gen_Bind_Env_String;
         end if;

         WBI ("");
      end if;

      --  Generate the adafinal routine unless there is no finalization to do

      if not Cumulative_Restrictions.Set (No_Finalization) then
         if Needs_Library_Finalization then
            Gen_Finalize_Library (Elab_Order);
         end if;

         Gen_Adafinal;
      end if;

      Gen_Adainit (Elab_Order);

      if Bind_Main_Program then
         Gen_Main;
      end if;

      --  Output object file list and the Ada body is complete

      Gen_Object_Files_Options (Elab_Order);

      WBI ("");
      WBI ("end " & Ada_Main & ";");

      Close_Binder_Output;
   end Gen_Output_File_Ada;

   ----------------------
   -- Gen_Restrictions --
   ----------------------

   procedure Gen_Restrictions is
      Count : Integer;

   begin
      if Suppress_Standard_Library_On_Target
        or not System_Restrictions_Used
      then
         return;
      end if;

      WBI ("      System.Restrictions.Run_Time_Restrictions :=");
      WBI ("        (Set =>");
      Set_String      ("          (");

      Count := 0;

      for J in Cumulative_Restrictions.Set'Range loop
         Set_Boolean (Cumulative_Restrictions.Set (J));
         Set_String (", ");
         Count := Count + 1;

         if J /= Cumulative_Restrictions.Set'Last and then Count = 8 then
            Write_Statement_Buffer;
            Set_String ("           ");
            Count := 0;
         end if;
      end loop;

      Set_String_Replace ("),");
      Write_Statement_Buffer;
      Set_String ("         Value => (");

      for J in Cumulative_Restrictions.Value'Range loop
         Set_Int (Int (Cumulative_Restrictions.Value (J)));
         Set_String (", ");
      end loop;

      Set_String_Replace ("),");
      Write_Statement_Buffer;
      WBI ("         Violated =>");
      Set_String ("          (");
      Count := 0;

      for J in Cumulative_Restrictions.Violated'Range loop
         Set_Boolean (Cumulative_Restrictions.Violated (J));
         Set_String (", ");
         Count := Count + 1;

         if J /= Cumulative_Restrictions.Set'Last and then Count = 8 then
            Write_Statement_Buffer;
            Set_String ("           ");
            Count := 0;
         end if;
      end loop;

      Set_String_Replace ("),");
      Write_Statement_Buffer;
      Set_String ("         Count => (");

      for J in Cumulative_Restrictions.Count'Range loop
         Set_Int (Int (Cumulative_Restrictions.Count (J)));
         Set_String (", ");
      end loop;

      Set_String_Replace ("),");
      Write_Statement_Buffer;
      Set_String ("         Unknown => (");

      for J in Cumulative_Restrictions.Unknown'Range loop
         Set_Boolean (Cumulative_Restrictions.Unknown (J));
         Set_String (", ");
      end loop;

      Set_String_Replace ("))");
      Set_String (";");
      Write_Statement_Buffer;
   end Gen_Restrictions;

   ------------------
   -- Gen_Versions --
   ------------------

   --  This routine generates lines such as:

   --    unnnnn : constant Integer := 16#hhhhhhhh#;
   --    pragma Export (C, unnnnn, unam);

   --  for each unit, where unam is the unit name suffixed by either B or S for
   --  body or spec, with dots replaced by double underscores, and hhhhhhhh is
   --  the version number, and nnnnn is a 5-digits serial number.

   procedure Gen_Versions is
      Ubuf : String (1 .. 6) := "u00000";

      procedure Increment_Ubuf;
      --  Little procedure to increment the serial number

      --------------------
      -- Increment_Ubuf --
      --------------------

      procedure Increment_Ubuf is
      begin
         for J in reverse Ubuf'Range loop
            Ubuf (J) := Character'Succ (Ubuf (J));
            exit when Ubuf (J) <= '9';
            Ubuf (J) := '0';
         end loop;
      end Increment_Ubuf;

   --  Start of processing for Gen_Versions

   begin
      WBI ("");

      WBI ("   type Version_32 is mod 2 ** 32;");
      for U in Units.First .. Units.Last loop
         if not Units.Table (U).SAL_Interface
           and then (not Bind_For_Library
                      or else Units.Table (U).Directly_Scanned)
         then
            Increment_Ubuf;
            WBI ("   " & Ubuf & " : constant Version_32 := 16#" &
                 Units.Table (U).Version & "#;");
            Set_String ("   pragma Export (C, ");
            Set_String (Ubuf);
            Set_String (", """);

            Get_Name_String (Units.Table (U).Uname);

            for K in 1 .. Name_Len loop
               if Name_Buffer (K) = '.' then
                  Set_Char ('_');
                  Set_Char ('_');

               elsif Name_Buffer (K) = '%' then
                  exit;

               else
                  Set_Char (Name_Buffer (K));
               end if;
            end loop;

            if Name_Buffer (Name_Len) = 's' then
               Set_Char ('S');
            else
               Set_Char ('B');
            end if;

            Set_String (""");");
            Write_Statement_Buffer;
         end if;
      end loop;
   end Gen_Versions;

   ------------------------
   -- Get_Main_Unit_Name --
   ------------------------

   function Get_Main_Unit_Name (S : String) return String is
      Result : String := S;

   begin
      for J in S'Range loop
         if Result (J) = '.' then
            Result (J) := '_';
         end if;
      end loop;

      return Result;
   end Get_Main_Unit_Name;

   -----------------------
   -- Get_Ada_Main_Name --
   -----------------------

   function Get_Ada_Main_Name return String is
      Suffix : constant String := "_00";
      Name   : String (1 .. Opt.Ada_Main_Name.all'Length + Suffix'Length) :=
                 Opt.Ada_Main_Name.all & Suffix;
      Nlen   : Natural;

   begin
      --  For CodePeer, we want reproducible names (independent of other mains
      --  that may or may not be present) that don't collide when analyzing
      --  multiple mains and which are easily recognizable as "ada_main" names.

      if CodePeer_Mode then
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         return
           "ada_main_for_" &
             Get_Main_Unit_Name (Name_Buffer (1 .. Name_Len - 2));
      end if;

      --  This loop tries the following possibilities in order
      --    <Ada_Main>
      --    <Ada_Main>_01
      --    <Ada_Main>_02
      --    ..
      --    <Ada_Main>_99
      --  where <Ada_Main> is equal to Opt.Ada_Main_Name. By default,
      --  it is set to 'ada_main'.

      for J in 0 .. 99 loop
         if J = 0 then
            Nlen := Name'Length - Suffix'Length;
         else
            Nlen := Name'Length;
            Name (Name'Last) := Character'Val (J mod 10 + Character'Pos ('0'));
            Name (Name'Last - 1) :=
              Character'Val (J /   10 + Character'Pos ('0'));
         end if;

         for K in ALIs.First .. ALIs.Last loop
            for L in ALIs.Table (K).First_Unit .. ALIs.Table (K).Last_Unit loop

               --  Get unit name, removing %b or %e at end

               Get_Name_String (Units.Table (L).Uname);
               Name_Len := Name_Len - 2;

               if Name_Buffer (1 .. Name_Len) = Name (1 .. Nlen) then
                  goto Continue;
               end if;
            end loop;
         end loop;

         return Name (1 .. Nlen);

      <<Continue>>
         null;
      end loop;

      --  If we fall through, just use a peculiar unlikely name

      return ("Qwertyuiop");
   end Get_Ada_Main_Name;

   -------------------
   -- Get_Main_Name --
   -------------------

   function Get_Main_Name return String is
   begin
      --  Explicit name given with -M switch

      if Bind_Alternate_Main_Name then
         return Alternate_Main_Name.all;

      --  Case of main program name to be used directly

      elsif Use_Ada_Main_Program_Name_On_Target then

         --  Get main program name

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         --  If this is a child name, return only the name of the child, since
         --  we can't have dots in a nested program name. Note that we do not
         --  include the %b at the end of the unit name.

         for J in reverse 1 .. Name_Len - 2 loop
            if J = 1 or else Name_Buffer (J - 1) = '.' then
               return Name_Buffer (J .. Name_Len - 2);
            end if;
         end loop;

         raise Program_Error; -- impossible exit

      --  Case where "main" is to be used as default

      else
         return "main";
      end if;
   end Get_Main_Name;

   ---------------------
   -- Get_WC_Encoding --
   ---------------------

   function Get_WC_Encoding return Character is
   begin
      --  If encoding method specified by -W switch, then return it

      if Wide_Character_Encoding_Method_Specified then
         return WC_Encoding_Letters (Wide_Character_Encoding_Method);

      --  If no main program, and not specified, set brackets, we really have
      --  no better choice. If some other encoding is required when there is
      --  no main, it must be set explicitly using -Wx.

      --  Note: if the ALI file always passed the wide character encoding of
      --  every file, then we could use the encoding of the initial specified
      --  file, but this information is passed only for potential main
      --  programs. We could fix this sometime, but it is a very minor point
      --  (wide character default encoding for [Wide_[Wide_]]Text_IO when there
      --  is no main program).

      elsif No_Main_Subprogram then
         return 'b';

      --  Otherwise if there is a main program, take encoding from it

      else
         return ALIs.Table (ALIs.First).WC_Encoding;
      end if;
   end Get_WC_Encoding;

   -------------------
   -- Has_Finalizer --
   -------------------

   function Has_Finalizer (Elab_Order : Unit_Id_Array) return Boolean is
      U     : Unit_Record;
      Unum  : Unit_Id;

   begin
      for E in reverse Elab_Order'Range loop
         Unum := Elab_Order (E);
         U    := Units.Table (Unum);

         --  We are only interested in non-generic packages

         if U.Unit_Kind = 'p'
           and then U.Has_Finalizer
           and then not U.Is_Generic
           and then not U.No_Elab
         then
            return True;
         end if;
      end loop;

      return False;
   end Has_Finalizer;

   ----------
   -- Hash --
   ----------

   function Hash (Nam : Name_Id) return Header_Num is
   begin
      return Int (Nam - Names_Low_Bound) rem Header_Num'Last;
   end Hash;

   ----------------------
   -- Lt_Linker_Option --
   ----------------------

   function Lt_Linker_Option (Op1 : Natural; Op2 : Natural) return Boolean is
   begin
      --  Sort internal files last

      if Linker_Options.Table (Op1).Internal_File
           /=
         Linker_Options.Table (Op2).Internal_File
      then
         --  Note: following test uses False < True

         return Linker_Options.Table (Op1).Internal_File
                  <
                Linker_Options.Table (Op2).Internal_File;

      --  If both internal or both non-internal, sort according to the
      --  elaboration position. A unit that is elaborated later should come
      --  earlier in the linker options list.

      else
         return Units.Table (Linker_Options.Table (Op1).Unit).Elab_Position
                  >
                Units.Table (Linker_Options.Table (Op2).Unit).Elab_Position;
      end if;
   end Lt_Linker_Option;

   ------------------------
   -- Move_Linker_Option --
   ------------------------

   procedure Move_Linker_Option (From : Natural; To : Natural) is
   begin
      Linker_Options.Table (To) := Linker_Options.Table (From);
   end Move_Linker_Option;

   ----------------------------
   -- Resolve_Binder_Options --
   ----------------------------

   procedure Resolve_Binder_Options (Elab_Order : Unit_Id_Array) is
      procedure Check_Package (Var : in out Boolean; Name : String);
      --  Set Var to true iff the current identifier in Namet is Name. Do
      --  nothing if it doesn't match. This procedure is just a helper to
      --  avoid explicitly dealing with length.

      -------------------
      -- Check_Package --
      -------------------

      procedure Check_Package (Var : in out Boolean; Name : String) is
      begin
         if Name_Len = Name'Length
           and then Name_Buffer (1 .. Name_Len) = Name
         then
            Var := True;
         end if;
      end Check_Package;

   --  Start of processing for Resolve_Binder_Options

   begin
      for E in Elab_Order'Range loop
         Get_Name_String (Units.Table (Elab_Order (E)).Uname);

         --  This is not a perfect approach, but is the current protocol
         --  between the run-time and the binder to indicate that tasking is
         --  used: System.OS_Interface should always be used by any tasking
         --  application.

         Check_Package (With_GNARL, "system.os_interface%s");

         --  Ditto for the use of restricted tasking

         Check_Package
           (System_Tasking_Restricted_Stages_Used,
            "system.tasking.restricted.stages%s");

         --  Ditto for the use of interrupts

         Check_Package (System_Interrupts_Used, "system.interrupts%s");

         --  Ditto for the use of dispatching domains

         Check_Package
           (Dispatching_Domains_Used,
            "system.multiprocessors.dispatching_domains%s");

         --  Ditto for the use of restrictions

         Check_Package (System_Restrictions_Used, "system.restrictions%s");

         --  Ditto for the use of System.Secondary_Stack

         Check_Package
           (System_Secondary_Stack_Used, "system.secondary_stack%s");

         --  Ditto for use of an SMP bareboard runtime

         Check_Package (System_BB_CPU_Primitives_Multiprocessors_Used,
                        "system.bb.cpu_primitives.multiprocessors%s");

         --  Ditto for System.Version_Control, which is used for Version and
         --  Body_Version attributes.

         Check_Package (System_Version_Control_Used,
                        "system.version_control%s");
      end loop;
   end Resolve_Binder_Options;

   ------------------
   -- Set_Bind_Env --
   ------------------

   procedure Set_Bind_Env (Key, Value : String) is
   begin
      --  The lengths of Key and Value are stored as single bytes

      if Key'Length > 255 then
         Osint.Fail ("bind environment key """ & Key & """ too long");
      end if;

      if Value'Length > 255 then
         Osint.Fail ("bind environment value """ & Value & """ too long");
      end if;

      Bind_Environment.Set (Name_Find (Key), Name_Find (Value));
   end Set_Bind_Env;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean (B : Boolean) is
      False_Str : constant String := "False";
      True_Str  : constant String := "True";

   begin
      if B then
         Statement_Buffer (Stm_Last + 1 .. Stm_Last + True_Str'Length) :=
           True_Str;
         Stm_Last := Stm_Last + True_Str'Length;
      else
         Statement_Buffer (Stm_Last + 1 .. Stm_Last + False_Str'Length) :=
           False_Str;
         Stm_Last := Stm_Last + False_Str'Length;
      end if;
   end Set_Boolean;

   --------------
   -- Set_Char --
   --------------

   procedure Set_Char (C : Character) is
   begin
      Stm_Last := Stm_Last + 1;
      Statement_Buffer (Stm_Last) := C;
   end Set_Char;

   -------------
   -- Set_Int --
   -------------

   procedure Set_Int (N : Int) is
   begin
      if N < 0 then
         Set_String ("-");
         Set_Int (-N);

      else
         if N > 9 then
            Set_Int (N / 10);
         end if;

         Stm_Last := Stm_Last + 1;
         Statement_Buffer (Stm_Last) :=
           Character'Val (N mod 10 + Character'Pos ('0'));
      end if;
   end Set_Int;

   -------------------------
   -- Set_IS_Pragma_Table --
   -------------------------

   procedure Set_IS_Pragma_Table is
   begin
      for F in ALIs.First .. ALIs.Last loop
         for K in ALIs.Table (F).First_Interrupt_State ..
                  ALIs.Table (F).Last_Interrupt_State
         loop
            declare
               Inum : constant Int :=
                        Interrupt_States.Table (K).Interrupt_Id;
               Stat : constant Character :=
                        Interrupt_States.Table (K).Interrupt_State;

            begin
               while IS_Pragma_Settings.Last < Inum loop
                  IS_Pragma_Settings.Append ('n');
               end loop;

               IS_Pragma_Settings.Table (Inum) := Stat;
            end;
         end loop;
      end loop;
   end Set_IS_Pragma_Table;

   ---------------------------
   -- Set_Main_Program_Name --
   ---------------------------

   procedure Set_Main_Program_Name is
   begin
      --  Note that name has %b on the end which we ignore

      --  First we output the initial _ada_ since we know that the main program
      --  is a library level subprogram.

      Set_String ("_ada_");

      --  Copy name, changing dots to double underscores

      for J in 1 .. Name_Len - 2 loop
         if Name_Buffer (J) = '.' then
            Set_String ("__");
         else
            Set_Char (Name_Buffer (J));
         end if;
      end loop;
   end Set_Main_Program_Name;

   ---------------------
   -- Set_Name_Buffer --
   ---------------------

   procedure Set_Name_Buffer is
   begin
      for J in 1 .. Name_Len loop
         Set_Char (Name_Buffer (J));
      end loop;
   end Set_Name_Buffer;

   -------------------------
   -- Set_PSD_Pragma_Table --
   -------------------------

   procedure Set_PSD_Pragma_Table is
   begin
      for F in ALIs.First .. ALIs.Last loop
         for K in ALIs.Table (F).First_Specific_Dispatching ..
                  ALIs.Table (F).Last_Specific_Dispatching
         loop
            declare
               DTK : Specific_Dispatching_Record
                       renames Specific_Dispatching.Table (K);

            begin
               while PSD_Pragma_Settings.Last < DTK.Last_Priority loop
                  PSD_Pragma_Settings.Append ('F');
               end loop;

               for Prio in DTK.First_Priority .. DTK.Last_Priority loop
                  PSD_Pragma_Settings.Table (Prio) := DTK.Dispatching_Policy;
               end loop;
            end;
         end loop;
      end loop;
   end Set_PSD_Pragma_Table;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (S : String) is
   begin
      Statement_Buffer (Stm_Last + 1 .. Stm_Last + S'Length) := S;
      Stm_Last := Stm_Last + S'Length;
   end Set_String;

   ------------------------
   -- Set_String_Replace --
   ------------------------

   procedure Set_String_Replace (S : String) is
   begin
      Statement_Buffer (Stm_Last - S'Length + 1 .. Stm_Last) := S;
   end Set_String_Replace;

   -------------------
   -- Set_Unit_Name --
   -------------------

   procedure Set_Unit_Name is
   begin
      for J in 1 .. Name_Len - 2 loop
         if Name_Buffer (J) = '.' then
            Set_String ("__");
         else
            Set_Char (Name_Buffer (J));
         end if;
      end loop;
   end Set_Unit_Name;

   ---------------------
   -- Set_Unit_Number --
   ---------------------

   procedure Set_Unit_Number (U : Unit_Id) is
      Num_Units : constant Nat := Nat (Units.Last) - Nat (Unit_Id'First);
      Unum      : constant Nat := Nat (U) - Nat (Unit_Id'First);

   begin
      if Num_Units >= 10 and then Unum < 10 then
         Set_Char ('0');
      end if;

      if Num_Units >= 100 and then Unum < 100 then
         Set_Char ('0');
      end if;

      Set_Int (Unum);
   end Set_Unit_Number;

   ---------------------
   -- Write_Bind_Line --
   ---------------------

   procedure Write_Bind_Line (S : String) is
   begin
      --  Need to strip trailing LF from S

      WBI (S (S'First .. S'Last - 1));
   end Write_Bind_Line;

   ----------------------------
   -- Write_Statement_Buffer --
   ----------------------------

   procedure Write_Statement_Buffer is
   begin
      WBI (Statement_Buffer (1 .. Stm_Last));
      Stm_Last := 0;
   end Write_Statement_Buffer;

   procedure Write_Statement_Buffer (S : String) is
   begin
      Set_String (S);
      Write_Statement_Buffer;
   end Write_Statement_Buffer;

end Bindgen;
