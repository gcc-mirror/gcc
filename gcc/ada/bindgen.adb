------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D G E N                               --
--                                                                          --
--                                 B o d y                                  --
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

with ALI;      use ALI;
with Binde;    use Binde;
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
with Table;    use Table;
with Targparm; use Targparm;
with Types;    use Types;

with System.OS_Lib;  use System.OS_Lib;
with System.WCh_Con; use System.WCh_Con;

with GNAT.Heap_Sort_A; use GNAT.Heap_Sort_A;

package body Bindgen is

   Statement_Buffer : String (1 .. 1000);
   --  Buffer used for constructing output statements

   Last : Natural := 0;
   --  Last location in Statement_Buffer currently set

   With_DECGNAT : Boolean := False;
   --  Flag which indicates whether the program uses the DECGNAT library
   --  (presence of the unit DEC).

   With_GNARL : Boolean := False;
   --  Flag which indicates whether the program uses the GNARL library
   --  (presence of the unit System.OS_Interface)

   Num_Elab_Calls : Nat := 0;
   --  Number of generated calls to elaboration routines

   System_Restrictions_Used : Boolean;
   --  Flag indicating whether the unit System.Restrictions is in the closure
   --  of the partition. This is set by Check_System_Restrictions_Used, and
   --  is used to determine whether or not to initialize the restrictions
   --  information in the body of the binder generated file (we do not want
   --  to do this unconditionally, since it drags in the System.Restrictions
   --  unit unconditionally, which is unpleasand, especially for ZFP etc.)

   ----------------------------------
   -- Interface_State Pragma Table --
   ----------------------------------

   --  This table assembles the interface state pragma information from
   --  all the units in the partition. Note that Bcheck has already checked
   --  that the information is consistent across units. The entries
   --  in this table are n/u/r/s for not set/user/runtime/system.

   package IS_Pragma_Settings is new Table.Table (
     Table_Component_Type => Character,
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

   package PSD_Pragma_Settings is new Table.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "PSD_Pragma_Settings");

   ----------------------
   -- Run-Time Globals --
   ----------------------

   --  This section documents the global variables that set from the
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
   --     Zero_Cost_Exceptions          : Integer;
   --     Detect_Blocking               : Integer;
   --     Default_Stack_Size            : Integer;
   --     Leap_Seconds_Support          : Integer;
   --     Main_CPU                      : Integer;

   --  Main_Priority is the priority value set by pragma Priority in the main
   --  program. If no such pragma is present, the value is -1.

   --  Time_Slice_Value is the time slice value set by pragma Time_Slice in the
   --  main program, or by the use of a -Tnnn parameter for the binder (if both
   --  are present, the binder value overrides). The value is in milliseconds.
   --  A value of zero indicates that time slicing should be suppressed. If no
   --  pragma is present, and no -T switch was used, the value is -1.

   --  Heap_Size is the heap to use for memory allocations set by use of a
   --  -Hnn parameter for the binder or by the GNAT$NO_MALLOC_64 logical.
   --  Valid values are 32 and 64. This switch is only effective on VMS.

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

   --  Num_Specific_Dispatching is the length of the
   --  Priority_Specific_Dispatching string. It will be set to zero if no
   --  Priority_Specific_Dispatching pragmas are present.

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

   --  Exception_Tracebacks is set to one if the -E parameter was present
   --  in the bind and to zero otherwise. Note that on some targets exception
   --  tracebacks are provided by default, so a value of zero for this
   --  parameter does not necessarily mean no trace backs are available.

   --  Zero_Cost_Exceptions is set to one if zero cost exceptions are used for
   --  this partition, and to zero if longjmp/setjmp exceptions are used.

   --  Detect_Blocking indicates whether pragma Detect_Blocking is active or
   --  not. A value of zero indicates that the pragma is not present, while a
   --  value of 1 signals its presence in the partition.

   --  Default_Stack_Size is the default stack size used when creating an Ada
   --  task with no explicit Storage_Size clause.

   --  Leap_Seconds_Support denotes whether leap seconds have been enabled or
   --  disabled. A value of zero indicates that leap seconds are turned "off",
   --  while a value of one signifies "on" status.

   --  Main_CPU is the processor set by pragma CPU in the main program. If no
   --  such pragma is present, the value is -1.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure WBI (Info : String) renames Osint.B.Write_Binder_Info;
   --  Convenient shorthand used throughout

   procedure Check_System_Restrictions_Used;
   --  Sets flag System_Restrictions_Used (Set to True if and only if the unit
   --  System.Restrictions is present in the partition, otherwise False).

   procedure Gen_Adainit_Ada;
   --  Generates the Adainit procedure (Ada code case)

   procedure Gen_Adainit_C;
   --  Generates the Adainit procedure (C code case)

   procedure Gen_Adafinal_Ada;
   --  Generate the Adafinal procedure (Ada code case)

   procedure Gen_Adafinal_C;
   --  Generate the Adafinal procedure (C code case)

   procedure Gen_Elab_Calls_Ada;
   --  Generate sequence of elaboration calls (Ada code case)

   procedure Gen_Elab_Calls_C;
   --  Generate sequence of elaboration calls (C code case)

   procedure Gen_Elab_Order_Ada;
   --  Generate comments showing elaboration order chosen (Ada case)

   procedure Gen_Elab_Order_C;
   --  Generate comments showing elaboration order chosen (C case)

   procedure Gen_Elab_Defs_C;
   --  Generate sequence of definitions for elaboration routines (C code case)

   procedure Gen_Main_Ada;
   --  Generate procedure main (Ada code case)

   procedure Gen_Main_C;
   --  Generate main() procedure (C code case)

   procedure Gen_Object_Files_Options;
   --  Output comments containing a list of the full names of the object
   --  files to be linked and the list of linker options supplied by
   --  Linker_Options pragmas in the source. (C and Ada code case)

   procedure Gen_Output_File_Ada (Filename : String);
   --  Generate output file (Ada code case)

   procedure Gen_Output_File_C (Filename : String);
   --  Generate output file (C code case)

   procedure Gen_Restrictions_Ada;
   --  Generate initialization of restrictions variable (Ada code case)

   procedure Gen_Restrictions_C;
   --  Generate initialization of restrictions variable (C code case)

   procedure Gen_Versions_Ada;
   --  Output series of definitions for unit versions (Ada code case)

   procedure Gen_Versions_C;
   --  Output series of definitions for unit versions (C code case)

   function Get_Ada_Main_Name return String;
   --  This function is used in the Ada main output case to compute a usable
   --  name for the generated main program. The normal main program name is
   --  Ada_Main, but this won't work if the user has a unit with this name.
   --  This function tries Ada_Main first, and if there is such a clash, then
   --  it tries Ada_Name_01, Ada_Name_02 ... Ada_Name_99 in sequence.

   function Get_Main_Unit_Name (S : String) return String;
   --  Return the main unit name corresponding to S by replacing '.' with '_'

   function Get_Main_Name return String;
   --  This function is used in the Ada main output case to compute the
   --  correct external main program. It is "main" by default, unless the
   --  flag Use_Ada_Main_Program_Name_On_Target is set, in which case it
   --  is the name of the Ada main name without the "_ada". This default
   --  can be overridden explicitly using the -Mname binder switch.

   function Get_WC_Encoding return Character;
   --  Return wide character encoding method to set as WC_Encoding in output.
   --  If -W has been used, returns the specified encoding, otherwise returns
   --  the encoding method used for the main program source. If there is no
   --  main program source (-z switch used), returns brackets ('b').

   function Lt_Linker_Option (Op1, Op2 : Natural) return Boolean;
   --  Compare linker options, when sorting, first according to
   --  Is_Internal_File (internal files come later) and then by
   --  elaboration order position (latest to earliest).

   procedure Move_Linker_Option (From : Natural; To : Natural);
   --  Move routine for sorting linker options

   procedure Resolve_Binder_Options;
   --  Set the value of With_GNARL and With_DECGNAT. The latter only on VMS
   --  since it tests for a package named "dec" which might cause a conflict
   --  on non-VMS systems.

   procedure Set_Char (C : Character);
   --  Set given character in Statement_Buffer at the Last + 1 position
   --  and increment Last by one to reflect the stored character.

   procedure Set_Int (N : Int);
   --  Set given value in decimal in Statement_Buffer with no spaces
   --  starting at the Last + 1 position, and updating Last past the value.
   --  A minus sign is output for a negative value.

   procedure Set_Boolean (B : Boolean);
   --  Set given boolean value in Statement_Buffer at the Last + 1 position
   --  and update Last past the value.

   procedure Set_IS_Pragma_Table;
   --  Initializes contents of IS_Pragma_Settings table from ALI table

   procedure Set_Main_Program_Name;
   --  Given the main program name in Name_Buffer (length in Name_Len)
   --  generate the name of the routine to be used in the call. The name
   --  is generated starting at Last + 1, and Last is updated past it.

   procedure Set_Name_Buffer;
   --  Set the value stored in positions 1 .. Name_Len of the Name_Buffer

   procedure Set_PSD_Pragma_Table;
   --  Initializes contents of PSD_Pragma_Settings table from ALI table

   procedure Set_String (S : String);
   --  Sets characters of given string in Statement_Buffer, starting at the
   --  Last + 1 position, and updating last past the string value.

   procedure Set_String_Replace (S : String);
   --  Replaces the last S'Length characters in the Statement_Buffer with
   --  the characters of S. The caller must ensure that these characters do
   --  in fact exist in the Statement_Buffer.

   procedure Set_Unit_Name;
   --  Given a unit name in the Name_Buffer, copies it to Statement_Buffer,
   --  starting at the Last + 1 position, and updating last past the value.
   --  changing periods to double underscores, and updating Last appropriately.

   procedure Set_Unit_Number (U : Unit_Id);
   --  Sets unit number (first unit is 1, leading zeroes output to line
   --  up all output unit numbers nicely as required by the value, and
   --  by the total number of units.

   procedure Write_Info_Ada_C (Ada : String; C : String; Common : String);
   --  For C code case, write C & Common, for Ada case write Ada & Common
   --  to current binder output file using Write_Binder_Info.

   procedure Write_Statement_Buffer;
   --  Write out contents of statement buffer up to Last, and reset Last to 0

   procedure Write_Statement_Buffer (S : String);
   --  First writes its argument (using Set_String (S)), then writes out the
   --  contents of statement buffer up to Last, and reset Last to 0

   ------------------------------------
   -- Check_System_Restrictions_Used --
   ------------------------------------

   procedure Check_System_Restrictions_Used is
   begin
      for J in Units.First .. Units.Last loop
         if Get_Name_String (Units.Table (J).Sfile) = "s-restri.ads" then
            System_Restrictions_Used := True;
            return;
         end if;
      end loop;

      System_Restrictions_Used := False;
   end Check_System_Restrictions_Used;

   ----------------------
   -- Gen_Adafinal_Ada --
   ----------------------

   procedure Gen_Adafinal_Ada is
   begin
      WBI ("");
      WBI ("   procedure " & Ada_Final_Name.all & " is");
      WBI ("   begin");

      --  If compiling for the JVM, we directly call Adafinal because
      --  we don't import it via Do_Finalize (see Gen_Output_File_Ada).

      if VM_Target /= No_VM then
         WBI ("      System.Standard_Library.Adafinal;");

      --  If there is no finalization, there is nothing to do

      elsif Cumulative_Restrictions.Set (No_Finalization) then
         WBI ("      null;");
      else
         WBI ("      Do_Finalize;");
      end if;

      WBI ("   end " & Ada_Final_Name.all & ";");
   end Gen_Adafinal_Ada;

   --------------------
   -- Gen_Adafinal_C --
   --------------------

   procedure Gen_Adafinal_C is
   begin
      WBI ("void " & Ada_Final_Name.all & " (void) {");
      WBI ("   system__standard_library__adafinal ();");
      WBI ("}");
      WBI ("");
   end Gen_Adafinal_C;

   ---------------------
   -- Gen_Adainit_Ada --
   ---------------------

   procedure Gen_Adainit_Ada is
      Main_Priority : Int renames ALIs.Table (ALIs.First).Main_Priority;
      Main_CPU      : Int renames ALIs.Table (ALIs.First).Main_CPU;

   begin
      WBI ("   procedure " & Ada_Init_Name.all & " is");

      --  Generate externals for elaboration entities

      for E in Elab_Order.First .. Elab_Order.Last loop
         declare
            Unum : constant Unit_Id := Elab_Order.Table (E);
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
               Set_String ("      ");
               Set_String ("E");
               Set_Unit_Number (Unum);

               case VM_Target is
                  when No_VM | JVM_Target =>
                     Set_String (" : Boolean; pragma Import (Ada, ");
                  when CLI_Target =>
                     Set_String (" : Boolean; pragma Import (CIL, ");
               end case;

               Set_String ("E");
               Set_Unit_Number (Unum);
               Set_String (", """);
               Get_Name_String (U.Uname);

               --  In the case of JGNAT we need to emit an Import name that
               --  includes the class name (using '$' separators in the case
               --  of a child unit name).

               if VM_Target /= No_VM then
                  for J in 1 .. Name_Len - 2 loop
                     if VM_Target = CLI_Target
                       or else Name_Buffer (J) /= '.'
                     then
                        Set_Char (Name_Buffer (J));
                     else
                        Set_String ("$");
                     end if;
                  end loop;

                  if VM_Target /= CLI_Target or else U.Unit_Kind = 's' then
                     Set_String (".");
                  else
                     Set_String ("_pkg.");
                  end if;

                  --  If the unit name is very long, then split the
                  --  Import link name across lines using "&" (occurs
                  --  in some C2 tests).

                  if 2 * Name_Len + 60 > Hostparm.Max_Line_Length then
                     Set_String (""" &");
                     Write_Statement_Buffer;
                     Set_String ("         """);
                  end if;
               end if;

               Set_Unit_Name;
               Set_String ("_E"");");
               Write_Statement_Buffer;
            end if;
         end;
      end loop;

      Write_Statement_Buffer;

      --  If the standard library is suppressed, then the only global variables
      --  that might be needed (by the Ravenscar profile) are the priority and
      --  the processor for the environment task.

      if Suppress_Standard_Library_On_Target then
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

         if Main_Priority = No_Main_Priority
           and then Main_CPU = No_Main_CPU
         then
            WBI ("      null;");
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

         if Exception_Tracebacks then
            WBI ("      Exception_Tracebacks : Integer;");
            WBI ("      pragma Import (C, Exception_Tracebacks, " &
                 """__gl_exception_tracebacks"");");
         end if;

         WBI ("      Zero_Cost_Exceptions : Integer;");
         WBI ("      pragma Import (C, Zero_Cost_Exceptions, " &
              """__gl_zero_cost_exceptions"");");
         WBI ("      Detect_Blocking : Integer;");
         WBI ("      pragma Import (C, Detect_Blocking, " &
              """__gl_detect_blocking"");");
         WBI ("      Default_Stack_Size : Integer;");
         WBI ("      pragma Import (C, Default_Stack_Size, " &
              """__gl_default_stack_size"");");
         WBI ("      Leap_Seconds_Support : Integer;");
         WBI ("      pragma Import (C, Leap_Seconds_Support, " &
              """__gl_leap_seconds_support"");");

         --  Import entry point for elaboration time signal handler
         --  installation, and indication of if it's been called previously.

         WBI ("");
         WBI ("      procedure Install_Handler;");
         WBI ("      pragma Import (C, Install_Handler, " &
              """__gnat_install_handler"");");
         WBI ("");
         WBI ("      Handler_Installed : Integer;");
         WBI ("      pragma Import (C, Handler_Installed, " &
              """__gnat_handler_installed"");");

         --  Import entry point for environment feature enable/disable
         --  routine, and indication that it's been called previously.

         if OpenVMS_On_Target then
            WBI ("");
            WBI ("      procedure Set_Features;");
            WBI ("      pragma Import (C, Set_Features, " &
                 """__gnat_set_features"");");
            WBI ("");
            WBI ("      Features_Set : Integer;");
            WBI ("      pragma Import (C, Features_Set, " &
                 """__gnat_features_set"");");

            if Opt.Heap_Size /= 0 then
               WBI ("");
               WBI ("      Heap_Size : Integer;");
               WBI ("      pragma Import (C, Heap_Size, " &
                    """__gl_heap_size"");");

               Write_Statement_Buffer;
            end if;
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

         --  Special processing when main program is CIL function/procedure

         if VM_Target = CLI_Target
           and then Bind_Main_Program
           and then not No_Main_Subprogram
         then
            WBI ("");

            --  Function case, use Set_Exit_Status to report the returned
            --  status code, since that is the only mechanism available.

            if ALIs.Table (ALIs.First).Main_Program = Func then
               WBI ("      Result : Integer;");
               WBI ("      procedure Set_Exit_Status (Code : Integer);");
               WBI ("      pragma Import (C, Set_Exit_Status, " &
                    """__gnat_set_exit_status"");");
               WBI ("");
               WBI ("      function Ada_Main_Program return Integer;");

            --  Procedure case

            else
               WBI ("      procedure Ada_Main_Program;");
            end if;

            Get_Name_String (Units.Table (First_Unit_Entry).Uname);
            Name_Len := Name_Len - 2;
            WBI ("      pragma Import (CIL, Ada_Main_Program, """
                 & Name_Buffer (1 .. Name_Len) & "."
                 & Get_Main_Unit_Name (Name_Buffer (1 .. Name_Len)) & """);");
         end if;

         WBI ("   begin");

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

         Gen_Restrictions_Ada;

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

         if Exception_Tracebacks then
            WBI ("      Exception_Tracebacks := 1;");
         end if;

         Set_String ("      Zero_Cost_Exceptions := ");

         if Zero_Cost_Exceptions_Specified then
            Set_String ("1");
         else
            Set_String ("0");
         end if;

         Set_String (";");
         Write_Statement_Buffer;

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

         --  Generate call to Install_Handler

         --  In .NET, when binding with -z, we don't install the signal handler
         --  to let the caller handle the last exception handler.

         if VM_Target /= CLI_Target
           or else Bind_Main_Program
         then
            WBI ("");
            WBI ("      if Handler_Installed = 0 then");
            WBI ("         Install_Handler;");
            WBI ("      end if;");
         end if;

         --  Generate call to Set_Features

         if OpenVMS_On_Target then
            WBI ("");
            WBI ("      if Features_Set = 0 then");
            WBI ("         Set_Features;");
            WBI ("      end if;");

            --  Features_Set may twiddle the heap size according to a logical
            --  name, but the binder switch must override.

            if Opt.Heap_Size /= 0 then
               Set_String ("      Heap_Size := ");
               Set_Int (Opt.Heap_Size);
               Set_Char   (';');
               Write_Statement_Buffer;
            end if;
         end if;
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

      --  Generate assignment of default secondary stack size if set

      if Sec_Stack_Used and then Default_Sec_Stack_Size /= -1 then
         WBI ("");
         Set_String ("      System.Secondary_Stack.");
         Set_String ("Default_Secondary_Stack_Size := ");
         Set_Int (Opt.Default_Sec_Stack_Size);
         Set_Char (';');
         Write_Statement_Buffer;
      end if;

      --  Initialize stack limit variable of the environment task if the
      --  stack check method is stack limit and stack check is enabled.

      if Stack_Check_Limits_On_Target
        and then (Stack_Check_Default_On_Target or Stack_Check_Switch_Set)
      then
         WBI ("");
         WBI ("      Initialize_Stack_Limit;");
      end if;

      --  Generate elaboration calls

      WBI ("");
      Gen_Elab_Calls_Ada;

      --  Case of main program is CIL function or procedure

      if VM_Target = CLI_Target
        and then Bind_Main_Program
        and then not No_Main_Subprogram
      then
         --  For function case, use Set_Exit_Status to set result

         if ALIs.Table (ALIs.First).Main_Program = Func then
            WBI ("      Result := Ada_Main_Program;");
            WBI ("      Set_Exit_Status (Result);");

         --  Procedure case

         else
            WBI ("      Ada_Main_Program;");
         end if;
      end if;

      WBI ("   end " & Ada_Init_Name.all & ";");
   end Gen_Adainit_Ada;

   -------------------
   -- Gen_Adainit_C --
   --------------------

   procedure Gen_Adainit_C is
      Main_Priority : Int renames ALIs.Table (ALIs.First).Main_Priority;
      Main_CPU      : Int renames ALIs.Table (ALIs.First).Main_CPU;

   begin
      WBI ("void " & Ada_Init_Name.all & " (void)");
      WBI ("{");

      --  Generate externals for elaboration entities

      for E in Elab_Order.First .. Elab_Order.Last loop
         declare
            Unum : constant Unit_Id := Elab_Order.Table (E);
            U    : Unit_Record renames Units.Table (Unum);

         begin
            --  Check for Elab entity to be set for this unit

            if U.Set_Elab_Entity

            --  Don't generate reference for stand alone library

              and then not U.SAL_Interface

            --  Don't generate reference for predefined file in No_Run_Time
            --  mode, since we don't include the object files in this case

              and then not
                (No_Run_Time_Mode
                   and then Is_Predefined_File_Name (U.Sfile))
            then
               Set_String ("   extern char ");
               Get_Name_String (U.Uname);
               Set_Unit_Name;
               Set_String ("_E;");
               Write_Statement_Buffer;
            end if;
         end;
      end loop;

      Write_Statement_Buffer;

      --  Standard library suppressed

      if Suppress_Standard_Library_On_Target then

         --  Case of High_Integrity_Mode mode. Set __gl_main_priority and
         --  __gl_main_cpu if needed for the Ravenscar profile.

         if Main_Priority /= No_Main_Priority then
            WBI ("   extern int __gl_main_priority;");
            Set_String ("   __gl_main_priority = ");
            Set_Int    (Main_Priority);
            Set_Char   (';');
            Write_Statement_Buffer;
         end if;

         if Main_CPU /= No_Main_CPU then
            WBI ("   extern int __gl_main_cpu;");
            Set_String ("   __gl_main_cpu = ");
            Set_Int    (Main_CPU);
            Set_Char   (';');
            Write_Statement_Buffer;
         end if;

      --  Normal case (standard library not suppressed)

      else
         --  Generate definition for interrupt states string

         Set_String ("   static const char *local_interrupt_states = """);

         for J in 0 .. IS_Pragma_Settings.Last loop
            Set_Char (IS_Pragma_Settings.Table (J));
         end loop;

         Set_String (""";");
         Write_Statement_Buffer;

         --  Generate definition for priority specific dispatching string

         Set_String
           ("   static const char *local_priority_specific_dispatching = """);

         for J in 0 .. PSD_Pragma_Settings.Last loop
            Set_Char (PSD_Pragma_Settings.Table (J));
         end loop;

         Set_String (""";");
         Write_Statement_Buffer;

         --  Generate declaration for secondary stack default if needed

         if Sec_Stack_Used and then Default_Sec_Stack_Size /= -1 then
            WBI ("   extern int system__secondary_stack__" &
                 "default_secondary_stack_size;");
         end if;

         WBI ("");

         --  Code for normal case (standard library not suppressed)

         --  We call the routine from inside adainit() because this works for
         --  both programs with and without binder generated "main" functions.

         WBI ("   extern int __gl_main_priority;");
         Set_String ("   __gl_main_priority = ");
         Set_Int (Main_Priority);
         Set_Char (';');
         Write_Statement_Buffer;

         WBI ("   extern int __gl_time_slice_val;");
         Set_String ("   __gl_time_slice_val = ");

         if Task_Dispatching_Policy = 'F'
           and then ALIs.Table (ALIs.First).Time_Slice_Value = -1
         then
            Set_Int (0);
         else
            Set_Int (ALIs.Table (ALIs.First).Time_Slice_Value);
         end if;

         Set_Char   (';');
         Write_Statement_Buffer;

         WBI ("   extern char __gl_wc_encoding;");
         Set_String ("   __gl_wc_encoding = '");
         Set_Char (Get_WC_Encoding);

         Set_String ("';");
         Write_Statement_Buffer;

         WBI ("   extern char __gl_locking_policy;");
         Set_String ("   __gl_locking_policy = '");
         Set_Char (Locking_Policy_Specified);
         Set_String ("';");
         Write_Statement_Buffer;

         WBI ("   extern char __gl_queuing_policy;");
         Set_String ("   __gl_queuing_policy = '");
         Set_Char (Queuing_Policy_Specified);
         Set_String ("';");
         Write_Statement_Buffer;

         WBI ("   extern char __gl_task_dispatching_policy;");
         Set_String ("   __gl_task_dispatching_policy = '");
         Set_Char (Task_Dispatching_Policy_Specified);
         Set_String ("';");
         Write_Statement_Buffer;

         WBI ("   extern int __gl_main_cpu;");
         Set_String ("   __gl_main_cpu = ");
         Set_Int (Main_CPU);
         Set_Char (';');
         Write_Statement_Buffer;

         Gen_Restrictions_C;

         WBI ("   extern const void *__gl_interrupt_states;");
         WBI ("   __gl_interrupt_states = local_interrupt_states;");

         WBI ("   extern int __gl_num_interrupt_states;");
         Set_String ("   __gl_num_interrupt_states = ");
         Set_Int (IS_Pragma_Settings.Last + 1);
         Set_String (";");
         Write_Statement_Buffer;

         WBI ("   extern const void *__gl_priority_specific_dispatching;");
         WBI ("   __gl_priority_specific_dispatching =" &
              " local_priority_specific_dispatching;");

         WBI ("   extern int __gl_num_specific_dispatching;");
         Set_String ("   __gl_num_specific_dispatching = ");
         Set_Int (PSD_Pragma_Settings.Last + 1);
         Set_String (";");
         Write_Statement_Buffer;

         WBI ("   extern int __gl_unreserve_all_interrupts;");
         Set_String ("   __gl_unreserve_all_interrupts = ");
         Set_Int    (Boolean'Pos (Unreserve_All_Interrupts_Specified));
         Set_String (";");
         Write_Statement_Buffer;

         if Exception_Tracebacks then
            WBI ("   extern int __gl_exception_tracebacks;");
            WBI ("   __gl_exception_tracebacks = 1;");
         end if;

         WBI ("   extern int __gl_zero_cost_exceptions;");
         Set_String ("   __gl_zero_cost_exceptions = ");
         Set_Int    (Boolean'Pos (Zero_Cost_Exceptions_Specified));
         Set_String (";");
         Write_Statement_Buffer;

         WBI ("   extern int __gl_detect_blocking;");
         Set_String ("   __gl_detect_blocking = ");

         if Detect_Blocking then
            Set_Int (1);
         else
            Set_Int (0);
         end if;

         Set_String (";");
         Write_Statement_Buffer;

         WBI ("   extern int __gl_default_stack_size;");
         Set_String ("   __gl_default_stack_size = ");
         Set_Int    (Default_Stack_Size);
         Set_String (";");
         Write_Statement_Buffer;

         WBI ("   extern int __gl_leap_seconds_support;");
         Set_String ("   __gl_leap_seconds_support = ");

         if Leap_Seconds_Support then
            Set_Int (1);
         else
            Set_Int (0);
         end if;

         Set_String (";");
         Write_Statement_Buffer;

         WBI ("");

         --  Install elaboration time signal handler

         WBI ("   if (__gnat_handler_installed == 0)");
         WBI ("     {");
         WBI ("        __gnat_install_handler ();");
         WBI ("     }");

         --  Call feature enable/disable routine

         if OpenVMS_On_Target then
            WBI ("   if (__gnat_features_set == 0)");
            WBI ("     {");
            WBI ("        __gnat_set_features ();");
            WBI ("     }");
         end if;
      end if;

      --  Initialize stack limit for the environment task if the stack
      --  check method is stack limit and stack check is enabled.

      if Stack_Check_Limits_On_Target
        and then (Stack_Check_Default_On_Target or Stack_Check_Switch_Set)
      then
         WBI ("");
         WBI ("   __gnat_initialize_stack_limit ();");
      end if;

      --  Generate call to set Initialize_Scalar values if needed

      if Initialize_Scalars_Used then
         WBI ("");
         Set_String ("      system__scalar_values__initialize('");
         Set_Char (Initialize_Scalars_Mode1);
         Set_String ("', '");
         Set_Char (Initialize_Scalars_Mode2);
         Set_String ("');");
         Write_Statement_Buffer;
      end if;

      --  Generate assignment of default secondary stack size if set

      if Sec_Stack_Used and then Default_Sec_Stack_Size /= -1 then
         WBI ("");
         Set_String ("   system__secondary_stack__");
         Set_String ("default_secondary_stack_size = ");
         Set_Int (Opt.Default_Sec_Stack_Size);
         Set_Char (';');
         Write_Statement_Buffer;
      end if;

      --  Generate elaboration calls

      WBI ("");
      Gen_Elab_Calls_C;
      WBI ("}");
   end Gen_Adainit_C;

   ------------------------
   -- Gen_Elab_Calls_Ada --
   ------------------------

   procedure Gen_Elab_Calls_Ada is
   begin
      for E in Elab_Order.First .. Elab_Order.Last loop
         declare
            Unum : constant Unit_Id := Elab_Order.Table (E);
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

            --  Case of no elaboration code

            elsif U.No_Elab then

               --  The only case in which we have to do something is if
               --  this is a body, with a separate spec, where the separate
               --  spec has an elaboration entity defined.

               --  In that case, this is where we set the elaboration entity
               --  to True, we do not need to test if this has already been
               --  done, since it is quicker to set the flag than to test it.

               if not U.SAL_Interface and then U.Utype = Is_Body
                 and then Units.Table (Unum_Spec).Set_Elab_Entity
               then
                  Set_String ("      E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" := True;");
                  Write_Statement_Buffer;
               end if;

            --  Here if elaboration code is present. If binding a library
            --  or if there is a non-Ada main subprogram then we generate:

            --    if not uname_E then
            --       uname'elab_[spec|body];
            --       uname_E := True;
            --    end if;

            --  Otherwise, elaboration routines are called unconditionally:

            --    uname'elab_[spec|body];
            --    uname_E := True;

            --  The uname_E assignment is skipped if this is a separate spec,
            --  since the assignment will be done when we process the body.

            elsif not U.SAL_Interface then
               if Force_Checking_Of_Elaboration_Flags or
                  Interface_Library_Unit or
                  (not Bind_Main_Program)
               then
                  Set_String ("      if not E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" then");
                  Write_Statement_Buffer;
                  Set_String ("   ");
               end if;

               Set_String ("      ");
               Get_Decoded_Name_String_With_Brackets (U.Uname);

               if VM_Target = CLI_Target and then U.Unit_Kind /= 's' then
                  if Name_Buffer (Name_Len) = 's' then
                     Name_Buffer (Name_Len - 1 .. Name_Len + 12) :=
                       "_pkg'elab_spec";
                  else
                     Name_Buffer (Name_Len - 1 .. Name_Len + 12) :=
                       "_pkg'elab_body";
                  end if;

                  Name_Len := Name_Len + 12;

               else
                  if Name_Buffer (Name_Len) = 's' then
                     Name_Buffer (Name_Len - 1 .. Name_Len + 8) :=
                       "'elab_spec";
                  else
                     Name_Buffer (Name_Len - 1 .. Name_Len + 8) :=
                       "'elab_body";
                  end if;

                  Name_Len := Name_Len + 8;
               end if;

               Set_Casing (U.Icasing);
               Set_Name_Buffer;
               Set_Char (';');
               Write_Statement_Buffer;

               if U.Utype /= Is_Spec then
                  if Force_Checking_Of_Elaboration_Flags or
                     Interface_Library_Unit or
                     (not Bind_Main_Program)
                  then
                     Set_String ("   ");
                  end if;

                  Set_String ("      E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" := True;");
                  Write_Statement_Buffer;
               end if;

               if Force_Checking_Of_Elaboration_Flags or
                  Interface_Library_Unit or
                  (not Bind_Main_Program)
               then
                  WBI ("      end if;");
               end if;
            end if;
         end;
      end loop;
   end Gen_Elab_Calls_Ada;

   ----------------------
   -- Gen_Elab_Calls_C --
   ----------------------

   procedure Gen_Elab_Calls_C is
   begin
      for E in Elab_Order.First .. Elab_Order.Last loop
         declare
            Unum : constant Unit_Id := Elab_Order.Table (E);
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

            --  Case of no elaboration code

            elsif U.No_Elab then

               --  The only case in which we have to do something is if
               --  this is a body, with a separate spec, where the separate
               --  spec has an elaboration entity defined.

               --  In that case, this is where we set the elaboration entity
               --  to True, we do not need to test if this has already been
               --  done, since it is quicker to set the flag than to test it.

               if not U.SAL_Interface and then U.Utype = Is_Body
                 and then Units.Table (Unum_Spec).Set_Elab_Entity
               then
                  Set_String ("   ");
                  Get_Name_String (U.Uname);
                  Set_Unit_Name;
                  Set_String ("_E = 1;");
                  Write_Statement_Buffer;
               end if;

            --  Here if elaboration code is present. If binding a library
            --  or if there is a non-Ada main subprogram then we generate:

            --    if (uname_E == 0) {
            --       uname__elab[s|b] ();
            --       uname_E++;
            --    }

            --  The uname_E assignment is skipped if this is a separate spec,
            --  since the assignment will be done when we process the body.

            elsif not U.SAL_Interface then
               Get_Name_String (U.Uname);

               if Force_Checking_Of_Elaboration_Flags or
                  Interface_Library_Unit or
                  (not Bind_Main_Program)
               then
                  Set_String ("   if (");
                  Set_Unit_Name;
                  Set_String ("_E == 0) {");
                  Write_Statement_Buffer;
                  Set_String ("   ");
               end if;

               Set_String ("   ");
               Set_Unit_Name;
               Set_String ("___elab");
               Set_Char (Name_Buffer (Name_Len)); -- 's' or 'b' for spec/body
               Set_String (" ();");
               Write_Statement_Buffer;

               if U.Utype /= Is_Spec then
                  if Force_Checking_Of_Elaboration_Flags or
                     Interface_Library_Unit or
                     (not Bind_Main_Program)
                  then
                     Set_String ("   ");
                  end if;

                  Set_String ("   ");
                  Set_Unit_Name;
                  Set_String ("_E++;");
                  Write_Statement_Buffer;
               end if;

               if Force_Checking_Of_Elaboration_Flags or
                  Interface_Library_Unit or
                  (not Bind_Main_Program)
               then
                  WBI ("   }");
               end if;
            end if;
         end;
      end loop;

   end Gen_Elab_Calls_C;

   ----------------------
   -- Gen_Elab_Defs_C --
   ----------------------

   procedure Gen_Elab_Defs_C is
   begin
      for E in Elab_Order.First .. Elab_Order.Last loop

         --  Generate declaration of elaboration procedure if elaboration
         --  needed. Note that passive units are always excluded.

         if not Units.Table (Elab_Order.Table (E)).No_Elab then
            Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);
            Set_String ("extern void ");
            Set_Unit_Name;
            Set_String ("___elab");
            Set_Char (Name_Buffer (Name_Len)); -- 's' or 'b' for spec/body
            Set_String (" (void);");
            Write_Statement_Buffer;
         end if;

      end loop;

      WBI ("");
   end Gen_Elab_Defs_C;

   ------------------------
   -- Gen_Elab_Order_Ada --
   ------------------------

   procedure Gen_Elab_Order_Ada is
   begin
      WBI ("");
      WBI ("   --  BEGIN ELABORATION ORDER");

      for J in Elab_Order.First .. Elab_Order.Last loop
         Set_String ("   --  ");
         Get_Name_String (Units.Table (Elab_Order.Table (J)).Uname);
         Set_Name_Buffer;
         Write_Statement_Buffer;
      end loop;

      WBI ("   --  END ELABORATION ORDER");
   end Gen_Elab_Order_Ada;

   ----------------------
   -- Gen_Elab_Order_C --
   ----------------------

   procedure Gen_Elab_Order_C is
   begin
      WBI ("");
      WBI ("/* BEGIN ELABORATION ORDER");

      for J in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (J)).Uname);
         Set_Name_Buffer;
         Write_Statement_Buffer;
      end loop;

      WBI ("   END ELABORATION ORDER */");
   end Gen_Elab_Order_C;

   ------------------
   -- Gen_Main_Ada --
   ------------------

   procedure Gen_Main_Ada is
   begin
      WBI ("");

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

      if not Cumulative_Restrictions.Set (No_Finalization) then
         WBI ("      procedure initialize (Addr : System.Address);");
         WBI ("      pragma Import (C, initialize, ""__gnat_initialize"");");
         WBI ("");
         WBI ("      procedure finalize;");
         WBI ("      pragma Import (C, finalize, ""__gnat_finalize"");");
      end if;

      --  If we want to analyze the stack, we have to import corresponding
      --  symbols

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

         --  To call the main program, we declare it using a pragma Import
         --  Ada with the right link name.

         --  It might seem more obvious to "with" the main program, and call
         --  it in the normal Ada manner. We do not do this for three reasons:

         --    1. It is more efficient not to recompile the main program
         --    2. We are not entitled to assume the source is accessible
         --    3. We don't know what options to use to compile it

         --  It is really reason 3 that is most critical (indeed we used
         --  to generate the "with", but several regression tests failed).

         WBI ("");

         if ALIs.Table (ALIs.First).Main_Program = Func then
            WBI ("      Result : Integer;");
            WBI ("");
            WBI ("      function Ada_Main_Program return Integer;");

         else
            WBI ("      procedure Ada_Main_Program;");
         end if;

         Set_String ("      pragma Import (Ada, Ada_Main_Program, """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (""");");

         Write_Statement_Buffer;
         WBI ("");

         if Bind_Main_Program
           and then not Suppress_Standard_Library_On_Target
         then
            WBI ("      SEH : aliased array (1 .. 2) of Integer;");
            WBI ("");
         end if;
      end if;

      --  Generate a reference to Ada_Main_Program_Name. This symbol is
      --  not referenced elsewhere in the generated program, but is needed
      --  by the debugger (that's why it is generated in the first place).
      --  The reference stops Ada_Main_Program_Name from being optimized
      --  away by smart linkers, such as the AiX linker.

      --  Because this variable is unused, we make this variable "aliased"
      --  with a pragma Volatile in order to tell the compiler to preserve
      --  this variable at any level of optimization.

      if Bind_Main_Program then
         WBI
           ("      Ensure_Reference : aliased System.Address := " &
            "Ada_Main_Program_Name'Address;");
         WBI ("      pragma Volatile (Ensure_Reference);");
         WBI ("");
      end if;

      WBI ("   begin");

      --  Acquire command line arguments if present on target

      if Command_Line_Args_On_Target then
         WBI ("      gnat_argc := argc;");
         WBI ("      gnat_argv := argv;");
         WBI ("      gnat_envp := envp;");
         WBI ("");

      --  If configurable run time and no command line args, then nothing
      --  needs to be done since the gnat_argc/argv/envp variables are
      --  suppressed in this case.

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

      if not Cumulative_Restrictions.Set (No_Finalization) then
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
         WBI ("      Break_Start;");

         if ALIs.Table (ALIs.First).Main_Program = Proc then
            WBI ("      Ada_Main_Program;");
         else
            WBI ("      Result := Ada_Main_Program;");
         end if;
      end if;

      --  Adafinal call is skipped if no finalization

      if not Cumulative_Restrictions.Set (No_Finalization) then

         --  If compiling for the JVM, we directly call Adafinal because
         --  we don't import it via Do_Finalize (see Gen_Output_File_Ada).

         if VM_Target = No_VM then
            WBI ("      Do_Finalize;");
         else
            WBI ("      System.Standard_Library.Adafinal;");
         end if;
      end if;

      --  Prints the result of static stack analysis

      if Dynamic_Stack_Measurement then
         WBI ("      Output_Results;");
      end if;

      --  Finalize is only called if we have a run time

      if not Cumulative_Restrictions.Set (No_Finalization) then
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
   end Gen_Main_Ada;

   ----------------
   -- Gen_Main_C --
   ----------------

   procedure Gen_Main_C is
   begin
      if Exit_Status_Supported_On_Target then
         WBI ("#include <stdlib.h>");
         Set_String ("int ");
      else
         Set_String ("void ");
      end if;

      Set_String (Get_Main_Name);

      --  Generate command line args in prototype if present on target

      if Command_Line_Args_On_Target then
         Write_Statement_Buffer (" (int argc, char **argv, char **envp)");

      --  Case of no command line arguments on target

      else
         Write_Statement_Buffer (" (void)");
      end if;

      WBI ("{");

      --  Generate a reference to __gnat_ada_main_program_name. This symbol
      --  is  not referenced elsewhere in the generated program, but is
      --  needed by the debugger (that's why it is generated in the first
      --  place). The reference stops Ada_Main_Program_Name from being
      --  optimized away by smart linkers, such as the AiX linker.

      --  Because this variable is unused, we declare this variable as
      --  volatile in order to tell the compiler to preserve it at any
      --  level of optimization.

      if Bind_Main_Program then
         WBI ("   char * volatile ensure_reference " &
              "__attribute__ ((__unused__)) = " &
              "__gnat_ada_main_program_name;");
         WBI ("");

         if not Suppress_Standard_Library_On_Target
           and then not No_Main_Subprogram
         then
            WBI ("   int SEH [2];");
            WBI ("");
         end if;
      end if;

      --  If main program is a function, generate result variable

      if ALIs.Table (ALIs.First).Main_Program = Func then
         WBI ("   int result;");
      end if;

      --  Set command line argument values from parameters if command line
      --  arguments are present on target

      if Command_Line_Args_On_Target then
         WBI ("   gnat_argc = argc;");
         WBI ("   gnat_argv = argv;");
         WBI ("   gnat_envp = envp;");
         WBI (" ");

      --  If configurable run-time, then nothing to do, since in this case
      --  the gnat_argc/argv/envp variables are entirely suppressed.

      elsif Configurable_Run_Time_On_Target then
         null;

      --  if no command line arguments on target, set dummy values

      else
         WBI ("   gnat_argc = 0;");
         WBI ("   gnat_argv = 0;");
         WBI ("   gnat_envp = 0;");
      end if;

      if Opt.Default_Exit_Status /= 0
        and then Bind_Main_Program
        and then not Configurable_Run_Time_Mode
      then
         Set_String ("   __gnat_set_exit_status (");
         Set_Int (Opt.Default_Exit_Status);
         Set_String (");");
         Write_Statement_Buffer;
      end if;

      --  Initializes dynamic stack measurement if needed

      if Dynamic_Stack_Measurement then
         Set_String ("   __gnat_stack_usage_initialize (");
         Set_Int (Dynamic_Stack_Measurement_Array_Size);
         Set_String (");");
         Write_Statement_Buffer;
      end if;

      --  The __gnat_initialize routine is used only if we have a run-time

      if not Suppress_Standard_Library_On_Target then
         if not No_Main_Subprogram and then Bind_Main_Program then
            WBI ("   __gnat_initialize ((void *)SEH);");
         else
            WBI ("   __gnat_initialize ((void *)0);");
         end if;
      end if;

      WBI ("   " & Ada_Init_Name.all & " ();");

      if not No_Main_Subprogram then
         WBI ("   __gnat_break_start ();");
         WBI (" ");

         --  Output main program name

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         --  Main program is procedure case

         if ALIs.Table (ALIs.First).Main_Program = Proc then
            Set_String ("   ");
            Set_Main_Program_Name;
            Set_String (" ();");
            Write_Statement_Buffer;

         --  Main program is function case

         else -- ALIs.Table (ALIs_First).Main_Program = Func
            Set_String ("   result = ");
            Set_Main_Program_Name;
            Set_String (" ();");
            Write_Statement_Buffer;
         end if;

      end if;

      --  Call adafinal if finalization active

      if not Cumulative_Restrictions.Set (No_Finalization) then
         WBI (" ");
         WBI ("   system__standard_library__adafinal ();");
      end if;

      --  Outputs the dynamic stack measurement if needed

      if Dynamic_Stack_Measurement then
         WBI ("   __gnat_stack_usage_output_results ();");
      end if;

      --  The finalize routine is used only if we have a run-time

      if not Suppress_Standard_Library_On_Target then
         WBI ("   __gnat_finalize ();");
      end if;

      --  Case of main program is a function, so the value it returns
      --  is the exit status in this case.

      if ALIs.Table (ALIs.First).Main_Program = Func then
         if Exit_Status_Supported_On_Target then

            --  VMS must use Posix exit routine in order to get the effect
            --  of a Unix compatible setting of the program exit status.
            --  For all other systems, we use the standard exit routine.

            if OpenVMS_On_Target then
               WBI ("   decc$__posix_exit (result);");
            else
               WBI ("   exit (result);");
            end if;
         end if;

      --  Case of main program is a procedure, in which case the exit
      --  status is whatever was set by a Set_Exit call most recently

      else
         if Exit_Status_Supported_On_Target then

            --  VMS must use Posix exit routine in order to get the effect
            --  of a Unix compatible setting of the program exit status.
            --  For all other systems, we use the standard exit routine.

            if OpenVMS_On_Target then
               WBI ("   decc$__posix_exit (gnat_exit_status);");
            else
               WBI ("   exit (gnat_exit_status);");
            end if;
         end if;
      end if;

      WBI ("}");
   end Gen_Main_C;

   ------------------------------
   -- Gen_Object_Files_Options --
   ------------------------------

   procedure Gen_Object_Files_Options is
      Lgnat : Natural;
      --  This keeps track of the position in the sorted set of entries
      --  in the Linker_Options table of where the first entry from an
      --  internal file appears.

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
               Write_Info_Ada_C
                 ("   --   ", "", Name_Buffer (Start .. Stop - 1));
            end if;

            Start := Stop + 1;
         end loop;
      end Write_Linker_Option;

   --  Start of processing for Gen_Object_Files_Options

   begin
      WBI ("");
      Write_Info_Ada_C ("-- ", "/* ", " BEGIN Object file/option list");

      if Object_List_Filename /= null then
         Set_List_File (Object_List_Filename.all);
      end if;

      for E in Elab_Order.First .. Elab_Order.Last loop

         --  If not spec that has an associated body, then generate a comment
         --  giving the name of the corresponding object file.

         if (not Units.Table (Elab_Order.Table (E)).SAL_Interface)
           and then Units.Table (Elab_Order.Table (E)).Utype /= Is_Spec
         then
            Get_Name_String
              (ALIs.Table
                (Units.Table (Elab_Order.Table (E)).My_ALI).Ofile_Full_Name);

            --  If the presence of an object file is necessary or if it exists,
            --  then use it.

            if not Hostparm.Exclude_Missing_Objects
              or else
                System.OS_Lib.Is_Regular_File (Name_Buffer (1 .. Name_Len))
            then
               Write_Info_Ada_C ("   --   ", "", Name_Buffer (1 .. Name_Len));

               if Output_Object_List then
                  Write_Str (Name_Buffer (1 .. Name_Len));
                  Write_Eol;
               end if;

               --  Don't link with the shared library on VMS if an internal
               --  filename object is seen. Multiply defined symbols will
               --  result.

               if OpenVMS_On_Target
                 and then Is_Internal_File_Name
                  (ALIs.Table
                   (Units.Table (Elab_Order.Table (E)).My_ALI).Sfile)
               then
                  --  Special case for g-trasym.obj (not included in libgnat)

                  Get_Name_String (ALIs.Table
                            (Units.Table (Elab_Order.Table (E)).My_ALI).Sfile);

                  if Name_Buffer (1 .. 8) /= "g-trasym" then
                     Opt.Shared_Libgnat := False;
                  end if;
               end if;
            end if;
         end if;
      end loop;

      if Object_List_Filename /= null then
         Close_List_File;
      end if;

      --  Add a "-Ldir" for each directory in the object path
      if VM_Target /= CLI_Target then
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

      --  Note that we do not insert anything when pragma No_Run_Time has been
      --  specified or when the standard libraries are not to be used,
      --  otherwise on some platforms, such as VMS, we may get duplicate
      --  symbols when linking.

      if not (Opt.No_Run_Time_Mode or else Opt.No_Stdlib) then
         Name_Len := 0;

         if Opt.Shared_Libgnat then
            Add_Str_To_Name_Buffer ("-shared");
         else
            Add_Str_To_Name_Buffer ("-static");
         end if;

         --  Write directly to avoid -K output (why???)

         Write_Info_Ada_C ("   --   ", "", Name_Buffer (1 .. Name_Len));

         if With_DECGNAT then
            Name_Len := 0;

            if Opt.Shared_Libgnat then
               Add_Str_To_Name_Buffer (Shared_Lib ("decgnat"));
            else
               Add_Str_To_Name_Buffer ("-ldecgnat");
            end if;

            Write_Linker_Option;
         end if;

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

      if Ada_Bind_File then
         WBI ("--  END Object file/option list   ");
      else
         WBI ("    END Object file/option list */");
      end if;
   end Gen_Object_Files_Options;

   ---------------------
   -- Gen_Output_File --
   ---------------------

   procedure Gen_Output_File (Filename : String) is
   begin
      --  Acquire settings for Interrupt_State pragmas

      Set_IS_Pragma_Table;

      --  Acquire settings for Priority_Specific_Dispatching pragma

      Set_PSD_Pragma_Table;

      --  Override Ada_Bind_File and Bind_Main_Program for VMs since JGNAT only
      --  supports Ada code, and the main program is already generated by the
      --  compiler.

      if VM_Target /= No_VM then
         Ada_Bind_File := True;

         if VM_Target = JVM_Target then
            Bind_Main_Program := False;
         end if;
      end if;

      --  Override time slice value if -T switch is set

      if Time_Slice_Set then
         ALIs.Table (ALIs.First).Time_Slice_Value := Opt.Time_Slice_Value;
      end if;

      --  Count number of elaboration calls

      for E in Elab_Order.First .. Elab_Order.Last loop
         if Units.Table (Elab_Order.Table (E)).No_Elab then
            null;
         else
            Num_Elab_Calls := Num_Elab_Calls + 1;
         end if;
      end loop;

      --  Generate output file in appropriate language

      Check_System_Restrictions_Used;

      if Ada_Bind_File then
         Gen_Output_File_Ada (Filename);
      else
         Gen_Output_File_C (Filename);
      end if;
   end Gen_Output_File;

   -------------------------
   -- Gen_Output_File_Ada --
   -------------------------

   procedure Gen_Output_File_Ada (Filename : String) is

      Bfiles : Name_Id;
      --  Name of generated bind file (spec)

      Bfileb : Name_Id;
      --  Name of generated bind file (body)

      Ada_Main : constant String := Get_Ada_Main_Name;
      --  Name to be used for generated Ada main program. See the body of
      --  function Get_Ada_Main_Name for details on the form of the name.

   begin
      --  Create spec first

      Create_Binder_Output (Filename, 's', Bfiles);

      --  We always compile the binder file in Ada 95 mode so that we properly
      --  handle use of Ada 2005 keywords as identifiers in Ada 95 mode. None
      --  of the Ada 2005 constructs are needed by the binder file.

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

      --  Generate with of System.Secondary_Stack if active

      if Sec_Stack_Used and then Default_Sec_Stack_Size /= -1 then
         WBI ("with System.Secondary_Stack;");
      end if;

      Resolve_Binder_Options;

      if VM_Target /= No_VM then
         if not Suppress_Standard_Library_On_Target then

            --  Usually, adafinal is called using a pragma Import C. Since
            --  Import C doesn't have the same semantics for JGNAT, we use
            --  standard Ada.

            WBI ("with System.Standard_Library;");
         end if;
      end if;

      WBI ("package " & Ada_Main & " is");
      WBI ("   pragma Warnings (Off);");

      --  Main program case

      if Bind_Main_Program then
         if VM_Target = No_VM then

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

            --  Define exit status. Again in normal mode, this is in the
            --  run-time library, and is initialized there, but in the
            --  configurable runtime case, the variable is declared and
            --  initialized in this file.

            WBI ("");

            if Configurable_Run_Time_Mode then
               if Exit_Status_Supported_On_Target then
                  WBI ("   gnat_exit_status : Integer := 0;");
               end if;

            else
               WBI ("   gnat_exit_status : Integer;");
               WBI ("   pragma Import (C, gnat_exit_status);");
            end if;
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

         if VM_Target = No_VM then
            Set_Main_Program_Name;
            Set_String (""" & ASCII.NUL;");
         else
            Set_String (Name_Buffer (1 .. Name_Len - 2) & """;");
         end if;

         Write_Statement_Buffer;

         WBI
           ("   pragma Export (C, Ada_Main_Program_Name, " &
            """__gnat_ada_main_program_name"");");
      end if;

      if not Cumulative_Restrictions.Set (No_Finalization) then
         WBI ("");
         WBI ("   procedure " & Ada_Final_Name.all & ";");
         WBI ("   pragma Export (C, " & Ada_Final_Name.all & ", """ &
              Ada_Final_Name.all & """);");
      end if;

      WBI ("");
      WBI ("   procedure " & Ada_Init_Name.all & ";");
      WBI ("   pragma Export (C, " & Ada_Init_Name.all & ", """ &
           Ada_Init_Name.all & """);");

      --  If -a has been specified use pragma Linker_Constructor for the init
      --  procedure. No need to use a similar pragma for the final procedure as
      --  global finalization will occur when the executable finishes execution
      --  and for plugins (shared stand-alone libraries that can be
      --  "unloaded"), finalization should not occur automatically, otherwise
      --  the main executable may not continue to work properly.

      if Use_Pragma_Linker_Constructor then
         WBI ("   pragma Linker_Constructor (" & Ada_Init_Name.all & ");");
      end if;

      if Bind_Main_Program and then VM_Target = No_VM then

         --  If we have the standard library, then Break_Start is defined
         --  there, but when the standard library is suppressed, Break_Start
         --  is defined here.

         WBI ("");
         WBI ("   procedure Break_Start;");

         if Suppress_Standard_Library_On_Target then
            WBI ("   pragma Export (C, Break_Start, ""__gnat_break_start"");");
         else
            WBI ("   pragma Import (C, Break_Start, ""__gnat_break_start"");");
         end if;

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

      Gen_Versions_Ada;
      Gen_Elab_Order_Ada;

      --  Spec is complete

      WBI ("");
      WBI ("end " & Ada_Main & ";");
      Close_Binder_Output;

      --  Prepare to write body

      Create_Binder_Output (Filename, 'b', Bfileb);

      --  We always compile the binder file in Ada 95 mode so that we properly
      --  handle use of Ada 2005 keywords as identifiers in Ada 95 mode. None
      --  of the Ada 2005 constructs are needed by the binder file.

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

      --  Generate with of System.Restrictions to initialize
      --  Run_Time_Restrictions.

      if System_Restrictions_Used
        and not Suppress_Standard_Library_On_Target
      then
         WBI ("");
         WBI ("with System.Restrictions;");
      end if;

      WBI ("");
      WBI ("package body " & Ada_Main & " is");
      WBI ("   pragma Warnings (Off);");

      --  Import the finalization procedure only if finalization active

      if not Cumulative_Restrictions.Set (No_Finalization) then

         --  In the Java case, pragma Import C cannot be used, so the standard
         --  Ada constructs will be used instead.

         if VM_Target = No_VM then
            WBI ("");
            WBI ("   procedure Do_Finalize;");
            WBI
              ("   pragma Import (C, Do_Finalize, " &
               """system__standard_library__adafinal"");");
            WBI ("");
         end if;
      end if;

      if not Suppress_Standard_Library_On_Target then

         --  Generate Priority_Specific_Dispatching pragma string

         Set_String
           ("   Local_Priority_Specific_Dispatching : constant String := """);

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

      Gen_Adainit_Ada;

      --  Generate the adafinal routine unless there is no finalization to do

      if not Cumulative_Restrictions.Set (No_Finalization) then
         Gen_Adafinal_Ada;
      end if;

      if Bind_Main_Program and then VM_Target = No_VM then

         --  When suppressing the standard library then generate dummy body
         --  for Break_Start

         if Suppress_Standard_Library_On_Target then
            WBI ("");
            WBI ("   procedure Break_Start is");
            WBI ("   begin");
            WBI ("      null;");
            WBI ("   end;");
         end if;

         Gen_Main_Ada;
      end if;

      --  Output object file list and the Ada body is complete

      Gen_Object_Files_Options;

      WBI ("");
      WBI ("end " & Ada_Main & ";");

      Close_Binder_Output;
   end Gen_Output_File_Ada;

   -----------------------
   -- Gen_Output_File_C --
   -----------------------

   procedure Gen_Output_File_C (Filename : String) is
      Bfile : Name_Id;
      pragma Warnings (Off, Bfile);
      --  Name of generated bind file (not referenced)

   begin
      Create_Binder_Output (Filename, 'c', Bfile);

      Resolve_Binder_Options;

      WBI ("extern void " & Ada_Final_Name.all & " (void);");

      --  If -a has been specified use __attribute__((constructor)) for the
      --  init procedure. No need to use a similar featute for the final
      --  procedure as global finalization will occur when the executable
      --  finishes execution and for plugins (shared stand-alone libraries that
      --  can be "unloaded"), finalization should not occur automatically,
      --  otherwise the main executable may not continue to work properly.

      if Use_Pragma_Linker_Constructor then
         WBI ("extern void " & Ada_Init_Name.all &
              " (void) __attribute__((constructor));");
      else
         WBI ("extern void " & Ada_Init_Name.all & " (void);");
      end if;

      WBI ("extern void system__standard_library__adafinal (void);");

      if not No_Main_Subprogram then
         Set_String ("extern ");

         if Exit_Status_Supported_On_Target then
            Set_String ("int");
         else
            Set_String ("void");
         end if;

         Set_String (" main ");

         if Command_Line_Args_On_Target then
            Write_Statement_Buffer ("(int, char **, char **);");
         else
            Write_Statement_Buffer ("(void);");
         end if;

         if OpenVMS_On_Target then
            WBI ("extern void decc$__posix_exit (int);");
         else
            WBI ("extern void exit (int);");
         end if;

         WBI ("extern void __gnat_break_start (void);");
         Set_String ("extern ");

         if ALIs.Table (ALIs.First).Main_Program = Proc then
            Set_String ("void ");
         else
            Set_String ("int ");
         end if;

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (" (void);");
         Write_Statement_Buffer;
      end if;

      if not Suppress_Standard_Library_On_Target then
         WBI ("extern void __gnat_initialize (void *);");
         WBI ("extern void __gnat_finalize (void);");
         WBI ("extern void __gnat_install_handler (void);");
      end if;

      if Dynamic_Stack_Measurement then
         WBI ("");
         WBI ("extern void __gnat_stack_usage_output_results (void);");
         WBI ("extern void __gnat_stack_usage_initialize (int size);");
      end if;

      --  Initialize stack limit for the environment task if the stack check
      --  method is stack limit and stack check is enabled.

      if Stack_Check_Limits_On_Target
        and then (Stack_Check_Default_On_Target or Stack_Check_Switch_Set)
      then
         WBI ("");
         WBI ("extern void __gnat_initialize_stack_limit (void);");
      end if;

      WBI ("");

      Gen_Elab_Defs_C;

      --  Imported variables used only when we have a runtime

      if not Suppress_Standard_Library_On_Target then

         --  Track elaboration/finalization phase

         WBI ("extern int  __gnat_handler_installed;");
         WBI ("");

         --  Track feature enable/disable on VMS

         if OpenVMS_On_Target then
            WBI ("extern int  __gnat_features_set;");
            WBI ("");
         end if;
      end if;

      --  Write argv/argc exit status stuff if main program case

      if Bind_Main_Program then

         --  First deal with argc/argv/envp. In the normal case they are in the
         --  run-time library.

         if not Configurable_Run_Time_On_Target then
            WBI ("extern int gnat_argc;");
            WBI ("extern char **gnat_argv;");
            WBI ("extern char **gnat_envp;");

         --  If configurable run time and no command line args, then the
         --  generation of these variables is entirely suppressed.

         elsif not Command_Line_Args_On_Target then
            null;

         --  Otherwise, in the configurable run-time case they are right in the
         --  binder file.

         else
            WBI ("int gnat_argc;");
            WBI ("char **gnat_argv;");
            WBI ("char **gnat_envp;");
         end if;

         --  Similarly deal with exit status

         if not Configurable_Run_Time_On_Target then
            WBI ("extern int gnat_exit_status;");

         --  If configurable run time and no exit status on target, then the
         --  generation of this variables is entirely suppressed.

         elsif not Exit_Status_Supported_On_Target then
            null;

         --  Otherwise, in the configurable run-time case this variable is
         --  right in the binder file, and initialized to zero there.

         else
            WBI ("int gnat_exit_status = 0;");
         end if;

         WBI ("");
      end if;

      --  When suppressing the standard library, the __gnat_break_start routine
      --  (for the debugger to get initial control) is defined in this file.

      if Suppress_Standard_Library_On_Target then
         WBI ("");
         WBI ("void __gnat_break_start (void) {}");
      end if;

      --  Generate the __gnat_version and __gnat_ada_main_program_name info
      --  only for the main program. Otherwise, it can lead under some
      --  circumstances to a symbol duplication during the link (for instance
      --  when a C program uses 2 Ada libraries)

      if Bind_Main_Program then
         WBI ("");
         WBI ("char __gnat_version[] = """ & Ver_Prefix &
                                   Gnat_Version_String & """;");

         Set_String ("char __gnat_ada_main_program_name[] = """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (""";");
         Write_Statement_Buffer;
      end if;

      --  Generate the adafinal routine. In no runtime mode, this is not
      --  needed, since there is no finalization to do.

      if not Cumulative_Restrictions.Set (No_Finalization) then
         Gen_Adafinal_C;
      end if;

      Gen_Adainit_C;

      --  Main is only present for Ada main case

      if Bind_Main_Program then
         Gen_Main_C;
      end if;

      --  Generate versions, elaboration order, list of object files

      Gen_Versions_C;
      Gen_Elab_Order_C;
      Gen_Object_Files_Options;

      --  C binder output is complete

      Close_Binder_Output;
   end Gen_Output_File_C;

   --------------------------
   -- Gen_Restrictions_Ada --
   --------------------------

   procedure Gen_Restrictions_Ada is
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
   end Gen_Restrictions_Ada;

   ------------------------
   -- Gen_Restrictions_C --
   ------------------------

   procedure Gen_Restrictions_C is
   begin
      if Suppress_Standard_Library_On_Target
        or not System_Restrictions_Used
      then
         return;
      end if;

      WBI ("   typedef struct {");
      Set_String ("     char set [");
      Set_Int (Cumulative_Restrictions.Set'Length);
      Set_String ("];");
      Write_Statement_Buffer;

      Set_String ("     int value [");
      Set_Int (Cumulative_Restrictions.Value'Length);
      Set_String ("];");
      Write_Statement_Buffer;

      Set_String ("     char violated [");
      Set_Int (Cumulative_Restrictions.Violated'Length);
      Set_String ("];");
      Write_Statement_Buffer;

      Set_String ("     int count [");
      Set_Int (Cumulative_Restrictions.Count'Length);
      Set_String ("];");
      Write_Statement_Buffer;

      Set_String ("     char unknown [");
      Set_Int (Cumulative_Restrictions.Unknown'Length);
      Set_String ("];");
      Write_Statement_Buffer;
      WBI ("   } restrictions;");
      WBI ("   extern restrictions " &
           "system__restrictions__run_time_restrictions;");
      WBI ("   restrictions r = {");
      Set_String ("     {");

      for J in Cumulative_Restrictions.Set'Range loop
         Set_Int (Boolean'Pos (Cumulative_Restrictions.Set (J)));
         Set_String (", ");
      end loop;

      Set_String_Replace ("},");
      Write_Statement_Buffer;
      Set_String ("     {");

      for J in Cumulative_Restrictions.Value'Range loop
         Set_Int (Int (Cumulative_Restrictions.Value (J)));
         Set_String (", ");
      end loop;

      Set_String_Replace ("},");
      Write_Statement_Buffer;
      Set_String ("     {");

      for J in Cumulative_Restrictions.Violated'Range loop
         Set_Int (Boolean'Pos (Cumulative_Restrictions.Violated (J)));
         Set_String (", ");
      end loop;

      Set_String_Replace ("},");
      Write_Statement_Buffer;
      Set_String ("     {");

      for J in Cumulative_Restrictions.Count'Range loop
         Set_Int (Int (Cumulative_Restrictions.Count (J)));
         Set_String (", ");
      end loop;

      Set_String_Replace ("},");
      Write_Statement_Buffer;
      Set_String ("     {");

      for J in Cumulative_Restrictions.Unknown'Range loop
         Set_Int (Boolean'Pos (Cumulative_Restrictions.Unknown (J)));
         Set_String (", ");
      end loop;

      Set_String_Replace ("}}");
      Set_String (";");
      Write_Statement_Buffer;
      WBI ("   system__restrictions__run_time_restrictions = r;");
   end Gen_Restrictions_C;

   ----------------------
   -- Gen_Versions_Ada --
   ----------------------

   --  This routine generates lines such as:

   --    unnnnn : constant Integer := 16#hhhhhhhh#;
   --    pragma Export (C, unnnnn, unam);

   --  for each unit, where unam is the unit name suffixed by either B or S for
   --  body or spec, with dots replaced by double underscores, and hhhhhhhh is
   --  the version number, and nnnnn is a 5-digits serial number.

   procedure Gen_Versions_Ada is
      Ubuf : String (1 .. 6) := "u00000";

      procedure Increment_Ubuf;
      --  Little procedure to increment the serial number

      procedure Increment_Ubuf is
      begin
         for J in reverse Ubuf'Range loop
            Ubuf (J) := Character'Succ (Ubuf (J));
            exit when Ubuf (J) <= '9';
            Ubuf (J) := '0';
         end loop;
      end Increment_Ubuf;

   --  Start of processing for Gen_Versions_Ada

   begin
      WBI ("");

      WBI ("   type Version_32 is mod 2 ** 32;");
      for U in Units.First .. Units.Last loop
         if not Units.Table (U).SAL_Interface and then
           ((not Bind_For_Library) or else Units.Table (U).Directly_Scanned)
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

   end Gen_Versions_Ada;

   --------------------
   -- Gen_Versions_C --
   --------------------

   --  This routine generates a line of the form:

   --    unsigned unam = 0xhhhhhhhh;

   --  for each unit, where unam is the unit name suffixed by either B or S for
   --  body or spec, with dots replaced by double underscores.

   procedure Gen_Versions_C is
   begin
      for U in Units.First .. Units.Last loop
         if not Units.Table (U).SAL_Interface and then
           ((not Bind_For_Library) or else Units.Table (U).Directly_Scanned)
         then
            Set_String ("unsigned ");

            Get_Name_String (Units.Table (U).Uname);

            for K in 1 .. Name_Len loop
               if Name_Buffer (K) = '.' then
                  Set_String ("__");

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

            Set_String (" = 0x");
            Set_String (Units.Table (U).Version);
            Set_Char   (';');
            Write_Statement_Buffer;
         end if;
      end loop;

   end Gen_Versions_C;

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
      --  The main program generated by JGNAT expects a package called
      --  ada_<main procedure>.

      if VM_Target /= No_VM then
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         return "ada_" & Get_Main_Unit_Name (Name_Buffer (1 .. Name_Len - 2));
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
      --  (wide character default encoding for [Wide_[Wide_]Text_IO when there
      --  is no main program).

      elsif No_Main_Subprogram then
         return 'b';

      --  Otherwise if there is a main program, take encoding from it

      else
         return ALIs.Table (ALIs.First).WC_Encoding;
      end if;
   end Get_WC_Encoding;

   ----------------------
   -- Lt_Linker_Option --
   ----------------------

   function Lt_Linker_Option (Op1, Op2 : Natural) return Boolean is
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

   procedure Resolve_Binder_Options is
   begin
      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);

         --  This is not a perfect approach, but is the current protocol
         --  between the run-time and the binder to indicate that tasking is
         --  used: system.os_interface should always be used by any tasking
         --  application.

         if Name_Buffer (1 .. 19) = "system.os_interface" then
            With_GNARL := True;
         end if;

         --  Ditto for declib and the "dec" package

         if OpenVMS_On_Target and then Name_Buffer (1 .. 5) = "dec%s" then
            With_DECGNAT := True;
         end if;
      end loop;
   end Resolve_Binder_Options;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean (B : Boolean) is
      True_Str  : constant String := "True";
      False_Str : constant String := "False";
   begin
      if B then
         Statement_Buffer (Last + 1 .. Last + True_Str'Length) := True_Str;
         Last := Last + True_Str'Length;
      else
         Statement_Buffer (Last + 1 .. Last + False_Str'Length) := False_Str;
         Last := Last + False_Str'Length;
      end if;
   end Set_Boolean;

   --------------
   -- Set_Char --
   --------------

   procedure Set_Char (C : Character) is
   begin
      Last := Last + 1;
      Statement_Buffer (Last) := C;
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

         Last := Last + 1;
         Statement_Buffer (Last) :=
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

      --  First we output the initial _ada_ since we know that the main
      --  program is a library level subprogram.

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
      Statement_Buffer (Last + 1 .. Last + S'Length) := S;
      Last := Last + S'Length;
   end Set_String;

   ------------------------
   -- Set_String_Replace --
   ------------------------

   procedure Set_String_Replace (S : String) is
   begin
      Statement_Buffer (Last - S'Length + 1 .. Last) := S;
   end Set_String_Replace;

   -------------------
   -- Set_Unit_Name --
   -------------------

   procedure Set_Unit_Name is
   begin
      for J in 1 .. Name_Len - 2 loop
         if Name_Buffer (J) /= '.' then
            Set_Char (Name_Buffer (J));
         else
            Set_String ("__");
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

   ----------------------
   -- Write_Info_Ada_C --
   ----------------------

   procedure Write_Info_Ada_C (Ada : String; C : String; Common : String) is
   begin
      if Ada_Bind_File then
         declare
            S : String (1 .. Ada'Length + Common'Length);
         begin
            S (1 .. Ada'Length) := Ada;
            S (Ada'Length + 1 .. S'Length) := Common;
            WBI (S);
         end;

      else
         declare
            S : String (1 .. C'Length + Common'Length);
         begin
            S (1 .. C'Length) := C;
            S (C'Length + 1 .. S'Length) := Common;
            WBI (S);
         end;
      end if;
   end Write_Info_Ada_C;

   ----------------------------
   -- Write_Statement_Buffer --
   ----------------------------

   procedure Write_Statement_Buffer is
   begin
      WBI (Statement_Buffer (1 .. Last));
      Last := 0;
   end Write_Statement_Buffer;

   procedure Write_Statement_Buffer (S : String) is
   begin
      Set_String (S);
      Write_Statement_Buffer;
   end Write_Statement_Buffer;

end Bindgen;
