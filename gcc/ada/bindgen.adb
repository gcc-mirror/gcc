------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D G E N                               --
--                                                                          --
--                                 B o d y                                  --
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

with ALI;         use ALI;
with Binde;       use Binde;
with Casing;      use Casing;
with Fname;       use Fname;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gnatvsn;     use Gnatvsn;
with Hostparm;
with Namet;       use Namet;
with Opt;         use Opt;
with Osint;       use Osint;
with Osint.B;     use Osint.B;
with Output;      use Output;
with Rident;      use Rident;
with Table;       use Table;
with Targparm;    use Targparm;
with Types;       use Types;

with GNAT.Heap_Sort_A;     use GNAT.Heap_Sort_A;

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

   ----------------------------------
   -- Interface_State Pragma Table --
   ----------------------------------

   --  This table assembles the interface state pragma information from
   --  all the units in the partition. Note that Bcheck has already checked
   --  that the information is consistent across partitions. The entries
   --  in this table are n/u/r/s for not set/user/runtime/system.

   package IS_Pragma_Settings is new Table.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "IS_Pragma_Settings");

   ----------------------
   -- Run-Time Globals --
   ----------------------

   --  This section documents the global variables that are passed to the
   --  run time from the generated binder file. The call that is made is
   --  to the routine Set_Globals, which has the following spec:

   --   procedure Set_Globals
   --     (Main_Priority            : Integer;
   --      Time_Slice_Value         : Integer;
   --      WC_Encoding              : Character;
   --      Locking_Policy           : Character;
   --      Queuing_Policy           : Character;
   --      Task_Dispatching_Policy  : Character;
   --      Restrictions             : System.Address;
   --      Interrupt_States         : System.Address;
   --      Num_Interrupt_States     : Integer;
   --      Unreserve_All_Interrupts : Integer;
   --      Exception_Tracebacks     : Integer;
   --      Zero_Cost_Exceptions     : Integer;
   --      Detect_Blocking          : Integer);

   --  Main_Priority is the priority value set by pragma Priority in the
   --  main program. If no such pragma is present, the value is -1.

   --  Time_Slice_Value is the time slice value set by pragma Time_Slice
   --  in the main program, or by the use of a -Tnnn parameter for the
   --  binder (if both are present, the binder value overrides). The
   --  value is in milliseconds. A value of zero indicates that time
   --  slicing should be suppressed. If no pragma is present, and no
   --  -T switch was used, the value is -1.

   --  WC_Encoding shows the wide character encoding method used for
   --  the main program. This is one of the encoding letters defined
   --  in System.WCh_Con.WC_Encoding_Letters.

   --  Locking_Policy is a space if no locking policy was specified
   --  for the partition. If a locking policy was specified, the value
   --  is the upper case first character of the locking policy name,
   --  for example, 'C' for Ceiling_Locking.

   --  Queuing_Policy is a space if no queuing policy was specified
   --  for the partition. If a queuing policy was specified, the value
   --  is the upper case first character of the queuing policy name
   --  for example, 'F' for FIFO_Queuing.

   --  Task_Dispatching_Policy is a space if no task dispatching policy
   --  was specified for the partition. If a task dispatching policy
   --  was specified, the value is the upper case first character of
   --  the policy name, e.g. 'F' for FIFO_Within_Priorities.

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
   --  possible settings: 'n' for not specified, 'u' for user, 'r' for
   --  run time, 's' for system, see description of Interrupt_State pragma
   --  for further details.

   --  Num_Interrupt_States is the length of the Interrupt_States string.
   --  It will be set to zero if no Interrupt_State pragmas are present.

   --  Unreserve_All_Interrupts is set to one if at least one unit in the
   --  partition had a pragma Unreserve_All_Interrupts, and zero otherwise.

   --  Exception_Tracebacks is set to one if the -E parameter was present
   --  in the bind and to zero otherwise. Note that on some targets exception
   --  tracebacks are provided by default, so a value of zero for this
   --  parameter does not necessarily mean no trace backs are available.

   --  Zero_Cost_Exceptions is set to one if zero cost exceptions are used for
   --  this partition, and to zero if longjmp/setjmp exceptions are used.
   --  the use of zero

   --  Detect_Blocking indicates whether pragma Detect_Blocking is
   --  active or not. A value of zero indicates that the pragma is not
   --  present, while a value of 1 signals its presence in the
   --  partition.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure WBI (Info : String) renames Osint.B.Write_Binder_Info;
   --  Convenient shorthand used throughout

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

   procedure Gen_Restrictions_String_1;
   --  Generate first restrictions string, which consists of the parameters
   --  the first R line, as described in lib-writ.ads, with the restrictions
   --  being those for the entire partition (from Cumulative_Restrictions).

   procedure Gen_Restrictions_String_2;
   --  Generate first restrictions string, which consists of the parameters
   --  the second R line, as described in lib-writ.ads, with the restrictions
   --  being those for the entire partition (from Cumulative_Restrictions).

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

   function Get_Main_Name return String;
   --  This function is used in the Ada main output case to compute the
   --  correct external main program. It is "main" by default, unless the
   --  flag Use_Ada_Main_Program_Name_On_Target is set, in which case it
   --  is the name of the Ada main name without the "_ada". This default
   --  can be overridden explicitly using the -Mname binder switch.

   function Lt_Linker_Option (Op1, Op2 : Natural) return Boolean;
   --  Compare linker options, when sorting, first according to
   --  Is_Internal_File (internal files come later) and then by
   --  elaboration order position (latest to earliest).

   procedure Move_Linker_Option (From : Natural; To : Natural);
   --  Move routine for sorting linker options

   procedure Public_Version_Warning;
   --  Emit a warning concerning the use of the Public version under
   --  certain circumstances. See details in body.

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

   procedure Set_IS_Pragma_Table;
   --  Initializes contents of IS_Pragma_Settings table from ALI table

   procedure Set_Main_Program_Name;
   --  Given the main program name in Name_Buffer (length in Name_Len)
   --  generate the name of the routine to be used in the call. The name
   --  is generated starting at Last + 1, and Last is updated past it.

   procedure Set_Name_Buffer;
   --  Set the value stored in positions 1 .. Name_Len of the Name_Buffer

   procedure Set_String (S : String);
   --  Sets characters of given string in Statement_Buffer, starting at the
   --  Last + 1 position, and updating last past the string value.

   procedure Set_Unit_Name;
   --  Given a unit name in the Name_Buffer, copies it to Statement_Buffer,
   --  starting at the Last + 1 position, and updating last past the value.
   --  changing periods to double underscores, and updating Last appropriately.

   procedure Set_Unit_Number (U : Unit_Id);
   --  Sets unit number (first unit is 1, leading zeroes output to line
   --  up all output unit numbers nicely as required by the value, and
   --  by the total number of units.

   procedure Tab_To (N : Natural);
   --  If Last is greater than or equal to N, no effect, otherwise store
   --  blanks in Statement_Buffer bumping Last, until Last = N.

   procedure Write_Info_Ada_C (Ada : String; C : String; Common : String);
   --  For C code case, write C & Common, for Ada case write Ada & Common
   --  to current binder output file using Write_Binder_Info.

   procedure Write_Statement_Buffer;
   --  Write out contents of statement buffer up to Last, and reset Last to 0

   procedure Write_Statement_Buffer (S : String);
   --  First writes its argument (using Set_String (S)), then writes out the
   --  contents of statement buffer up to Last, and reset Last to 0

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

      if Hostparm.Java_VM then
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
      WBI ("void " & Ada_Final_Name.all & " () {");
      WBI ("   system__standard_library__adafinal ();");
      WBI ("}");
      WBI ("");
   end Gen_Adafinal_C;

   ---------------------
   -- Gen_Adainit_Ada --
   ---------------------

   procedure Gen_Adainit_Ada is
      Main_Priority : Int renames ALIs.Table (ALIs.First).Main_Priority;

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
               Set_String (" : Boolean; pragma Import (Ada, ");
               Set_String ("E");
               Set_Unit_Number (Unum);
               Set_String (", """);
               Get_Name_String (U.Uname);

               --  In the case of JGNAT we need to emit an Import name
               --  that includes the class name (using '$' separators
               --  in the case of a child unit name).

               if Hostparm.Java_VM then
                  for J in 1 .. Name_Len - 2 loop
                     if Name_Buffer (J) /= '.' then
                        Set_Char (Name_Buffer (J));
                     else
                        Set_String ("$");
                     end if;
                  end loop;

                  Set_String (".");

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

      --  If the standard library is suppressed, then the only global variable
      --  that might be needed (by the Ravenscar profile) is the priority of
      --  the environment. Also no exception tables are needed.

      if Suppress_Standard_Library_On_Target then
         if Main_Priority /= No_Main_Priority then
            WBI ("      Main_Priority : Integer;");
            WBI ("      pragma Import (C, Main_Priority," &
                 " ""__gl_main_priority"");");
            WBI ("");
         end if;

         WBI ("   begin");

         if Main_Priority /= No_Main_Priority then
            Set_String ("      Main_Priority := ");
            Set_Int    (Main_Priority);
            Set_Char   (';');
            Write_Statement_Buffer;

         else
            WBI ("      null;");
         end if;

      --  Normal case (standard library not suppressed). Global values are
      --  assigned using the runtime routine Set_Globals (we have to use
      --  the routine call, rather than define the globals in the binder
      --  file to deal with cross-library calls in some systems.

      else
         --  Generate restrictions string

         Set_String ("      Restrictions : constant String :=");
         Write_Statement_Buffer;

         Set_String ("        """);
         Gen_Restrictions_String_1;
         Set_String (""" &");
         Write_Statement_Buffer;

         Set_String ("        """);
         Gen_Restrictions_String_2;
         Set_String (""" & ASCII.Nul;");
         Write_Statement_Buffer;
         WBI ("");

         --  Generate Interrupt_State pragma string

         Set_String ("      Interrupt_States : constant String :=");
         Write_Statement_Buffer;

         declare
            Col : Natural;

         begin
            Set_String ("        """);
            Col := 9;

            for J in 0 .. IS_Pragma_Settings.Last loop
               if Col > 72 then
                  Set_String (""" &");
                  Write_Statement_Buffer;
                  Set_String ("        """);
                  Col := 9;

               else
                  Col := Col + 1;
               end if;

               Set_Char (IS_Pragma_Settings.Table (J));
            end loop;
         end;

         Set_String (""";");
         Write_Statement_Buffer;
         WBI ("");

         --  Generate spec for Set_Globals procedure

         WBI ("      procedure Set_Globals");
         WBI ("        (Main_Priority            : Integer;");
         WBI ("         Time_Slice_Value         : Integer;");
         WBI ("         WC_Encoding              : Character;");
         WBI ("         Locking_Policy           : Character;");
         WBI ("         Queuing_Policy           : Character;");
         WBI ("         Task_Dispatching_Policy  : Character;");

         WBI ("         Restrictions             : System.Address;");
         WBI ("         Interrupt_States         : System.Address;");
         WBI ("         Num_Interrupt_States     : Integer;");
         WBI ("         Unreserve_All_Interrupts : Integer;");
         WBI ("         Exception_Tracebacks     : Integer;");
         WBI ("         Zero_Cost_Exceptions     : Integer;");
         WBI ("         Detect_Blocking          : Integer);");
         WBI ("      pragma Import (C, Set_Globals, ""__gnat_set_globals"");");

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
         WBI ("   begin");

         --  Generate the call to Set_Globals

         WBI ("      Set_Globals");

         Set_String ("        (Main_Priority            => ");
         Set_Int    (Main_Priority);
         Set_Char   (',');
         Write_Statement_Buffer;

         Set_String ("         Time_Slice_Value         => ");

         if Task_Dispatching_Policy_Specified = 'F'
           and then ALIs.Table (ALIs.First).Time_Slice_Value = -1
         then
            Set_Int (0);
         else
            Set_Int (ALIs.Table (ALIs.First).Time_Slice_Value);
         end if;

         Set_Char   (',');
         Write_Statement_Buffer;

         Set_String ("         WC_Encoding              => '");
         Set_Char   (ALIs.Table (ALIs.First).WC_Encoding);
         Set_String ("',");
         Write_Statement_Buffer;

         Set_String ("         Locking_Policy           => '");
         Set_Char   (Locking_Policy_Specified);
         Set_String ("',");
         Write_Statement_Buffer;

         Set_String ("         Queuing_Policy           => '");
         Set_Char   (Queuing_Policy_Specified);
         Set_String ("',");
         Write_Statement_Buffer;

         Set_String ("         Task_Dispatching_Policy  => '");
         Set_Char   (Task_Dispatching_Policy_Specified);
         Set_String ("',");
         Write_Statement_Buffer;

         WBI ("         Restrictions             => Restrictions'Address,");

         WBI ("         Interrupt_States         => " &
                                                 "Interrupt_States'Address,");

         Set_String ("         Num_Interrupt_States     => ");
         Set_Int (IS_Pragma_Settings.Last + 1);
         Set_Char (',');
         Write_Statement_Buffer;

         Set_String ("         Unreserve_All_Interrupts => ");

         if Unreserve_All_Interrupts_Specified then
            Set_String ("1");
         else
            Set_String ("0");
         end if;

         Set_Char (',');
         Write_Statement_Buffer;

         Set_String ("         Exception_Tracebacks     => ");

         if Exception_Tracebacks then
            Set_String ("1");
         else
            Set_String ("0");
         end if;

         Set_String (",");
         Write_Statement_Buffer;

         Set_String ("         Zero_Cost_Exceptions     => ");

         if Zero_Cost_Exceptions_Specified then
            Set_String ("1");
         else
            Set_String ("0");
         end if;

         Set_String (",");
         Write_Statement_Buffer;

         Set_String ("         Detect_Blocking          => ");

         if Detect_Blocking then
            Set_Int (1);
         else
            Set_Int (0);
         end if;

         Set_String (");");
         Write_Statement_Buffer;

         --  Generate call to Install_Handler

         WBI ("");
         WBI ("      if Handler_Installed = 0 then");
         WBI ("         Install_Handler;");
         WBI ("      end if;");
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

      --  Generate elaboration calls

      WBI ("");
      Gen_Elab_Calls_Ada;

      WBI ("   end " & Ada_Init_Name.all & ";");
   end Gen_Adainit_Ada;

   -------------------
   -- Gen_Adainit_C --
   --------------------

   procedure Gen_Adainit_C is
      Main_Priority : Int renames ALIs.Table (ALIs.First).Main_Priority;

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

         --  Case of High_Integrity_Mode mode. Set __gl_main_priority if needed
         --  for the Ravenscar profile.

         if Main_Priority /= No_Main_Priority then
            Set_String ("   extern int __gl_main_priority = ");
            Set_Int    (Main_Priority);
            Set_Char   (';');
            Write_Statement_Buffer;
         end if;

      --  Normal case (standard library not suppressed)

      else
         --  Generate definition for restrictions string

         Set_String ("   const char *restrictions = """);
         Gen_Restrictions_String_1;
         Gen_Restrictions_String_2;
         Set_String (""";");
         Write_Statement_Buffer;

         --  Generate definition for interrupt states string

         Set_String ("   const char *interrupt_states = """);

         for J in 0 .. IS_Pragma_Settings.Last loop
            Set_Char (IS_Pragma_Settings.Table (J));
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

         --  Generate call to set the runtime global variables defined in
         --  init.c. We define the varables in init.c, rather than in
         --  the binder generated file itself to avoid undefined externals
         --  when the runtime is linked as a shareable image library.

         --  We call the routine from inside adainit() because this works for
         --  both programs with and without binder generated "main" functions.

         WBI ("   __gnat_set_globals (");

         Set_String ("      ");
         Set_Int (Main_Priority);
         Set_Char (',');
         Tab_To (24);
         Set_String ("/* Main_Priority              */");
         Write_Statement_Buffer;

         Set_String ("      ");

         if Task_Dispatching_Policy = 'F'
           and then ALIs.Table (ALIs.First).Time_Slice_Value = -1
         then
            Set_Int (0);
         else
            Set_Int (ALIs.Table (ALIs.First).Time_Slice_Value);
         end if;

         Set_Char   (',');
         Tab_To (24);
         Set_String ("/* Time_Slice_Value           */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char   (ALIs.Table (ALIs.First).WC_Encoding);
         Set_String ("',");
         Tab_To (24);
         Set_String ("/* WC_Encoding                */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char (Locking_Policy_Specified);
         Set_String ("',");
         Tab_To (24);
         Set_String ("/* Locking_Policy             */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char (Queuing_Policy_Specified);
         Set_String ("',");
         Tab_To (24);
         Set_String ("/* Queuing_Policy             */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char (Task_Dispatching_Policy_Specified);
         Set_String ("',");
         Tab_To (24);
         Set_String ("/* Tasking_Dispatching_Policy */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_String ("restrictions");
         Set_String (",");
         Tab_To (24);
         Set_String ("/* Restrictions               */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_String ("interrupt_states");
         Set_String (",");
         Tab_To (24);
         Set_String ("/* Interrupt_States           */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_Int (IS_Pragma_Settings.Last + 1);
         Set_String (",");
         Tab_To (24);
         Set_String ("/* Num_Interrupt_States       */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_Int    (Boolean'Pos (Unreserve_All_Interrupts_Specified));
         Set_String (",");
         Tab_To (24);
         Set_String ("/* Unreserve_All_Interrupts   */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_Int    (Boolean'Pos (Exception_Tracebacks));
         Set_String (",");
         Tab_To (24);
         Set_String ("/* Exception_Tracebacks       */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_Int    (Boolean'Pos (Zero_Cost_Exceptions_Specified));
         Set_String (",");
         Tab_To (24);
         Set_String ("/* Zero_Cost_Exceptions       */");
         Write_Statement_Buffer;

         Set_String ("      ");

         if Detect_Blocking then
            Set_Int (1);
         else
            Set_Int (0);
         end if;

         Set_String (");");
         Tab_To (24);
         Set_String ("/* Detect_Blocking            */");
         Write_Statement_Buffer;
         WBI ("");

         --  Install elaboration time signal handler

         WBI ("   if (__gnat_handler_installed == 0)");
         WBI ("     {");
         WBI ("        __gnat_install_handler ();");
         WBI ("     }");
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

               if Name_Buffer (Name_Len) = 's' then
                  Name_Buffer (Name_Len - 1 .. Name_Len + 8) := "'elab_spec";
               else
                  Name_Buffer (Name_Len - 1 .. Name_Len + 8) := "'elab_body";
               end if;

               Name_Len := Name_Len + 8;
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

      if Bind_Main_Program then
         WBI
           ("      Ensure_Reference : System.Address := " &
            "Ada_Main_Program_Name'Address;");
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

         if Hostparm.Java_VM then
            WBI ("      System.Standard_Library.Adafinal;");
         else
            WBI ("      Do_Finalize;");
         end if;
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
         Write_Statement_Buffer (" ()");
      end if;

      WBI ("{");

      --  Generate a reference to __gnat_ada_main_program_name. This symbol
      --  is  not referenced elsewhere in the generated program, but is
      --  needed by the debugger (that's why it is generated in the first
      --  place). The reference stops Ada_Main_Program_Name from being
      --  optimized away by smart linkers, such as the AiX linker.

      if Bind_Main_Program then
         WBI ("   char *ensure_reference __attribute__ ((__unused__)) = " &
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
         WBI ("   int result;");
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

      for E in Elab_Order.First .. Elab_Order.Last loop

         --  If not spec that has an associated body, then generate a
         --  comment giving the name of the corresponding object file.

         if (not Units.Table (Elab_Order.Table (E)).SAL_Interface)
           and then Units.Table (Elab_Order.Table (E)).Utype /= Is_Spec
         then
            Get_Name_String
              (ALIs.Table
                (Units.Table (Elab_Order.Table (E)).My_ALI).Ofile_Full_Name);

            --  If the presence of an object file is necessary or if it
            --  exists, then use it.

            if not Hostparm.Exclude_Missing_Objects
              or else GNAT.OS_Lib.Is_Regular_File (Name_Buffer (1 .. Name_Len))
            then
               Write_Info_Ada_C ("   --   ", "", Name_Buffer (1 .. Name_Len));

               if Output_Object_List then
                  Write_Str (Name_Buffer (1 .. Name_Len));
                  Write_Eol;
               end if;

               --  Don't link with the shared library on VMS if an internal
               --  filename object is seen. Multiply defined symbols will
               --  result.

               if Hostparm.OpenVMS
                 and then Is_Internal_File_Name
                  (ALIs.Table
                   (Units.Table (Elab_Order.Table (E)).My_ALI).Sfile)
               then
                  --  Special case for g-trasym.obj, which is not included
                  --  in libgnat.

                  Get_Name_String (ALIs.Table
                            (Units.Table (Elab_Order.Table (E)).My_ALI).Sfile);

                  if Name_Buffer (1 .. 8) /= "g-trasym" then
                     Opt.Shared_Libgnat := False;
                  end if;
               end if;
            end if;
         end if;
      end loop;

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

      --  Sort linker options

      --  This sort accomplishes two important purposes:

      --    a) All application files are sorted to the front, and all
      --       GNAT internal files are sorted to the end. This results
      --       in a well defined dividing line between the two sets of
      --       files, for the purpose of inserting certain standard
      --       library references into the linker arguments list.

      --    b) Given two different units, we sort the linker options so
      --       that those from a unit earlier in the elaboration order
      --       comes later in the list. This is a heuristic designed
      --       to create a more friendly order of linker options when
      --       the operations appear in separate units. The idea is that
      --       if unit A must be elaborated before unit B, then it is
      --       more likely that B references libraries included by A,
      --       than vice versa, so we want the libraries included by
      --       A to come after the libraries included by B.

      --  These two criteria are implemented by function Lt_Linker_Option.
      --  Note that a special case of b) is that specs are elaborated before
      --  bodies, so linker options from specs come after linker options
      --  for bodies, and again, the assumption is that libraries used by
      --  the body are more likely to reference libraries used by the spec,
      --  than vice versa.

      Sort
        (Linker_Options.Last,
         Move_Linker_Option'Access,
         Lt_Linker_Option'Access);

      --  Write user linker options, i.e. the set of linker options that
      --  come from all files other than GNAT internal files, Lgnat is
      --  left set to point to the first entry from a GNAT internal file,
      --  or past the end of the entriers if there are no internal files.

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

      if not Opt.No_Stdlib then
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
            Add_Str_To_Name_Buffer ("-ldecgnat");
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
      Is_Public_Version : constant Boolean := Get_Gnat_Build_Type = Public;
      Is_GAP_Version    : constant Boolean := Get_Gnat_Build_Type = GAP;

   begin
      --  Acquire settings for Interrupt_State pragmas

      Set_IS_Pragma_Table;

      --  Override Ada_Bind_File and Bind_Main_Program for Java since
      --  JGNAT only supports Ada code, and the main program is already
      --  generated by the compiler.

      if Hostparm.Java_VM then
         Ada_Bind_File := True;
         Bind_Main_Program := False;
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

      --  Get the time stamp of the former bind for public version warning

      if Is_Public_Version or Is_GAP_Version then
         Record_Time_From_Last_Bind;
      end if;

      --  Generate output file in appropriate language

      if Ada_Bind_File then
         Gen_Output_File_Ada (Filename);
      else
         Gen_Output_File_C (Filename);
      end if;

      --  Periodically issue a warning when the public version is used on
      --  big projects

      if Is_Public_Version then
         Public_Version_Warning;
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

      --  If we are operating in Restrictions (No_Exception_Handlers) mode,
      --  then we need to make sure that the binder program is compiled with
      --  the same restriction, so that no exception tables are generated.

      if Cumulative_Restrictions.Set (No_Exception_Handlers) then
         WBI ("pragma Restrictions (No_Exception_Handlers);");
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

      if not Suppress_Standard_Library_On_Target then

         --  Usually, adafinal is called using a pragma Import C. Since
         --  Import C doesn't have the same semantics for JGNAT, we use
         --  standard Ada.

         if Hostparm.Java_VM then
            WBI ("with System.Standard_Library;");
         end if;
      end if;

      WBI ("package " & Ada_Main & " is");
      WBI ("   pragma Warnings (Off);");

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

            --  If the standard library is not suppressed, these variables are
            --  in the runtime data area for easy access from the runtime

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
      --  to a symbol duplication during the link (for instance when a
      --  C program uses 2 Ada libraries)

      if Bind_Main_Program then
         WBI ("");
         WBI ("   GNAT_Version : constant String :=");
         WBI ("                    ""GNAT Version: " &
                                   Gnat_Version_String & """;");
         WBI ("   pragma Export (C, GNAT_Version, ""__gnat_version"");");

         WBI ("");
         Set_String ("   Ada_Main_Program_Name : constant String := """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (""" & Ascii.NUL;");
         Write_Statement_Buffer;

         WBI
           ("   pragma Export (C, Ada_Main_Program_Name, " &
            """__gnat_ada_main_program_name"");");
      end if;

      WBI ("");
      WBI ("   procedure " & Ada_Final_Name.all & ";");
      WBI ("   pragma Export (C, " & Ada_Final_Name.all & ", """ &
           Ada_Final_Name.all & """);");

      if Use_Pragma_Linker_Constructor then
         WBI ("   pragma Linker_Destructor (" & Ada_Final_Name.all & ");");
      end if;

      WBI ("");
      WBI ("   procedure " & Ada_Init_Name.all & ";");
      WBI ("   pragma Export (C, " & Ada_Init_Name.all & ", """ &
           Ada_Init_Name.all & """);");

      if Use_Pragma_Linker_Constructor then
         WBI ("   pragma Linker_Constructor (" & Ada_Init_Name.all & ");");
      end if;

      if Bind_Main_Program then

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

      WBI ("");
      WBI ("package body " & Ada_Main & " is");
      WBI ("   pragma Warnings (Off);");

      --  Import the finalization procedure only if finalization active

      if not Cumulative_Restrictions.Set (No_Finalization) then

         --  In the Java case, pragma Import C cannot be used, so the
         --  standard Ada constructs will be used instead.

         if not Hostparm.Java_VM then
            WBI ("");
            WBI ("   procedure Do_Finalize;");
            WBI
              ("   pragma Import (C, Do_Finalize, " &
               """system__standard_library__adafinal"");");
            WBI ("");
         end if;
      end if;

      Gen_Adainit_Ada;

      Gen_Adafinal_Ada;

      if Bind_Main_Program then

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
      --  Name of generated bind file

   begin
      Create_Binder_Output (Filename, 'c', Bfile);

      Resolve_Binder_Options;

      WBI ("extern void __gnat_set_globals");
      WBI ("  (int, int, char, char, char, char,");
      WBI ("   const char *, const char *,");
      WBI ("   int, int, int, int, int);");

      if Use_Pragma_Linker_Constructor then
         WBI ("extern void " & Ada_Final_Name.all &
              " (void) __attribute__((destructor));");
         WBI ("extern void " & Ada_Init_Name.all &
              " (void) __attribute__((constructor));");

      else
         WBI ("extern void " & Ada_Final_Name.all & " (void);");
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

      WBI ("");

      Gen_Elab_Defs_C;

      --  Imported variable used to track elaboration/finalization phase.
      --  Used only when we have a runtime.

      if not Suppress_Standard_Library_On_Target then
         WBI ("extern int  __gnat_handler_installed;");
         WBI ("");
      end if;

      --  Write argv/argc exit status stuff if main program case

      if Bind_Main_Program then

         --  First deal with argc/argv/envp. In the normal case they
         --  are in the run-time library.

         if not Configurable_Run_Time_On_Target then
            WBI ("extern int gnat_argc;");
            WBI ("extern char **gnat_argv;");
            WBI ("extern char **gnat_envp;");

         --  If configurable run time and no command line args, then the
         --  generation of these variables is entirely suppressed.

         elsif not Command_Line_Args_On_Target then
            null;

         --  Otherwise, in the configurable run-time case they are right in
         --  the binder file.

         else
            WBI ("int gnat_argc;");
            WBI ("char **gnat_argv;");
            WBI ("char **gnat_envp;");
         end if;

         --  Similarly deal with exit status
         --  are in the run-time library.

         if not Configurable_Run_Time_On_Target then
            WBI ("extern int gnat_exit_status;");

         --  If configurable run time and no exit status on target, then
         --  the generation of this variables is entirely suppressed.

         elsif not Exit_Status_Supported_On_Target then
            null;

         --  Otherwise, in the configurable run-time case this variable is
         --  right in the binder file, and initialized to zero there.

         else
            WBI ("int gnat_exit_status = 0;");
         end if;

         WBI ("");
      end if;

      --  When suppressing the standard library, the __gnat_break_start
      --  routine (for the debugger to get initial control) is defined in
      --  this file.

      if Suppress_Standard_Library_On_Target then
         WBI ("");
         WBI ("void __gnat_break_start () {}");
      end if;

      --  Generate the __gnat_version and __gnat_ada_main_program_name info
      --  only for the main program. Otherwise, it can lead under some
      --  circumstances to a symbol duplication during the link (for instance
      --  when a C program uses 2 Ada libraries)

      if Bind_Main_Program then
         WBI ("");
         WBI ("char __gnat_version[] = ""GNAT Version: " &
                                   Gnat_Version_String & """;");

         Set_String ("char __gnat_ada_main_program_name[] = """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (""";");
         Write_Statement_Buffer;
      end if;

      --  Generate the adafinal routine. In no runtime mode, this is
      --  not needed, since there is no finalization to do.

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

   -------------------------------
   -- Gen_Restrictions_String_1 --
   -------------------------------

   procedure Gen_Restrictions_String_1 is
   begin
      for R in All_Boolean_Restrictions loop
         if Cumulative_Restrictions.Set (R) then
            Set_Char ('r');
         elsif Cumulative_Restrictions.Violated (R) then
            Set_Char ('v');
         else
            Set_Char ('n');
         end if;
      end loop;
   end Gen_Restrictions_String_1;

   -------------------------------
   -- Gen_Restrictions_String_2 --
   -------------------------------

   procedure Gen_Restrictions_String_2 is
   begin
      for RP in All_Parameter_Restrictions loop
         if Cumulative_Restrictions.Set (RP) then
            Set_Char ('r');
            Set_Int (Int (Cumulative_Restrictions.Value (RP)));
         else
            Set_Char ('n');
         end if;

         if not Cumulative_Restrictions.Violated (RP)
           or else RP not in Checked_Parameter_Restrictions
         then
            Set_Char ('n');
         else
            Set_Char ('v');
            Set_Int (Int (Cumulative_Restrictions.Count (RP)));

            if Cumulative_Restrictions.Unknown (RP) then
               Set_Char ('+');
            end if;
         end if;
      end loop;
   end Gen_Restrictions_String_2;

   ----------------------
   -- Gen_Versions_Ada --
   ----------------------

   --  This routine generates two sets of lines. The first set has the form:

   --    unnnnn : constant Integer := 16#hhhhhhhh#;

   --  The second set has the form

   --    pragma Export (C, unnnnn, unam);

   --  for each unit, where unam is the unit name suffixed by either B or
   --  S for body or spec, with dots replaced by double underscores, and
   --  hhhhhhhh is the version number, and nnnnn is a 5-digits serial number.

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
      if Bind_For_Library then

         --  When building libraries, the version number of each unit can
         --  not be computed, since the binder does not know the full list
         --  of units. Therefore, the 'Version and 'Body_Version
         --  attributes can not supported in this case.

         return;
      end if;

      WBI ("");

      WBI ("   type Version_32 is mod 2 ** 32;");
      for U in Units.First .. Units.Last loop
         Increment_Ubuf;
         WBI ("   " & Ubuf & " : constant Version_32 := 16#" &
              Units.Table (U).Version & "#;");
      end loop;

      WBI ("");
      Ubuf := "u00000";

      for U in Units.First .. Units.Last loop
         Increment_Ubuf;
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
      end loop;

   end Gen_Versions_Ada;

   --------------------
   -- Gen_Versions_C --
   --------------------

   --  This routine generates a line of the form:

   --    unsigned unam = 0xhhhhhhhh;

   --  for each unit, where unam is the unit name suffixed by either B or
   --  S for body or spec, with dots replaced by double underscores.

   procedure Gen_Versions_C is
   begin
      if Bind_For_Library then

         --  When building libraries, the version number of each unit can
         --  not be computed, since the binder does not know the full list
         --  of units. Therefore, the 'Version and 'Body_Version
         --  attributes can not supported.

         return;
      end if;

      for U in Units.First .. Units.Last loop
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
      end loop;

   end Gen_Versions_C;

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

      if Hostparm.Java_VM then
         --  Get main program name

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         --  Remove the %b

         return "ada_" & Name_Buffer (1 .. Name_Len - 2);
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

         --  If this is a child name, return only the name of the child,
         --  since we can't have dots in a nested program name. Note that
         --  we do not include the %b at the end of the unit name.

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
      --  elaboration position. A unit that is elaborated later should
      --  come earlier in the linker options list.

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
   -- Public_Version_Warning --
   ----------------------------

   procedure Public_Version_Warning is
      Time : constant Int := Time_From_Last_Bind;

      --  Constants to help defining periods

      Hour : constant := 60;
      Day  : constant := 24 * Hour;

      Never : constant := Integer'Last;
      --  Special value indicating no warnings should be given

      --  Constants defining when the warning is issued. Programs with more
      --  than Large Units will issue a warning every Period_Large amount of
      --  time. Smaller programs will generate a warning every Period_Small
      --  amount of time.

      Large : constant := 20;
      --  Threshold for considering a program small or large

      Period_Large : constant := Day;
      --  Periodic warning time for large programs

      Period_Small : constant := Never;
      --  Periodic warning time for small programs

      Nb_Unit : Int;

   begin
      --  Compute the number of units that are not GNAT internal files

      Nb_Unit := 0;
      for A in ALIs.First .. ALIs.Last loop
         if not Is_Internal_File_Name (ALIs.Table (A).Sfile) then
            Nb_Unit := Nb_Unit + 1;
         end if;
      end loop;

      --  Do not emit the message if the last message was emitted in the
      --  specified period taking into account the number of units.

      pragma Warnings (Off);
      --  Turn off warning of constant condition, which may happen here
      --  depending on the choice of constants in the above declarations.

      if Nb_Unit < Large and then Time <= Period_Small then
         return;
      elsif Time <= Period_Large then
         return;
      end if;

      pragma Warnings (On);

      Write_Eol;
      Write_Str ("IMPORTANT NOTICE:");
      Write_Eol;
      Write_Str ("    This version of GNAT is unsupported"
        &                        " and comes with absolutely no warranty.");
      Write_Eol;
      Write_Str ("    If you intend to evaluate or use GNAT for building "
        &                                       "commercial applications,");
      Write_Eol;
      Write_Str ("    please consult http://www.gnat.com/ for information");
      Write_Eol;
      Write_Str ("    on the GNAT Professional product line.");
      Write_Eol;
      Write_Eol;
   end Public_Version_Warning;

   ----------------------------
   -- Resolve_Binder_Options --
   ----------------------------

   procedure Resolve_Binder_Options is
   begin
      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);

         --  The procedure of looking for specific packages and setting
         --  flags is somewhat dubious, but there isn't a good alternative
         --  at the current time ???

         if Name_Buffer (1 .. 19) = "system.os_interface" then
            With_GNARL := True;
         end if;

         if Hostparm.OpenVMS and then Name_Buffer (1 .. 5) = "dec%s" then
            With_DECGNAT := True;
         end if;
      end loop;
   end Resolve_Binder_Options;

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

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (S : String) is
   begin
      Statement_Buffer (Last + 1 .. Last + S'Length) := S;
      Last := Last + S'Length;
   end Set_String;

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

   ------------
   -- Tab_To --
   ------------

   procedure Tab_To (N : Natural) is
   begin
      while Last < N loop
         Set_Char (' ');
      end loop;
   end Tab_To;

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
