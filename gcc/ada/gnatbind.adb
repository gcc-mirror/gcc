------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T B I N D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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
with ALI.Util; use ALI.Util;
with Bcheck;   use Bcheck;
with Binde;    use Binde;
with Binderr;  use Binderr;
with Bindgen;  use Bindgen;
with Bindusg;
with Casing;   use Casing;
with Csets;
with Debug;    use Debug;
with Fmap;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.B;  use Osint.B;
with Output;   use Output;
with Rident;   use Rident;
with Snames;
with Switch;   use Switch;
with Switch.B; use Switch.B;
with Targparm; use Targparm;
with Types;    use Types;

with System.Case_Util; use System.Case_Util;
with System.OS_Lib;    use System.OS_Lib;

with Ada.Command_Line.Response_File; use Ada.Command_Line;

procedure Gnatbind is

   Total_Errors : Nat := 0;
   --  Counts total errors in all files

   Total_Warnings : Nat := 0;
   --  Total warnings in all files

   Main_Lib_File : File_Name_Type;
   --  Current main library file

   First_Main_Lib_File : File_Name_Type := No_File;
   --  The first library file, that should be a main subprogram if neither -n
   --  nor -z are used.

   Text : Text_Buffer_Ptr;

   Output_File_Name_Seen : Boolean := False;
   Output_File_Name      : String_Ptr := new String'("");

   Mapping_File : String_Ptr := null;

   procedure Add_Artificial_ALI_File (Name : String);
   --  Artificially add ALI file Name in the closure

   function Gnatbind_Supports_Auto_Init return Boolean;
   --  Indicates if automatic initialization of elaboration procedure through
   --  the constructor mechanism is possible on the platform.

   function Is_Cross_Compiler return Boolean;
   --  Returns True iff this is a cross-compiler

   procedure List_Applicable_Restrictions;
   --  List restrictions that apply to this partition if option taken

   procedure Scan_Bind_Arg (Argv : String);
   --  Scan and process binder specific arguments. Argv is a single argument.
   --  All the one character arguments are still handled by Switch. This
   --  routine handles -aO -aI and -I-. The lower bound of Argv must be 1.

   generic
      with procedure Action (Argv : String);
   procedure Generic_Scan_Bind_Args;
   --  Iterate through the args calling Action on each one, taking care of
   --  response files.

   procedure Write_Arg (S : String);
   --  Passed to Generic_Scan_Bind_Args to print args

   -----------------------------
   -- Add_Artificial_ALI_File --
   -----------------------------

   procedure Add_Artificial_ALI_File (Name : String) is
      Id : ALI_Id;
      pragma Warnings (Off, Id);

      Std_Lib_File : File_Name_Type;
      --  Standard library

   begin
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Std_Lib_File := Name_Find;
      Text := Read_Library_Info (Std_Lib_File, True);

      Id :=
        Scan_ALI
          (F             => Std_Lib_File,
           T             => Text,
           Ignore_ED     => False,
           Err           => False,
           Ignore_Errors => Debug_Flag_I);

      Free (Text);
   end Add_Artificial_ALI_File;

   ---------------------------------
   -- Gnatbind_Supports_Auto_Init --
   ---------------------------------

   function Gnatbind_Supports_Auto_Init return Boolean is
      function gnat_binder_supports_auto_init return Integer;
      pragma Import (C, gnat_binder_supports_auto_init,
                     "__gnat_binder_supports_auto_init");

   begin
      return gnat_binder_supports_auto_init /= 0;
   end Gnatbind_Supports_Auto_Init;

   -----------------------
   -- Is_Cross_Compiler --
   -----------------------

   function Is_Cross_Compiler return Boolean is
      Cross_Compiler : Integer;
      pragma Import (C, Cross_Compiler, "__gnat_is_cross_compiler");

   begin
      return Cross_Compiler = 1;
   end Is_Cross_Compiler;

   ----------------------------------
   -- List_Applicable_Restrictions --
   ----------------------------------

   procedure List_Applicable_Restrictions is

      --  Define those restrictions that should be output if the gnatbind
      --  -r switch is used. Not all restrictions are output for the reasons
      --  given below in the list, and this array is used to test whether
      --  the corresponding pragma should be listed. True means that it
      --  should not be listed.

      No_Restriction_List : constant array (All_Restrictions) of Boolean :=
        (No_Standard_Allocators_After_Elaboration => True,
         --  This involves run-time conditions not checkable at compile time

         No_Anonymous_Allocators         => True,
         --  Premature, since we have not implemented this yet

         No_Exception_Propagation        => True,
         --  Modifies code resulting in different exception semantics

         No_Exceptions                   => True,
         --  Has unexpected Suppress (All_Checks) effect

         No_Implicit_Conditionals        => True,
         --  This could modify and pessimize generated code

         No_Implicit_Dynamic_Code        => True,
         --  This could modify and pessimize generated code

         No_Implicit_Loops               => True,
         --  This could modify and pessimize generated code

         No_Recursion                    => True,
         --  Not checkable at compile time

         No_Reentrancy                   => True,
         --  Not checkable at compile time

         Max_Entry_Queue_Length           => True,
         --  Not checkable at compile time

         Max_Storage_At_Blocking         => True,
         --  Not checkable at compile time

         --  The following three should not be partition-wide, so the
         --  following tests are junk to be removed eventually ???

         No_Specification_Of_Aspect      => True,
         --  Requires a parameter value, not a count

         No_Use_Of_Attribute             => True,
         --  Requires a parameter value, not a count

         No_Use_Of_Pragma                => True,
         --  Requires a parameter value, not a count

         others                          => False);

      Additional_Restrictions_Listed : Boolean := False;
      --  Set True if we have listed header for restrictions

      function Restriction_Could_Be_Set (R : Restriction_Id) return Boolean;
      --  Returns True if the given restriction can be listed as an additional
      --  restriction that could be set.

      ------------------------------
      -- Restriction_Could_Be_Set --
      ------------------------------

      function Restriction_Could_Be_Set (R : Restriction_Id) return Boolean is
         CR : Restrictions_Info renames Cumulative_Restrictions;

      begin
         case R is

            --  Boolean restriction

            when All_Boolean_Restrictions =>

               --  The condition for listing a boolean restriction as an
               --  additional restriction that could be set is that it is
               --  not violated by any unit, and not already set.

               return CR.Violated (R) = False and then CR.Set (R) = False;

            --  Parameter restriction

            when All_Parameter_Restrictions =>

               --  If the restriction is violated and the level of violation is
               --  unknown, the restriction can definitely not be listed.

               if CR.Violated (R) and then CR.Unknown (R) then
                  return False;

               --  We can list the restriction if it is not set

               elsif not CR.Set (R) then
                  return True;

               --  We can list the restriction if is set to a greater value
               --  than the maximum value known for the violation.

               else
                  return CR.Value (R) > CR.Count (R);
               end if;

            --  No other values for R possible

            when others =>
               raise Program_Error;
         end case;
      end Restriction_Could_Be_Set;

   --  Start of processing for List_Applicable_Restrictions

   begin
      --  Loop through restrictions

      for R in All_Restrictions loop
         if not No_Restriction_List (R)
           and then Restriction_Could_Be_Set (R)
         then
            if not Additional_Restrictions_Listed then
               Write_Eol;
               Write_Line
                 ("The following additional restrictions may be applied to "
                  & "this partition:");
               Additional_Restrictions_Listed := True;
            end if;

            Write_Str ("pragma Restrictions (");

            declare
               S : constant String := Restriction_Id'Image (R);

            begin
               Name_Len := S'Length;
               Name_Buffer (1 .. Name_Len) := S;
            end;

            Set_Casing (Mixed_Case);
            Write_Str (Name_Buffer (1 .. Name_Len));

            if R in All_Parameter_Restrictions then
               Write_Str (" => ");
               Write_Int (Int (Cumulative_Restrictions.Count (R)));
            end if;

            Write_Str (");");
            Write_Eol;
         end if;
      end loop;
   end List_Applicable_Restrictions;

   -------------------
   -- Scan_Bind_Arg --
   -------------------

   procedure Scan_Bind_Arg (Argv : String) is
      pragma Assert (Argv'First = 1);

   begin
      --  Now scan arguments that are specific to the binder and are not
      --  handled by the common circuitry in Switch.

      if Opt.Output_File_Name_Present
        and then not Output_File_Name_Seen
      then
         Output_File_Name_Seen := True;

         if Argv'Length = 0
           or else (Argv'Length >= 1 and then Argv (1) = '-')
         then
            Fail ("output File_Name missing after -o");

         else
            Output_File_Name := new String'(Argv);
         end if;

      elsif Argv'Length >= 2 and then Argv (1) = '-' then

         --  -I-

         if Argv (2 .. Argv'Last) = "I-" then
            Opt.Look_In_Primary_Dir := False;

         --  -Idir

         elsif Argv (2) = 'I' then
            Add_Src_Search_Dir (Argv (3 .. Argv'Last));
            Add_Lib_Search_Dir (Argv (3 .. Argv'Last));

         --  -Ldir

         elsif Argv (2) = 'L' then
            if Argv'Length >= 3 then

               Opt.Bind_For_Library := True;
               Opt.Ada_Init_Name :=
                 new String'(Argv (3 .. Argv'Last) & Opt.Ada_Init_Suffix);
               Opt.Ada_Final_Name :=
                 new String'(Argv (3 .. Argv'Last) & Opt.Ada_Final_Suffix);
               Opt.Ada_Main_Name :=
                 new String'(Argv (3 .. Argv'Last) & Opt.Ada_Main_Name_Suffix);

               --  This option (-Lxxx) implies -n

               Opt.Bind_Main_Program := False;

            else
               Fail
                 ("Prefix of initialization and finalization procedure names "
                  & "missing in -L");
            end if;

         --  -Sin -Slo -Shi -Sxx -Sev

         elsif Argv'Length = 4
           and then Argv (2) = 'S'
         then
            declare
               C1 : Character := Argv (3);
               C2 : Character := Argv (4);

            begin
               --  Fold to upper case

               if C1 in 'a' .. 'z' then
                  C1 := Character'Val (Character'Pos (C1) - 32);
               end if;

               if C2 in 'a' .. 'z' then
                  C2 := Character'Val (Character'Pos (C2) - 32);
               end if;

               --  Test valid option and set mode accordingly

               if C1 = 'E' and then C2 = 'V' then
                  null;

               elsif C1 = 'I' and then C2 = 'N' then
                  null;

               elsif C1 = 'L' and then C2 = 'O' then
                  null;

               elsif C1 = 'H' and then C2 = 'I' then
                  null;

               elsif (C1 in '0' .. '9' or else C1 in 'A' .. 'F')
                       and then
                     (C2 in '0' .. '9' or else C2 in 'A' .. 'F')
               then
                  null;

               --  Invalid -S switch, let Switch give error, set default of IN

               else
                  Scan_Binder_Switches (Argv);
                  C1 := 'I';
                  C2 := 'N';
               end if;

               Initialize_Scalars_Mode1 := C1;
               Initialize_Scalars_Mode2 := C2;
            end;

         --  -aIdir

         elsif Argv'Length >= 3
           and then Argv (2 .. 3) = "aI"
         then
            Add_Src_Search_Dir (Argv (4 .. Argv'Last));

         --  -aOdir

         elsif Argv'Length >= 3
           and then Argv (2 .. 3) = "aO"
         then
            Add_Lib_Search_Dir (Argv (4 .. Argv'Last));

         --  -nostdlib

         elsif Argv (2 .. Argv'Last) = "nostdlib" then
            Opt.No_Stdlib := True;

         --  -nostdinc

         elsif Argv (2 .. Argv'Last) = "nostdinc" then
            Opt.No_Stdinc := True;

         --  -static

         elsif Argv (2 .. Argv'Last) = "static" then
            Opt.Shared_Libgnat := False;

         --  -shared

         elsif Argv (2 .. Argv'Last) = "shared" then
            Opt.Shared_Libgnat := True;

         --  -F=mapping_file

         elsif Argv'Length >= 4 and then Argv (2 .. 3) = "F=" then
            if Mapping_File /= null then
               Fail ("cannot specify several mapping files");
            end if;

            Mapping_File := new String'(Argv (4 .. Argv'Last));

         --  -Mname

         elsif Argv'Length >= 3 and then Argv (2) = 'M' then
            if not Is_Cross_Compiler then
               Write_Line
                 ("gnatbind: -M not expected to be used on native platforms");
            end if;

            Opt.Bind_Alternate_Main_Name := True;
            Opt.Alternate_Main_Name := new String'(Argv (3 .. Argv'Last));

         --  All other options are single character and are handled by
         --  Scan_Binder_Switches.

         else
            Scan_Binder_Switches (Argv);
         end if;

      --  Not a switch, so must be a file name (if non-empty)

      elsif Argv'Length /= 0 then
         if Argv'Length > 4
           and then Argv (Argv'Last - 3 .. Argv'Last) = ".ali"
         then
            Add_File (Argv);
         else
            Add_File (Argv & ".ali");
         end if;
      end if;
   end Scan_Bind_Arg;

   ----------------------------
   -- Generic_Scan_Bind_Args --
   ----------------------------

   procedure Generic_Scan_Bind_Args is
      Next_Arg : Positive := 1;

   begin
      --  Use low level argument routines to avoid dragging in secondary stack

      while Next_Arg < Arg_Count loop
         declare
            Next_Argv : String (1 .. Len_Arg (Next_Arg));

         begin
            Fill_Arg (Next_Argv'Address, Next_Arg);

            if Next_Argv'Length > 0 then
               if Next_Argv (1) = '@' then
                  if Next_Argv'Length > 1 then
                     declare
                        Arguments : constant Argument_List :=
                                      Response_File.Arguments_From
                                        (Response_File_Name        =>
                                           Next_Argv (2 .. Next_Argv'Last),
                                         Recursive                 => True,
                                         Ignore_Non_Existing_Files => True);
                     begin
                        for J in Arguments'Range loop
                           Action (Arguments (J).all);
                        end loop;
                     end;
                  end if;

               else
                  Action (Next_Argv);
               end if;
            end if;
         end;

         Next_Arg := Next_Arg + 1;
      end loop;
   end Generic_Scan_Bind_Args;

   ---------------
   -- Write_Arg --
   ---------------

   procedure Write_Arg (S : String) is
   begin
      Write_Str (" " & S);
   end Write_Arg;

   procedure Check_Version_And_Help is
     new Check_Version_And_Help_G (Bindusg.Display);

   procedure Put_Bind_Args  is new Generic_Scan_Bind_Args (Write_Arg);
   procedure Scan_Bind_Args is new Generic_Scan_Bind_Args (Scan_Bind_Arg);

--  Start of processing for Gnatbind

begin
   --  Set default for Shared_Libgnat option

   declare
      Shared_Libgnat_Default : Character;
      pragma Import
        (C, Shared_Libgnat_Default, "__gnat_shared_libgnat_default");

      SHARED : constant Character := 'H';
      STATIC : constant Character := 'T';

   begin
      pragma Assert
        (Shared_Libgnat_Default = SHARED
          or else
         Shared_Libgnat_Default = STATIC);
      Shared_Libgnat := (Shared_Libgnat_Default = SHARED);
   end;

   --  Carry out package initializations. These are initializations which
   --  might logically be performed at elaboration time, and we decide to be
   --  consistent. Like elaboration, the order in which these calls are made
   --  is in some cases important.

   Csets.Initialize;
   Snames.Initialize;

   --  Scan the switches and arguments. Note that Snames must already be
   --  initialized (for processing of the -V switch).

   --  First, scan to detect --version and/or --help

   Check_Version_And_Help ("GNATBIND", "1992");

   --  We need to Scan_Bind_Args first, to set Verbose_Mode, so we know whether
   --  to Put_Bind_Args.

   Scan_Bind_Args;

   if Verbose_Mode then
      Write_Str (Command_Name);
      Put_Bind_Args;
      Write_Eol;
   end if;

   if Use_Pragma_Linker_Constructor then
      if Bind_Main_Program then
         Fail ("switch -a must be used in conjunction with -n or -Lxxx");

      elsif not Gnatbind_Supports_Auto_Init then
         Fail ("automatic initialisation of elaboration not supported on this "
               & "platform");
      end if;
   end if;

   --  Test for trailing -o switch

   if Opt.Output_File_Name_Present and then not Output_File_Name_Seen then
      Fail ("output file name missing after -o");
   end if;

   --  Output usage if requested

   if Usage_Requested then
      Bindusg.Display;
   end if;

   --  Check that the binder file specified has extension .adb

   if Opt.Output_File_Name_Present and then Output_File_Name_Seen then
      Check_Extensions : declare
         Length : constant Natural := Output_File_Name'Length;
         Last   : constant Natural := Output_File_Name'Last;

      begin
         if Length <= 4
           or else Output_File_Name (Last - 3 .. Last) /= ".adb"
         then
            Fail ("output file name should have .adb extension");
         end if;
      end Check_Extensions;
   end if;

   Osint.Add_Default_Search_Dirs;

   --  Acquire target parameters

   Targparm.Get_Target_Parameters;

   --  Initialize Cumulative_Restrictions with the restrictions on the target
   --  scanned from the system.ads file. Then as we read ALI files, we will
   --  accumulate additional restrictions specified in other files.

   Cumulative_Restrictions := Targparm.Restrictions_On_Target;

   --  Acquire configurable run-time mode

   if Configurable_Run_Time_On_Target then
      Configurable_Run_Time_Mode := True;
   end if;

   --  Output copyright notice if in verbose mode

   if Verbose_Mode then
      Write_Eol;
      Display_Version ("GNATBIND", "1995");
   end if;

   --  Output usage information if no arguments

   if not More_Lib_Files then
      if Argument_Count = 0 then
         Bindusg.Display;
      else
         Write_Line ("try ""gnatbind --help"" for more information.");
      end if;

      Exit_Program (E_Fatal);
   end if;

   --  If a mapping file was specified, initialize the file mapping

   if Mapping_File /= null then
      Fmap.Initialize (Mapping_File.all);
   end if;

   --  The block here is to catch the Unrecoverable_Error exception in the
   --  case where we exceed the maximum number of permissible errors or some
   --  other unrecoverable error occurs.

   begin
      --  Initialize binder packages

      Initialize_Binderr;
      Initialize_ALI;
      Initialize_ALI_Source;

      if Verbose_Mode then
         Write_Eol;
      end if;

      --  Input ALI files

      while More_Lib_Files loop
         Main_Lib_File := Next_Main_Lib_File;

         if First_Main_Lib_File = No_File then
            First_Main_Lib_File := Main_Lib_File;
         end if;

         if Verbose_Mode then
            if Check_Only then
               Write_Str ("Checking: ");
            else
               Write_Str ("Binding: ");
            end if;

            Write_Name (Main_Lib_File);
            Write_Eol;
         end if;

         Text := Read_Library_Info (Main_Lib_File, True);

         declare
            Id : ALI_Id;
            pragma Warnings (Off, Id);

         begin
            Id := Scan_ALI
                    (F                => Main_Lib_File,
                     T                => Text,
                     Ignore_ED        => False,
                     Err              => False,
                     Ignore_Errors    => Debug_Flag_I,
                     Directly_Scanned => True);
         end;

         Free (Text);
      end loop;

      --  No_Run_Time mode

      if No_Run_Time_Mode then

         --  Set standard configuration parameters

         Suppress_Standard_Library_On_Target := True;
         Configurable_Run_Time_Mode          := True;
      end if;

      --  For main ALI files, even if they are interfaces, we get their
      --  dependencies. To be sure, we reset the Interface flag for all main
      --  ALI files.

      for Index in ALIs.First .. ALIs.Last loop
         ALIs.Table (Index).SAL_Interface := False;
      end loop;

      --  Add System.Standard_Library to list to ensure that these files are
      --  included in the bind, even if not directly referenced from Ada code
      --  This is suppressed if the appropriate targparm switch is set. Be sure
      --  in any case that System is in the closure, as it may contain linker
      --  options. Note that it will be automatically added if s-stalib is
      --  added.

      if not Suppress_Standard_Library_On_Target then
         Add_Artificial_ALI_File ("s-stalib.ali");
      else
         Add_Artificial_ALI_File ("system.ali");
      end if;

      --  Load ALIs for all dependent units

      for Index in ALIs.First .. ALIs.Last loop
         Read_Withed_ALIs (Index);
      end loop;

      --  Quit if some file needs compiling

      if No_Object_Specified then
         raise Unrecoverable_Error;
      end if;

      --  Quit with message if we had a GNATprove file

      if GNATprove_Mode_Specified then
         Error_Msg ("one or more files compiled in GNATprove mode");
         raise Unrecoverable_Error;
      end if;

      --  Output list of ALI files in closure

      if Output_ALI_List then
         if ALI_List_Filename /= null then
            Set_List_File (ALI_List_Filename.all);
         end if;

         for Index in ALIs.First .. ALIs.Last loop
            declare
               Full_Afile : constant File_Name_Type :=
                              Find_File (ALIs.Table (Index).Afile, Library);
            begin
               Write_Name (Full_Afile);
               Write_Eol;
            end;
         end loop;

         if ALI_List_Filename /= null then
            Close_List_File;
         end if;
      end if;

      --  Build source file table from the ALI files we have read in

      Set_Source_Table;

      --  If there is main program to bind, set Main_Lib_File to the first
      --  library file, and the name from which to derive the binder generate
      --  file to the first ALI file.

      if Bind_Main_Program then
         Main_Lib_File := First_Main_Lib_File;
         Set_Current_File_Name_Index (To => 1);
      end if;

      --  Check that main library file is a suitable main program

      if Bind_Main_Program
        and then ALIs.Table (ALIs.First).Main_Program = None
        and then not No_Main_Subprogram
      then
         Get_Name_String
           (Units.Table (ALIs.Table (ALIs.First).First_Unit).Uname);

         declare
            Unit_Name : String := Name_Buffer (1 .. Name_Len - 2);
         begin
            To_Mixed (Unit_Name);
            Get_Name_String (ALIs.Table (ALIs.First).Sfile);
            Add_Str_To_Name_Buffer (":1: ");
            Add_Str_To_Name_Buffer (Unit_Name);
            Add_Str_To_Name_Buffer (" cannot be used as a main program");
            Write_Line (Name_Buffer (1 .. Name_Len));
            Errors_Detected := Errors_Detected + 1;
         end;
      end if;

      --  Perform consistency and correctness checks. Disable these in CodePeer
      --  mode where we want to be more flexible.

      if not CodePeer_Mode then
         Check_Duplicated_Subunits;
         Check_Versions;
         Check_Consistency;
         Check_Configuration_Consistency;
      end if;

      --  List restrictions that could be applied to this partition

      if List_Restrictions then
         List_Applicable_Restrictions;
      end if;

      --  Complete bind if no errors

      if Errors_Detected = 0 then
         declare
            Elab_Order : Unit_Id_Table;
            use Unit_Id_Tables;

         begin
            Find_Elab_Order (Elab_Order, First_Main_Lib_File);

            if Errors_Detected = 0 and then not Check_Only then
               Gen_Output_File
                 (Output_File_Name.all,
                  Elab_Order => Elab_Order.Table (First .. Last (Elab_Order)));
            end if;
         end;
      end if;

      Total_Errors := Total_Errors + Errors_Detected;
      Total_Warnings := Total_Warnings + Warnings_Detected;

   exception
      when Unrecoverable_Error =>
         Total_Errors := Total_Errors + Errors_Detected;
         Total_Warnings := Total_Warnings + Warnings_Detected;
   end;

   --  All done. Set the proper exit status.

   Finalize_Binderr;
   Namet.Finalize;

   if Total_Errors > 0 then
      Exit_Program (E_Errors);

   elsif Total_Warnings > 0 then
      Exit_Program (E_Warnings);

   else
      --  Do not call Exit_Program (E_Success), so that finalization occurs
      --  normally.

      null;
   end if;
end Gnatbind;
