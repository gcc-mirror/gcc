------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T B I N D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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
with Butil;    use Butil;
with Casing;   use Casing;
with Csets;
with Debug;    use Debug;
with Fmap;
with Fname;    use Fname;
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

   Std_Lib_File : File_Name_Type;
   --  Standard library

   Text     : Text_Buffer_Ptr;
   Next_Arg : Positive;

   Output_File_Name_Seen : Boolean := False;
   Output_File_Name      : String_Ptr := new String'("");

   L_Switch_Seen : Boolean := False;

   Mapping_File : String_Ptr := null;

   function Gnatbind_Supports_Auto_Init return Boolean;
   --  Indicates if automatic initialization of elaboration procedure
   --  through the constructor mechanism is possible on the platform.

   procedure List_Applicable_Restrictions;
   --  List restrictions that apply to this partition if option taken

   procedure Scan_Bind_Arg (Argv : String);
   --  Scan and process binder specific arguments. Argv is a single argument.
   --  All the one character arguments are still handled by Switch. This
   --  routine handles -aO -aI and -I-. The lower bound of Argv must be 1.

   function Is_Cross_Compiler return Boolean;
   --  Returns True iff this is a cross-compiler

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
        (No_Exception_Propagation => True,
         --  Modifies code resulting in different exception semantics

         No_Exceptions            => True,
         --  Has unexpected Suppress (All_Checks) effect

         No_Implicit_Conditionals => True,
         --  This could modify and pessimize generated code

         No_Implicit_Dynamic_Code => True,
         --  This could modify and pessimize generated code

         No_Implicit_Loops        => True,
         --  This could modify and pessimize generated code

         No_Recursion             => True,
         --  Not checkable at compile time

         No_Reentrancy            => True,
         --  Not checkable at compile time

         Max_Entry_Queue_Length    => True,
         --  Not checkable at compile time

         Max_Storage_At_Blocking  => True,
         --  Not checkable at compile time

         others => False);

      Additional_Restrictions_Listed : Boolean := False;
      --  Set True if we have listed header for restrictions

   begin
      --  Loop through restrictions

      for R in All_Restrictions loop
         if not No_Restriction_List (R) then

            --  We list a restriction if it is not violated, or if
            --  it is violated but the violation count is exactly known.

            if Cumulative_Restrictions.Violated (R) = False
              or else (R in All_Parameter_Restrictions
                       and then
                         Cumulative_Restrictions.Unknown (R) = False)
            then
               if not Additional_Restrictions_Listed then
                  Write_Eol;
                  Write_Line
                    ("The following additional restrictions may be" &
                     " applied to this partition:");
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

               --  Remember that the -L switch was specified, so that if this
               --  is on OpenVMS, the export names are put in uppercase.
               --  This is not known before the target parameters are read.

               L_Switch_Seen := True;

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
                 ("Prefix of initialization and finalization " &
                  "procedure names missing in -L");
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

               --  Invalid -S switch, let Switch give error, set defalut of IN

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

   procedure Check_Version_And_Help is
      new Check_Version_And_Help_G (Bindusg.Display);

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

   --  Scan the switches and arguments

   --  First, scan to detect --version and/or --help

   Check_Version_And_Help ("GNATBIND", "1995");

   --  Use low level argument routines to avoid dragging in the secondary stack

   Next_Arg := 1;
   Scan_Args : while Next_Arg < Arg_Count loop
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
                        Scan_Bind_Arg (Arguments (J).all);
                     end loop;
                  end;
               end if;

            else
               Scan_Bind_Arg (Next_Argv);
            end if;
         end if;
      end;

      Next_Arg := Next_Arg + 1;
   end loop Scan_Args;

   if Use_Pragma_Linker_Constructor then
      if Bind_Main_Program then
         Fail ("switch -a must be used in conjunction with -n or -Lxxx");

      elsif not Gnatbind_Supports_Auto_Init then
         Fail ("automatic initialisation of elaboration " &
               "not supported on this platform");
      end if;
   end if;

   --  Test for trailing -o switch

   if Opt.Output_File_Name_Present
     and then not Output_File_Name_Seen
   then
      Fail ("output file name missing after -o");
   end if;

   --  Output usage if requested

   if Usage_Requested then
      Bindusg.Display;
   end if;

   --  Check that the Ada binder file specified has extension .adb and that
   --  the C binder file has extension .c

   if Opt.Output_File_Name_Present
     and then Output_File_Name_Seen
   then
      Check_Extensions : declare
         Length : constant Natural := Output_File_Name'Length;
         Last   : constant Natural := Output_File_Name'Last;

      begin
         if Ada_Bind_File then
            if Length <= 4
              or else Output_File_Name (Last - 3 .. Last) /= ".adb"
            then
               Fail ("output file name should have .adb extension");
            end if;

         else
            if Length <= 2
              or else Output_File_Name (Last - 1 .. Last) /= ".c"
            then
               Fail ("output file name should have .c extension");
            end if;
         end if;
      end Check_Extensions;
   end if;

   Osint.Add_Default_Search_Dirs;

   --  Carry out package initializations. These are initializations which
   --  might logically be performed at elaboration time, but Namet at least
   --  can't be done that way (because it is used in the Compiler), and we
   --  decide to be consistent. Like elaboration, the order in which these
   --  calls are made is in some cases important.

   Csets.Initialize;
   Namet.Initialize;
   Snames.Initialize;

   --  Acquire target parameters

   Targparm.Get_Target_Parameters;

   --  Initialize Cumulative_Restrictions with the restrictions on the target
   --  scanned from the system.ads file. Then as we read ALI files, we will
   --  accumulate additional restrictions specified in other files.

   Cumulative_Restrictions := Targparm.Restrictions_On_Target;

   --  On OpenVMS, when -L is used, all external names used in pragmas Export
   --  are in upper case. The reason is that on OpenVMS, the macro-assembler
   --  MACASM-32, used to build Stand-Alone Libraries, only understands
   --  uppercase.

   if L_Switch_Seen and then OpenVMS_On_Target then
      To_Upper (Opt.Ada_Init_Name.all);
      To_Upper (Opt.Ada_Final_Name.all);
      To_Upper (Opt.Ada_Main_Name.all);
   end if;

   --  Acquire configurable run-time mode

   if Configurable_Run_Time_On_Target then
      Configurable_Run_Time_Mode := True;
   end if;

   --  Output copyright notice if in verbose mode

   if Verbose_Mode then
      Write_Eol;
      Display_Version ("GNATBIND", "1995");
   end if;

   --  Output usage information if no files

   if not More_Lib_Files then
      Bindusg.Display;
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
                    (F             => Main_Lib_File,
                     T             => Text,
                     Ignore_ED     => False,
                     Err           => False,
                     Ignore_Errors => Debug_Flag_I);
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
      --  This is suppressed if the appropriate targparm switch is set.

      if not Suppress_Standard_Library_On_Target then
         Name_Buffer (1 .. 12) := "s-stalib.ali";
         Name_Len := 12;
         Std_Lib_File := Name_Find;
         Text := Read_Library_Info (Std_Lib_File, True);

         declare
            Id : ALI_Id;
            pragma Warnings (Off, Id);

         begin
            Id :=
              Scan_ALI
                (F             => Std_Lib_File,
                 T             => Text,
                 Ignore_ED     => False,
                 Err           => False,
                 Ignore_Errors => Debug_Flag_I);
         end;

         Free (Text);
      end if;

      --  Acquire all information in ALI files that have been read in

      for Index in ALIs.First .. ALIs.Last loop
         Read_ALI (Index);
      end loop;

      --  Quit if some file needs compiling

      if No_Object_Specified then
         raise Unrecoverable_Error;
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
         Error_Msg_File_1 := Main_Lib_File;
         Error_Msg ("{ does not contain a unit that can be a main program");
      end if;

      --  Perform consistency and correctness checks

      Check_Duplicated_Subunits;
      Check_Versions;
      Check_Consistency;
      Check_Configuration_Consistency;

      --  List restrictions that could be applied to this partition

      if List_Restrictions then
         List_Applicable_Restrictions;
      end if;

      --  Complete bind if no errors

      if Errors_Detected = 0 then
         Find_Elab_Order;

         if Errors_Detected = 0 then
            --  Display elaboration order if -l was specified

            if Elab_Order_Output then
               if not Zero_Formatting then
                  Write_Eol;
                  Write_Str ("ELABORATION ORDER");
                  Write_Eol;
               end if;

               for J in Elab_Order.First .. Elab_Order.Last loop
                  if not Units.Table (Elab_Order.Table (J)).SAL_Interface then
                     if not Zero_Formatting then
                        Write_Str ("   ");
                     end if;

                     Write_Unit_Name
                       (Units.Table (Elab_Order.Table (J)).Uname);
                     Write_Eol;
                  end if;
               end loop;

               if not Zero_Formatting then
                  Write_Eol;
               end if;
            end if;

            if not Check_Only then
               Gen_Output_File (Output_File_Name.all);
            end if;

            --  Display list of sources in the closure (except predefined
            --  sources) if -R was used.

            if List_Closure then
               if not Zero_Formatting then
                  Write_Eol;
                  Write_Str ("REFERENCED SOURCES");
                  Write_Eol;
               end if;

               for J in reverse Elab_Order.First .. Elab_Order.Last loop

                  --  Do not include the sources of the runtime

                  if not Is_Internal_File_Name
                           (Units.Table (Elab_Order.Table (J)).Sfile)
                  then
                     if not Zero_Formatting then
                        Write_Str ("   ");
                     end if;

                     Write_Str
                       (Get_Name_String
                          (Units.Table (Elab_Order.Table (J)).Sfile));
                     Write_Eol;
                  end if;
               end loop;

               if not Zero_Formatting then
                  Write_Eol;
               end if;
            end if;
         end if;
      end if;

      Total_Errors := Total_Errors + Errors_Detected;
      Total_Warnings := Total_Warnings + Warnings_Detected;

   exception
      when Unrecoverable_Error =>
         Total_Errors := Total_Errors + Errors_Detected;
         Total_Warnings := Total_Warnings + Warnings_Detected;
   end;

   --  All done. Set proper exit status

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
