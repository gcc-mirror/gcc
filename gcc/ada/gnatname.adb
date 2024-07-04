------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T N A M E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2024, Free Software Foundation, Inc.         --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Dynamic_Tables;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Make_Util; use Make_Util;
with Namet;     use Namet;
with Opt;
with Osint;     use Osint;
with Output;
with Switch;    use Switch;
with Table;
with Tempdir;
with Types;     use Types;

with System.CRTL;
with System.Regexp;    use System.Regexp;

procedure Gnatname is

   pragma Warnings (Off);
   type Matched_Type is (True, False, Excluded);
   pragma Warnings (On);

   Create_Project : Boolean := False;

   Subdirs_Switch : constant String := "--subdirs=";

   Usage_Output : Boolean := False;
   --  Set to True when usage is output, to avoid multiple output

   Usage_Needed : Boolean := False;
   --  Set to True by -h switch

   Version_Output : Boolean := False;
   --  Set to True when version is output, to avoid multiple output

   Very_Verbose : Boolean := False;
   --  Set to True with -v -v

   File_Path : String_Access := new String'("gnat.adc");
   --  Path name of the file specified by -c or -P switch

   File_Set : Boolean := False;
   --  Set to True by -c or -P switch.
   --  Used to detect multiple -c/-P switches.

   Args : Argument_List_Access;
   --  The list of arguments for calls to the compiler to get the unit names
   --  and kinds (spec or body) in the Ada sources.

   Path_Name : String_Access;

   Path_Last : Natural;

   Directory_Last    : Natural := 0;

   function Dup (Fd : File_Descriptor) return File_Descriptor;

   procedure Dup2 (Old_Fd, New_Fd : File_Descriptor);

   Gcc      : constant String := "gcc";
   Gcc_Path : String_Access := null;

   package Patterns is new GNAT.Dynamic_Tables
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  Table to accumulate the patterns

   type Argument_Data is record
      Directories       : Patterns.Instance;
      Name_Patterns     : Patterns.Instance;
      Excluded_Patterns : Patterns.Instance;
      Foreign_Patterns  : Patterns.Instance;
   end record;

   package Arguments is new Table.Table
     (Table_Component_Type => Argument_Data,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Gnatname.Arguments");
   --  Table to accumulate directories and patterns

   package Preprocessor_Switches is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Gnatname.Preprocessor_Switches");
   --  Table to store the preprocessor switches to be used in the call
   --  to the compiler.

   type Source is record
      File_Name : Name_Id;
      Unit_Name : Name_Id;
      Index     : Int := 0;
      Spec      : Boolean;
   end record;

   package Processed_Directories is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Prj.Makr.Processed_Directories");
   --  The list of already processed directories for each section, to avoid
   --  processing several times the same directory in the same section.

   package Sources is new Table.Table
     (Table_Component_Type => Source,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Gnatname.Sources");
   --  The list of Ada sources found, with their unit name and kind, to be put
   --  in the pragmas Source_File_Name in the configuration pragmas file.

   procedure Output_Version;
   --  Print name and version

   procedure Usage;
   --  Print usage

   procedure Scan_Args;
   --  Scan the command line arguments

   procedure Add_Source_Directory (S : String);
   --  Add S in the Source_Directories table

   procedure Get_Directories (From_File : String);
   --  Read a source directory text file

   procedure Write_Eol;
   --  Output an empty line

   procedure Write_A_String (S : String);
   --  Write a String to Output_FD

   procedure Initialize
     (File_Path         : String;
      Preproc_Switches  : Argument_List);
   --  Start the creation of a configuration pragmas file
   --
   --  File_Path is the name of the configuration pragmas file to create
   --
   --  Preproc_Switches is a list of switches to be used when invoking the
   --  compiler to get the name and kind of unit of a source file.

   type Regexp_List is array (Positive range <>) of Regexp;

   procedure Process
     (Directories       : Argument_List;
      Name_Patterns     : Regexp_List;
      Excluded_Patterns : Regexp_List;
      Foreign_Patterns  : Regexp_List);
   --  Look for source files in the specified directories, with the specified
   --  patterns.
   --
   --  Directories is the list of source directories where to look for sources.
   --
   --  Name_Patterns is a potentially empty list of file name patterns to check
   --  for Ada Sources.
   --
   --  Excluded_Patterns is a potentially empty list of file name patterns that
   --  should not be checked for Ada or non Ada sources.
   --
   --  Foreign_Patterns is a potentially empty list of file name patterns to
   --  check for non Ada sources.
   --
   --  At least one of Name_Patterns and Foreign_Patterns is not empty

   procedure Finalize;
   --  Write the configuration pragmas file indicated in a call to procedure
   --  Initialize, after one or several calls to procedure Process.

   --------------------------
   -- Add_Source_Directory --
   --------------------------

   procedure Add_Source_Directory (S : String) is
   begin
      Patterns.Append
        (Arguments.Table (Arguments.Last).Directories, new String'(S));
   end Add_Source_Directory;

   ---------
   -- Dup --
   ---------

   function Dup  (Fd : File_Descriptor) return File_Descriptor is
   begin
      return File_Descriptor (System.CRTL.dup (Integer (Fd)));
   end Dup;

   ----------
   -- Dup2 --
   ----------

   procedure Dup2 (Old_Fd, New_Fd : File_Descriptor) is
      Fd : Integer;
      pragma Warnings (Off, Fd);
   begin
      Fd := System.CRTL.dup2 (Integer (Old_Fd), Integer (New_Fd));
   end Dup2;

   ---------------------
   -- Get_Directories --
   ---------------------

   procedure Get_Directories (From_File : String) is
      File : Ada.Text_IO.File_Type;
      Line : String (1 .. 2_000);
      Last : Natural;

   begin
      Open (File, In_File, From_File);

      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);

         if Last /= 0 then
            Add_Source_Directory (Line (1 .. Last));
         end if;
      end loop;

      Close (File);

   exception
      when Name_Error =>
         Fail ("cannot open source directory file """ & From_File & '"');
   end Get_Directories;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      Discard : Boolean;
      pragma Warnings (Off, Discard);

   begin
      --  Delete the file if it already exists

      Delete_File
        (Path_Name (Directory_Last + 1 .. Path_Last),
         Success => Discard);

      --  Create a new one

      if Opt.Verbose_Mode then
         Output.Write_Str ("Creating new file """);
         Output.Write_Str (Path_Name (Directory_Last + 1 .. Path_Last));
         Output.Write_Line ("""");
      end if;

      Output_FD := Create_New_File
        (Path_Name (Directory_Last + 1 .. Path_Last),
         Fmode => Text);

      --  Fails if file cannot be created

      if Output_FD = Invalid_FD then
         Fail_Program
           ("cannot create new """ & Path_Name (1 .. Path_Last) & """");
      end if;

      --  For each Ada source, write a pragma Source_File_Name to the
      --  configuration pragmas file.

      for Index in 1 .. Sources.Last loop
         if Sources.Table (Index).Unit_Name /= No_Name then
            Write_A_String ("pragma Source_File_Name");
            Write_Eol;
            Write_A_String ("  (");
            Write_A_String
              (Get_Name_String (Sources.Table (Index).Unit_Name));
            Write_A_String (",");
            Write_Eol;

            if Sources.Table (Index).Spec then
               Write_A_String ("   Spec_File_Name => """);

            else
               Write_A_String ("   Body_File_Name => """);
            end if;

            Write_A_String
              (Get_Name_String (Sources.Table (Index).File_Name));

            Write_A_String ("""");

            if Sources.Table (Index).Index /= 0 then
               Write_A_String (", Index =>");
               Write_A_String (Sources.Table (Index).Index'Img);
            end if;

            Write_A_String (");");
            Write_Eol;
         end if;
      end loop;

      Close (Output_FD);
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (File_Path         : String;
      Preproc_Switches  : Argument_List)
   is
   begin
      Sources.Set_Last (0);

      --  Initialize the compiler switches

      Args := new Argument_List (1 .. Preproc_Switches'Length + 6);
      Args (1) := new String'("-c");
      Args (2) := new String'("-gnats");
      Args (3) := new String'("-gnatu");
      Args (4 .. 3 + Preproc_Switches'Length) := Preproc_Switches;
      Args (4 + Preproc_Switches'Length) := new String'("-x");
      Args (5 + Preproc_Switches'Length) := new String'("ada");

      --  Get the path and file names

      Path_Name := new
        String (1 .. File_Path'Length);
      Path_Last := File_Path'Length;

      if File_Names_Case_Sensitive then
         Path_Name (1 .. Path_Last) := File_Path;
      else
         Path_Name (1 .. Path_Last) := To_Lower (File_Path);
      end if;

      --  Get the end of directory information, if any

      for Index in reverse 1 .. Path_Last loop
         if Path_Name (Index) = Directory_Separator then
            Directory_Last := Index;
            exit;
         end if;
      end loop;

      --  Change the current directory to the directory of the project file,
      --  if any directory information is specified.

      if Directory_Last /= 0 then
         begin
            Change_Dir (Path_Name (1 .. Directory_Last));
         exception
            when Directory_Error =>
               Fail_Program
                 ("unknown directory """
                  & Path_Name (1 .. Directory_Last)
                  & """");
         end;
      end if;
   end Initialize;

   -------------
   -- Process --
   -------------

   procedure Process
     (Directories       : Argument_List;
      Name_Patterns     : Regexp_List;
      Excluded_Patterns : Regexp_List;
      Foreign_Patterns  : Regexp_List)
  is
      procedure Process_Directory (Dir_Name : String);
      --  Look for Ada and foreign sources in a directory, according to the
      --  patterns.

      -----------------------
      -- Process_Directory --
      -----------------------

      procedure Process_Directory (Dir_Name : String) is
         Matched : Matched_Type := False;
         Str     : String (1 .. 2_000);
         Canon   : String (1 .. 2_000);
         Last    : Natural;
         Dir     : Dir_Type;
         Do_Process : Boolean := True;

         Temp_File_Name         : String_Access := null;
         Save_Last_Source_Index : Natural := 0;
         File_Name_Id           : Name_Id := No_Name;

         Current_Source : Source;

      begin
         --  Avoid processing the same directory more than once

         for Index in 1 .. Processed_Directories.Last loop
            if Processed_Directories.Table (Index).all = Dir_Name then
               Do_Process := False;
               exit;
            end if;
         end loop;

         if Do_Process then
            if Opt.Verbose_Mode then
               Output.Write_Str ("Processing directory """);
               Output.Write_Str (Dir_Name);
               Output.Write_Line ("""");
            end if;

            Processed_Directories. Increment_Last;
            Processed_Directories.Table (Processed_Directories.Last) :=
              new String'(Dir_Name);

            --  Get the source file names from the directory. Fails if the
            --  directory does not exist.

            begin
               Open (Dir, Dir_Name);
            exception
               when Directory_Error =>
                  Fail_Program ("cannot open directory """ & Dir_Name & """");
            end;

            --  Process each regular file in the directory

            File_Loop : loop
               Read (Dir, Str, Last);
               exit File_Loop when Last = 0;

               --  Copy the file name and put it in canonical case to match
               --  against the patterns that have themselves already been put
               --  in canonical case.

               Canon (1 .. Last) := Str (1 .. Last);
               Canonical_Case_File_Name (Canon (1 .. Last));

               if Is_Regular_File
                    (Dir_Name & Directory_Separator & Str (1 .. Last))
               then
                  Matched := True;

                  Name_Len := Last;
                  Name_Buffer (1 .. Name_Len) := Str (1 .. Last);
                  File_Name_Id := Name_Find;

                  --  First, check if the file name matches at least one of
                  --  the excluded expressions;

                  for Index in Excluded_Patterns'Range loop
                     if
                       Match (Canon (1 .. Last), Excluded_Patterns (Index))
                     then
                        Matched := Excluded;
                        exit;
                     end if;
                  end loop;

                  --  If it does not match any of the excluded expressions,
                  --  check if the file name matches at least one of the
                  --  regular expressions.

                  if Matched = True then
                     Matched := False;

                     for Index in Name_Patterns'Range loop
                        if
                          Match
                            (Canon (1 .. Last), Name_Patterns (Index))
                        then
                           Matched := True;
                           exit;
                        end if;
                     end loop;
                  end if;

                  if Very_Verbose
                    or else (Matched = True and then Opt.Verbose_Mode)
                  then
                     Output.Write_Str ("   Checking """);
                     Output.Write_Str (Str (1 .. Last));
                     Output.Write_Line (""": ");
                  end if;

                  --  If the file name matches one of the regular expressions,
                  --  parse it to get its unit name.

                  if Matched = True then
                     declare
                        FD : File_Descriptor;
                        Success : Boolean;
                        Saved_Output : File_Descriptor;
                        Saved_Error  : File_Descriptor;
                        Tmp_File     : Path_Name_Type;

                     begin
                        --  If we don't have the path of the compiler yet,
                        --  get it now. The compiler name may have a prefix,
                        --  so we get the potentially prefixed name.

                        if Gcc_Path = null then
                           declare
                              Prefix_Gcc : String_Access :=
                                             Program_Name (Gcc, "gnatname");
                           begin
                              Gcc_Path :=
                                Locate_Exec_On_Path (Prefix_Gcc.all);
                              Free (Prefix_Gcc);
                           end;

                           if Gcc_Path = null then
                              Fail_Program ("could not locate " & Gcc);
                           end if;
                        end if;

                        --  Create the temporary file

                        Tempdir.Create_Temp_File (FD, Tmp_File);

                        if FD = Invalid_FD then
                           Fail_Program
                             ("could not create temporary file");

                        else
                           Temp_File_Name :=
                             new String'(Get_Name_String (Tmp_File));
                        end if;

                        Args (Args'Last) :=
                          new String'
                            (Dir_Name & Directory_Separator & Str (1 .. Last));

                        --  Save the standard output and error

                        Saved_Output := Dup (Standout);
                        Saved_Error  := Dup (Standerr);

                        --  Set standard output and error to the temporary file

                        Dup2 (FD, Standout);
                        Dup2 (FD, Standerr);

                        --  And spawn the compiler

                        Spawn (Gcc_Path.all, Args.all, Success);

                        --  Restore the standard output and error

                        Dup2 (Saved_Output, Standout);
                        Dup2 (Saved_Error, Standerr);

                        --  Close the temporary file

                        Close (FD);

                        --  And close the saved standard output and error to
                        --  avoid too many file descriptors.

                        Close (Saved_Output);
                        Close (Saved_Error);

                        --  Now that standard output is restored, check if
                        --  the compiler ran correctly.

                        --  Read the lines of the temporary file:
                        --  they should contain the kind and name of the unit.

                        declare
                           File      : Ada.Text_IO.File_Type;
                           Text_Line : String (1 .. 1_000);
                           Text_Last : Natural;

                        begin
                           begin
                              Open (File, In_File, Temp_File_Name.all);

                           exception
                              when others =>
                                 Fail_Program
                                   ("could not read temporary file " &
                                      Temp_File_Name.all);
                           end;

                           Save_Last_Source_Index := Sources.Last;

                           if End_Of_File (File) then
                              if Opt.Verbose_Mode then
                                 if not Success then
                                    Output.Write_Str ("      (process died) ");
                                 end if;
                              end if;

                           else
                              Line_Loop : while not End_Of_File (File) loop
                                 Get_Line (File, Text_Line, Text_Last);

                                 --  Find the first closing parenthesis

                                 Char_Loop : for J in 1 .. Text_Last loop
                                    if Text_Line (J) = ')' then
                                       if J >= 13 and then
                                         Text_Line (1 .. 4) = "Unit"
                                       then
                                          --  Add entry to Sources table

                                          Name_Len := J - 12;
                                          Name_Buffer (1 .. Name_Len) :=
                                            Text_Line (6 .. J - 7);
                                          Current_Source :=
                                            (Unit_Name  => Name_Find,
                                             File_Name  => File_Name_Id,
                                             Index => 0,
                                             Spec  => Text_Line (J - 5 .. J) =
                                                        "(spec)");

                                          Sources.Append (Current_Source);
                                       end if;

                                       exit Char_Loop;
                                    end if;
                                 end loop Char_Loop;
                              end loop Line_Loop;
                           end if;

                           if Save_Last_Source_Index = Sources.Last then
                              if Opt.Verbose_Mode then
                                 Output.Write_Line ("      not a unit");
                              end if;

                           else
                              if Sources.Last >
                                   Save_Last_Source_Index + 1
                              then
                                 for Index in Save_Last_Source_Index + 1 ..
                                                Sources.Last
                                 loop
                                    Sources.Table (Index).Index :=
                                      Int (Index - Save_Last_Source_Index);
                                 end loop;
                              end if;

                              for Index in Save_Last_Source_Index + 1 ..
                                             Sources.Last
                              loop
                                 Current_Source := Sources.Table (Index);
                                 pragma Annotate
                                   (CodePeer, Modified, Current_Source);

                                 if Opt.Verbose_Mode then
                                    if Current_Source.Spec then
                                       Output.Write_Str ("      spec of ");

                                    else
                                       Output.Write_Str ("      body of ");
                                    end if;

                                    Output.Write_Line
                                      (Get_Name_String
                                         (Current_Source.Unit_Name));
                                 end if;
                              end loop;
                           end if;

                           Close (File);

                           Delete_File (Temp_File_Name.all, Success);
                        end;
                     end;

                  --  File name matches none of the regular expressions

                  else
                     --  If file is not excluded, see if this is foreign source

                     if Matched /= Excluded then
                        for Index in Foreign_Patterns'Range loop
                           if Match (Canon (1 .. Last),
                                     Foreign_Patterns (Index))
                           then
                              Matched := True;
                              exit;
                           end if;
                        end loop;
                     end if;

                     if Very_Verbose then
                        case Matched is
                           when False =>
                              Output.Write_Line ("no match");

                           when Excluded =>
                              Output.Write_Line ("excluded");

                           when True =>
                              Output.Write_Line ("foreign source");
                        end case;
                     end if;

                     if Matched = True then

                        --  Add source file name without unit name

                        Name_Len := 0;
                        Add_Str_To_Name_Buffer (Canon (1 .. Last));
                        Sources.Append
                          ((File_Name => Name_Find,
                            Unit_Name => No_Name,
                            Index     => 0,
                            Spec      => False));
                     end if;
                  end if;
               end if;
            end loop File_Loop;

            Close (Dir);
         end if;

      end Process_Directory;

   --  Start of processing for Process

   begin
      Processed_Directories.Set_Last (0);

      --  Process each directory

      for Index in Directories'Range  loop
         Process_Directory (Directories (Index).all);
      end loop;
   end Process;

   --------------------
   -- Output_Version --
   --------------------

   procedure Output_Version is
   begin
      if not Version_Output then
         Version_Output := True;
         Output.Write_Eol;
         Display_Version ("GNATNAME", "2001");
      end if;
   end Output_Version;

   ---------------
   -- Scan_Args --
   ---------------

   procedure Scan_Args is

      procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

      Project_File_Name_Expected : Boolean;

      Pragmas_File_Expected : Boolean;

      Directory_Expected : Boolean;

      Dir_File_Name_Expected : Boolean;

      Foreign_Pattern_Expected : Boolean;

      Excluded_Pattern_Expected : Boolean;

      procedure Check_Regular_Expression (S : String);
      --  Compile string S into a Regexp, fail if any error

      -----------------------------
      -- Check_Regular_Expression--
      -----------------------------

      procedure Check_Regular_Expression (S : String) is
         Dummy : Regexp;
         pragma Warnings (Off, Dummy);
      begin
         Dummy := Compile (S, Glob => True);
      exception
         when Error_In_Regexp =>
            Fail ("invalid regular expression """ & S & """");
      end Check_Regular_Expression;

   --  Start of processing for Scan_Args

   begin
      --  First check for --version or --help

      Check_Version_And_Help ("GNATNAME", "2001");

      --  Now scan the other switches

      Project_File_Name_Expected := False;
      Pragmas_File_Expected      := False;
      Directory_Expected         := False;
      Dir_File_Name_Expected     := False;
      Foreign_Pattern_Expected   := False;
      Excluded_Pattern_Expected  := False;

      for Next_Arg in 1 .. Argument_Count loop
         declare
            Next_Argv : constant String := Argument (Next_Arg);
            Arg       : String (1 .. Next_Argv'Length) := Next_Argv;

         begin
            if Arg'Length > 0 then

               --  -P xxx

               if Project_File_Name_Expected then
                  if Arg (1) = '-' then
                     Fail ("project file name missing");

                  else
                     File_Set       := True;
                     File_Path      := new String'(Arg);
                     Project_File_Name_Expected := False;
                  end if;

               --  -c file

               elsif Pragmas_File_Expected then
                  File_Set := True;
                  File_Path := new String'(Arg);
                  Pragmas_File_Expected := False;

               --  -d xxx

               elsif Directory_Expected then
                  Add_Source_Directory (Arg);
                  Directory_Expected := False;

               --  -D xxx

               elsif Dir_File_Name_Expected then
                  Get_Directories (Arg);
                  Dir_File_Name_Expected := False;

               --  -f xxx

               elsif Foreign_Pattern_Expected then
                  Patterns.Append
                    (Arguments.Table (Arguments.Last).Foreign_Patterns,
                     new String'(Arg));
                  Check_Regular_Expression (Arg);
                  Foreign_Pattern_Expected := False;

               --  -x xxx

               elsif Excluded_Pattern_Expected then
                  Patterns.Append
                    (Arguments.Table (Arguments.Last).Excluded_Patterns,
                     new String'(Arg));
                  Check_Regular_Expression (Arg);
                  Excluded_Pattern_Expected := False;

               --  There must be at least one Ada pattern or one foreign
               --  pattern for the previous section.

               --  --and

               elsif Arg = "--and" then

                  if Patterns.Last
                    (Arguments.Table (Arguments.Last).Name_Patterns) = 0
                    and then
                      Patterns.Last
                        (Arguments.Table (Arguments.Last).Foreign_Patterns) = 0
                  then
                     Try_Help;
                     return;
                  end if;

                  --  If no directory were specified for the previous section,
                  --  then the directory is the project directory.

                  if Patterns.Last
                    (Arguments.Table (Arguments.Last).Directories) = 0
                  then
                     Patterns.Append
                       (Arguments.Table (Arguments.Last).Directories,
                        new String'("."));
                  end if;

                  --  Add and initialize another component to Arguments table

                  declare
                     New_Arguments : Argument_Data;
                     pragma Warnings (Off, New_Arguments);
                     --  Declaring this defaulted initialized object ensures
                     --  that the new allocated component of table Arguments
                     --  is correctly initialized.

                     --  This is VERY ugly, Table should never be used with
                     --  data requiring default initialization. We should
                     --  find a way to avoid violating this rule ???

                  begin
                     Arguments.Append (New_Arguments);
                  end;

                  Patterns.Init
                    (Arguments.Table (Arguments.Last).Directories);
                  Patterns.Set_Last
                    (Arguments.Table (Arguments.Last).Directories, 0);
                  Patterns.Init
                    (Arguments.Table (Arguments.Last).Name_Patterns);
                  Patterns.Set_Last
                    (Arguments.Table (Arguments.Last).Name_Patterns, 0);
                  Patterns.Init
                    (Arguments.Table (Arguments.Last).Excluded_Patterns);
                  Patterns.Set_Last
                    (Arguments.Table (Arguments.Last).Excluded_Patterns, 0);
                  Patterns.Init
                    (Arguments.Table (Arguments.Last).Foreign_Patterns);
                  Patterns.Set_Last
                    (Arguments.Table (Arguments.Last).Foreign_Patterns, 0);

               --  Subdirectory switch

               elsif Arg'Length > Subdirs_Switch'Length
                 and then Arg (1 .. Subdirs_Switch'Length) = Subdirs_Switch
               then
                  null;
                  --  Subdirs are only used in gprname

               --  --no-backup

               elsif Arg = "--no-backup" then
                  Opt.No_Backup := True;

               --  -c

               elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-c" then
                  if File_Set then
                     Fail ("only one -P or -c switch may be specified");
                  end if;

                  if Arg'Length = 2 then
                     Pragmas_File_Expected := True;

                     if Next_Arg = Argument_Count then
                        Fail ("configuration pragmas file name missing");
                     end if;

                  else
                     File_Set := True;
                     File_Path := new String'(Arg (3 .. Arg'Last));
                  end if;

               --  -d

               elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-d" then
                  if Arg'Length = 2 then
                     Directory_Expected := True;

                     if Next_Arg = Argument_Count then
                        Fail ("directory name missing");
                     end if;

                  else
                     Add_Source_Directory (Arg (3 .. Arg'Last));
                  end if;

               --  -D

               elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-D" then
                  if Arg'Length = 2 then
                     Dir_File_Name_Expected := True;

                     if Next_Arg = Argument_Count then
                        Fail ("directory list file name missing");
                     end if;

                  else
                     Get_Directories (Arg (3 .. Arg'Last));
                  end if;

               --  -eL

               elsif Arg = "-eL" then
                  Opt.Follow_Links_For_Files := True;
                  Opt.Follow_Links_For_Dirs  := True;

               --  -f

               elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-f" then
                  if Arg'Length = 2 then
                     Foreign_Pattern_Expected := True;

                     if Next_Arg = Argument_Count then
                        Fail ("foreign pattern missing");
                     end if;

                  else
                     Patterns.Append
                       (Arguments.Table (Arguments.Last).Foreign_Patterns,
                        new String'(Arg (3 .. Arg'Last)));
                     Check_Regular_Expression (Arg (3 .. Arg'Last));
                  end if;

               --  -gnatep or -gnateD

               elsif Arg'Length > 7 and then
                 (Arg  (1 .. 7) = "-gnatep" or else Arg (1 .. 7) = "-gnateD")
               then
                  Preprocessor_Switches.Append (new String'(Arg));

               --  -h

               elsif Arg = "-h" then
                  Usage_Needed := True;

               --  -P

               elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-P" then
                  if File_Set then
                     Fail ("only one -c or -P switch may be specified");
                  end if;

                  if Arg'Length = 2 then
                     if Next_Arg = Argument_Count then
                        Fail ("project file name missing");

                     else
                        Project_File_Name_Expected := True;
                     end if;

                  else
                     File_Set       := True;
                     File_Path      := new String'(Arg (3 .. Arg'Last));
                  end if;

                  Create_Project := True;

               --  -v

               elsif Arg = "-v" then
                  if Opt.Verbose_Mode then
                     Very_Verbose := True;
                  else
                     Opt.Verbose_Mode := True;
                  end if;

               --  -x

               elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-x" then
                  if Arg'Length = 2 then
                     Excluded_Pattern_Expected := True;

                     if Next_Arg = Argument_Count then
                        Fail ("excluded pattern missing");
                     end if;

                  else
                     Patterns.Append
                       (Arguments.Table (Arguments.Last).Excluded_Patterns,
                        new String'(Arg (3 .. Arg'Last)));
                     Check_Regular_Expression (Arg (3 .. Arg'Last));
                  end if;

               --  Junk switch starting with minus

               elsif Arg (1) = '-' then
                  Fail ("wrong switch: " & Arg);

               --  Not a recognized switch, assume file name

               else
                  Canonical_Case_File_Name (Arg);
                  Patterns.Append
                    (Arguments.Table (Arguments.Last).Name_Patterns,
                     new String'(Arg));
                  Check_Regular_Expression (Arg);
               end if;
            end if;
         end;
      end loop;
   end Scan_Args;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      if not Usage_Output then
         Usage_Needed := False;
         Usage_Output := True;
         Output.Write_Str ("Usage: ");
         Osint.Write_Program_Name;
         Output.Write_Line (" [switches] naming-pattern [naming-patterns]");
         Output.Write_Line
           ("   {--and [switches] naming-pattern [naming-patterns]}");
         Output.Write_Eol;
         Output.Write_Line ("switches:");

         Display_Usage_Version_And_Help;

         Output.Write_Line
           ("  --subdirs=dir real obj/lib/exec dirs are subdirs");
         Output.Write_Line
           ("  --no-backup   do not create backup of project file");
         Output.Write_Eol;

         Output.Write_Line ("  --and        use different patterns");
         Output.Write_Eol;

         Output.Write_Line
           ("  -cfile       create configuration pragmas file");
         Output.Write_Line ("  -ddir        use dir as one of the source " &
                            "directories");
         Output.Write_Line ("  -Dfile       get source directories from file");
         Output.Write_Line
           ("  -eL          follow symbolic links when processing " &
            "project files");
         Output.Write_Line ("  -fpat        foreign pattern");
         Output.Write_Line
           ("  -gnateDsym=v preprocess with symbol definition");
         Output.Write_Line ("  -gnatep=data preprocess files with data file");
         Output.Write_Line ("  -h           output this help message");
         Output.Write_Line
           ("  -Pproj       update or create project file proj");
         Output.Write_Line ("  -v           verbose output");
         Output.Write_Line ("  -v -v        very verbose output");
         Output.Write_Line ("  -xpat        exclude pattern pat");
      end if;
   end Usage;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol is
   begin
      Write_A_String ((1 => ASCII.LF));
   end Write_Eol;

   --------------------
   -- Write_A_String --
   --------------------

   procedure Write_A_String (S : String) is
      Str : String (1 .. S'Length);

   begin
      if S'Length > 0 then
         Str := S;

         if Write (Output_FD, Str (1)'Address, Str'Length) /= Str'Length then
            Fail_Program ("disk full");
         end if;
      end if;
   end Write_A_String;

--  Start of processing for Gnatname

begin
   --  Add the directory where gnatname is invoked in front of the
   --  path, if gnatname is invoked with directory information.

   declare
      Command : constant String := Command_Name;

   begin
      for Index in reverse Command'Range loop
         if Command (Index) = Directory_Separator then
            declare
               Absolute_Dir : constant String :=
                                Normalize_Pathname
                                  (Command (Command'First .. Index));

               PATH         : constant String :=
                                Absolute_Dir &
                                Path_Separator &
                                Getenv ("PATH").all;

            begin
               Setenv ("PATH", PATH);
            end;

            exit;
         end if;
      end loop;
   end;

   --  Initialize tables

   Arguments.Set_Last (0);
   declare
      New_Arguments : Argument_Data;
      pragma Warnings (Off, New_Arguments);
      --  Declaring this defaulted initialized object ensures that the new
      --  allocated component of table Arguments is correctly initialized.
   begin
      Arguments.Append (New_Arguments);
   end;

   Patterns.Init (Arguments.Table (1).Directories);
   Patterns.Set_Last (Arguments.Table (1).Directories, 0);
   Patterns.Init (Arguments.Table (1).Name_Patterns);
   Patterns.Set_Last (Arguments.Table (1).Name_Patterns, 0);
   Patterns.Init (Arguments.Table (1).Excluded_Patterns);
   Patterns.Set_Last (Arguments.Table (1).Excluded_Patterns, 0);
   Patterns.Init (Arguments.Table (1).Foreign_Patterns);
   Patterns.Set_Last (Arguments.Table (1).Foreign_Patterns, 0);

   Preprocessor_Switches.Set_Last (0);

   --  Get the arguments

   Scan_Args;

   if Create_Project then
      declare
         Gprname_Path : constant String_Access :=
           Locate_Exec_On_Path ("gprname");
         Arg_Len : Natural       := Argument_Count;
         Pos     : Natural       := 0;
         Target  : String_Access := null;
         Success : Boolean       := False;
      begin
         if Gprname_Path = null then
            Fail_Program
              ("project files are no longer supported by gnatname;" &
               " use gprname instead");
         end if;

         Find_Program_Name;

         if Name_Len > 9
            and then Name_Buffer (Name_Len - 7 .. Name_Len) = "gnatname"
         then
            Target  := new String'(Name_Buffer (1 .. Name_Len - 9));
            Arg_Len := Arg_Len + 1;
         end if;

         declare
            Args : Argument_List (1 .. Arg_Len);
         begin
            if Target /= null then
               Args (1) := new String'("--target=" & Target.all);
               Pos := 1;
            end if;

            for J in 1 .. Argument_Count loop
               Pos := Pos + 1;
               Args (Pos) := new String'(Argument (J));
            end loop;

            Spawn (Gprname_Path.all, Args, Success);

            if Success then
               Exit_Program (E_Success);
            else
               Exit_Program (E_Errors);
            end if;
         end;
      end;
   end if;

   if Opt.Verbose_Mode then
      Output_Version;
   end if;

   if Usage_Needed then
      Usage;
   end if;

   --  If no Ada or foreign pattern was specified, print the usage and return

   if Patterns.Last (Arguments.Table (Arguments.Last).Name_Patterns) = 0
        and then
      Patterns.Last (Arguments.Table (Arguments.Last).Foreign_Patterns) = 0
   then
      if Argument_Count = 0 then
         Usage;
      elsif not Usage_Output then
         Try_Help;
      end if;

      return;
   end if;

   --  If no source directory was specified, use the current directory as the
   --  unique directory. Note that if a file was specified with directory
   --  information, the current directory is the directory of the specified
   --  file.

   if Patterns.Last (Arguments.Table (Arguments.Last).Directories) = 0 then
      Patterns.Append
        (Arguments.Table (Arguments.Last).Directories, new String'("."));
   end if;

   --  Initialize

   declare
      Prep_Switches : Argument_List
                        (1 .. Integer (Preprocessor_Switches.Last));

   begin
      for Index in Prep_Switches'Range loop
         Prep_Switches (Index) := Preprocessor_Switches.Table (Index);
      end loop;

      Initialize
        (File_Path         => File_Path.all,
         Preproc_Switches  => Prep_Switches);
   end;

   --  Process each section successively

   for J in 1 .. Arguments.Last loop
      declare
         Directories   : Argument_List
           (1 .. Integer
                   (Patterns.Last (Arguments.Table (J).Directories)));
         Name_Patterns : Regexp_List
           (1 .. Integer
                   (Patterns.Last (Arguments.Table (J).Name_Patterns)));
         Excl_Patterns : Regexp_List
           (1 .. Integer
                   (Patterns.Last (Arguments.Table (J).Excluded_Patterns)));
         Frgn_Patterns : Regexp_List
           (1 .. Integer
                   (Patterns.Last (Arguments.Table (J).Foreign_Patterns)));

      begin
         --  Build the Directories and Patterns arguments

         for Index in Directories'Range loop
            Directories (Index) :=
              Arguments.Table (J).Directories.Table (Index);
         end loop;

         for Index in Name_Patterns'Range loop
            Name_Patterns (Index) :=
              Compile
                (Arguments.Table (J).Name_Patterns.Table (Index).all,
                 Glob => True);
         end loop;

         for Index in Excl_Patterns'Range loop
            Excl_Patterns (Index) :=
              Compile
                (Arguments.Table (J).Excluded_Patterns.Table (Index).all,
                 Glob => True);
         end loop;

         for Index in Frgn_Patterns'Range loop
            Frgn_Patterns (Index) :=
              Compile
                (Arguments.Table (J).Foreign_Patterns.Table (Index).all,
                 Glob => True);
         end loop;

         --  Call Prj.Makr.Process where the real work is done

         Process
           (Directories       => Directories,
            Name_Patterns     => Name_Patterns,
            Excluded_Patterns => Excl_Patterns,
            Foreign_Patterns  => Frgn_Patterns);
      end;
   end loop;

   --  Finalize

   Finalize;

   if Opt.Verbose_Mode then
      Output.Write_Eol;
   end if;
end Gnatname;
