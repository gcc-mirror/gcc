------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T N A M E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2015, Free Software Foundation, Inc.         --
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

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Dynamic_Tables;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj;      use Prj;
with Prj.Makr;
with Switch;   use Switch;
with Table;

with System.Regexp; use System.Regexp;

procedure Gnatname is

   Subdirs_Switch : constant String := "--subdirs=";

   Usage_Output : Boolean := False;
   --  Set to True when usage is output, to avoid multiple output

   Usage_Needed : Boolean := False;
   --  Set to True by -h switch

   Version_Output : Boolean := False;
   --  Set to True when version is output, to avoid multiple output

   Very_Verbose : Boolean := False;
   --  Set to True with -v -v

   Create_Project : Boolean := False;
   --  Set to True with a -P switch

   File_Path : String_Access := new String'("gnat.adc");
   --  Path name of the file specified by -c or -P switch

   File_Set : Boolean := False;
   --  Set to True by -c or -P switch.
   --  Used to detect multiple -c/-P switches.

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

   --------------------------
   -- Add_Source_Directory --
   --------------------------

   procedure Add_Source_Directory (S : String) is
   begin
      Patterns.Append
        (Arguments.Table (Arguments.Last).Directories, new String'(S));
   end Add_Source_Directory;

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
                  Create_Project := False;
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
                  Subdirs :=
                    new String'(Arg (Subdirs_Switch'Length + 1 .. Arg'Last));

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
                     Create_Project := False;
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
         Write_Str ("Usage: ");
         Osint.Write_Program_Name;
         Write_Line (" [switches] naming-pattern [naming-patterns]");
         Write_Line ("   {--and [switches] naming-pattern [naming-patterns]}");
         Write_Eol;
         Write_Line ("switches:");

         Display_Usage_Version_And_Help;

         Write_Line ("  --subdirs=dir real obj/lib/exec dirs are subdirs");
         Write_Line ("  --no-backup   do not create backup of project file");
         Write_Eol;

         Write_Line ("  --and        use different patterns");
         Write_Eol;

         Write_Line ("  -cfile       create configuration pragmas file");
         Write_Line ("  -ddir        use dir as one of the source " &
                     "directories");
         Write_Line ("  -Dfile       get source directories from file");
         Write_Line ("  -eL          follow symbolic links when processing " &
                     "project files");
         Write_Line ("  -fpat        foreign pattern");
         Write_Line ("  -gnateDsym=v preprocess with symbol definition");
         Write_Line ("  -gnatep=data preprocess files with data file");
         Write_Line ("  -h           output this help message");
         Write_Line ("  -Pproj       update or create project file proj");
         Write_Line ("  -v           verbose output");
         Write_Line ("  -v -v        very verbose output");
         Write_Line ("  -xpat        exclude pattern pat");
      end if;
   end Usage;

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

   if Opt.Verbose_Mode then
      Output_Version;
   end if;

   if Usage_Needed then
      Usage;
   end if;

   if Create_Project then
      declare
         Gnatname : constant String_Access :=
                      Program_Name ("gnatname", "gnatname");
         Arg_Len  : Positive      := Argument_Count;
         Target   : String_Access := null;

      begin
         --  Find the target, if any

         if Gnatname.all /= "gnatname" then
            Target :=
              new String'(Gnatname (Gnatname'First .. Gnatname'Last - 9));
            Arg_Len := Arg_Len + 1;
         end if;

         declare
            Args    : Argument_List (1 .. Arg_Len);
            Gprname : String_Access :=
                        Locate_Exec_On_Path (Exec_Name => "gprname");
            Success : Boolean;

         begin
            if Gprname /= null then
               for J in 1 .. Argument_Count loop
                  Args (J) := new String'(Argument (J));
               end loop;

               --  Add the target if there is one

               if Target /= null then
                  Args (Args'Last) := new String'("--target=" & Target.all);
               end if;

               Spawn (Gprname.all, Args, Success);

               Free (Gprname);

               if Success then
                  Exit_Program (E_Success);
               end if;
            end if;
         end;
      end;
   end if;

   --  This only happens if gprname is not found or if the invocation of
   --  gprname did not succeed.

   if Create_Project then
      Write_Line
        ("warning: gnatname -P is obsolete and will not be available in the" &
         " next release; use gprname instead");
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

      Prj.Makr.Initialize
        (File_Path         => File_Path.all,
         Project_File      => Create_Project,
         Preproc_Switches  => Prep_Switches,
         Very_Verbose      => Very_Verbose,
         Flags             => Gnatmake_Flags);
   end;

   --  Process each section successively

   for J in 1 .. Arguments.Last loop
      declare
         Directories   : Argument_List
           (1 .. Integer
                   (Patterns.Last (Arguments.Table (J).Directories)));
         Name_Patterns : Prj.Makr.Regexp_List
           (1 .. Integer
                   (Patterns.Last (Arguments.Table (J).Name_Patterns)));
         Excl_Patterns : Prj.Makr.Regexp_List
           (1 .. Integer
                   (Patterns.Last (Arguments.Table (J).Excluded_Patterns)));
         Frgn_Patterns : Prj.Makr.Regexp_List
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

         Prj.Makr.Process
           (Directories       => Directories,
            Name_Patterns     => Name_Patterns,
            Excluded_Patterns => Excl_Patterns,
            Foreign_Patterns  => Frgn_Patterns);
      end;
   end loop;

   --  Finalize

   Prj.Makr.Finalize;

   if Opt.Verbose_Mode then
      Write_Eol;
   end if;
end Gnatname;
