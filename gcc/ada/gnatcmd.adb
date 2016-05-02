------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T C M D                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2016, Free Software Foundation, Inc.         --
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

with Csets;
with Gnatvsn;
with Makeutl;  use Makeutl;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj;      use Prj;
with Prj.Env;
with Prj.Ext;  use Prj.Ext;
with Prj.Pars;
with Prj.Tree; use Prj.Tree;
with Prj.Util; use Prj.Util;
with Sdefault;
with Sinput.P;
with Snames;   use Snames;
with Stringt;
with Switch;   use Switch;
with Table;
with Tempdir;
with Types;    use Types;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

procedure GNATCmd is
   Gprbuild : constant String := "gprbuild";
   Gprclean : constant String := "gprclean";
   Gprname  : constant String := "gprname";
   Gprls    : constant String := "gprls";

   Error_Exit : exception;
   --  Raise this exception if error detected

   type Command_Type is
     (Bind,
      Chop,
      Clean,
      Compile,
      Check,
      Elim,
      Find,
      Krunch,
      Link,
      List,
      Make,
      Metric,
      Name,
      Preprocess,
      Pretty,
      Stack,
      Stub,
      Test,
      Xref,
      Undefined);

   subtype Real_Command_Type is Command_Type range Bind .. Xref;
   --  All real command types (excludes only Undefined).

   type Alternate_Command is (Comp, Ls, Kr, Pp, Prep);
   --  Alternate command label

   Corresponding_To : constant array (Alternate_Command) of Command_Type :=
     (Comp  => Compile,
      Ls    => List,
      Kr    => Krunch,
      Prep  => Preprocess,
      Pp    => Pretty);
   --  Mapping of alternate commands to commands

   Call_GPR_Tool : Boolean := False;
   --  True when a GPR tool should be called, if available

   Project_Node_Tree : Project_Node_Tree_Ref;
   Project_File      : String_Access;
   Project           : Prj.Project_Id;
   Current_Verbosity : Prj.Verbosity := Prj.Default;
   Tool_Package_Name : Name_Id       := No_Name;

   Project_Tree : constant Project_Tree_Ref :=
                    new Project_Tree_Data (Is_Root_Tree => True);
   --  The project tree

   All_Projects : Boolean := False;

   Temp_File_Name : Path_Name_Type := No_Path;
   --  The name of the temporary text file to put a list of source/object
   --  files to pass to a tool.

   package First_Switches is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Gnatcmd.First_Switches");
   --  A table to keep the switches from the project file

   package Last_Switches is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Gnatcmd.Last_Switches");

   ----------------------------------
   -- Declarations for GNATCMD use --
   ----------------------------------

   The_Command : Command_Type;
   --  The command specified in the invocation of the GNAT driver

   Command_Arg : Positive := 1;
   --  The index of the command in the arguments of the GNAT driver

   My_Exit_Status : Exit_Status := Success;
   --  The exit status of the spawned tool

   type Command_Entry is record
      Cname : String_Access;
      --  Command name for GNAT xxx command

      Unixcmd : String_Access;
      --  Corresponding Unix command

      Unixsws : Argument_List_Access;
      --  List of switches to be used with the Unix command
   end record;

   Command_List : constant array (Real_Command_Type) of Command_Entry :=
     (Bind =>
        (Cname    => new String'("BIND"),
         Unixcmd  => new String'("gnatbind"),
         Unixsws  => null),

      Chop =>
        (Cname    => new String'("CHOP"),
         Unixcmd  => new String'("gnatchop"),
         Unixsws  => null),

      Clean =>
        (Cname    => new String'("CLEAN"),
         Unixcmd  => new String'("gnatclean"),
         Unixsws  => null),

      Compile =>
        (Cname    => new String'("COMPILE"),
         Unixcmd  => new String'("gnatmake"),
         Unixsws  => new Argument_List'(1 => new String'("-f"),
                                        2 => new String'("-u"),
                                        3 => new String'("-c"))),

      Check =>
        (Cname    => new String'("CHECK"),
         Unixcmd  => new String'("gnatcheck"),
         Unixsws  => null),

      Elim =>
        (Cname    => new String'("ELIM"),
         Unixcmd  => new String'("gnatelim"),
         Unixsws  => null),

      Find =>
        (Cname    => new String'("FIND"),
         Unixcmd  => new String'("gnatfind"),
         Unixsws  => null),

      Krunch =>
        (Cname    => new String'("KRUNCH"),
         Unixcmd  => new String'("gnatkr"),
         Unixsws  => null),

      Link =>
        (Cname    => new String'("LINK"),
         Unixcmd  => new String'("gnatlink"),
         Unixsws  => null),

      List =>
        (Cname    => new String'("LIST"),
         Unixcmd  => new String'("gnatls"),
         Unixsws  => null),

      Make =>
        (Cname    => new String'("MAKE"),
         Unixcmd  => new String'("gnatmake"),
         Unixsws  => null),

      Metric =>
        (Cname    => new String'("METRIC"),
         Unixcmd  => new String'("gnatmetric"),
         Unixsws  => null),

      Name =>
        (Cname    => new String'("NAME"),
         Unixcmd  => new String'("gnatname"),
         Unixsws  => null),

      Preprocess =>
        (Cname    => new String'("PREPROCESS"),
         Unixcmd  => new String'("gnatprep"),
         Unixsws  => null),

      Pretty =>
        (Cname    => new String'("PRETTY"),
         Unixcmd  => new String'("gnatpp"),
         Unixsws  => null),

      Stack =>
        (Cname    => new String'("STACK"),
         Unixcmd  => new String'("gnatstack"),
         Unixsws  => null),

      Stub =>
        (Cname    => new String'("STUB"),
         Unixcmd  => new String'("gnatstub"),
         Unixsws  => null),

      Test =>
        (Cname    => new String'("TEST"),
         Unixcmd  => new String'("gnattest"),
         Unixsws  => null),

      Xref =>
        (Cname    => new String'("XREF"),
         Unixcmd  => new String'("gnatxref"),
         Unixsws  => null)
     );

   subtype SA is String_Access;

   Naming_String      : constant SA := new String'("naming");
   Gnatls_String      : constant SA := new String'("gnatls");

   Packages_To_Check_By_Gnatls    : constant String_List_Access :=
     new String_List'((Naming_String, Gnatls_String));

   Packages_To_Check : String_List_Access := Prj.All_Packages;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Files;
   --  For GNAT LIST -V, check if a project file is specified, without any file
   --  arguments and without a switch -files=. If it is the case, invoke the
   --  GNAT tool with the proper list of files, derived from the sources of
   --  the project.

   procedure Output_Version;
   --  Output the version of this program

   procedure Usage;
   --  Display usage

   -----------------
   -- Check_Files --
   -----------------

   procedure Check_Files is
      Add_Sources : Boolean := True;
      Unit        : Prj.Unit_Index;
      Subunit     : Boolean := False;
      FD          : File_Descriptor := Invalid_FD;
      Status      : Integer;
      Success     : Boolean;

      procedure Add_To_Response_File
        (File_Name  : String;
         Check_File : Boolean := True);
      --  Include the file name passed as parameter in the response file for
      --  the tool being called. If the response file can not be written then
      --  the file name is passed in the parameter list of the tool. If the
      --  Check_File parameter is True then the procedure verifies the
      --  existence of the file before adding it to the response file.

      --------------------------
      -- Add_To_Response_File --
      --------------------------

      procedure Add_To_Response_File
        (File_Name  : String;
         Check_File : Boolean := True)
      is
      begin
         Name_Len := 0;

         Add_Str_To_Name_Buffer (File_Name);

         if not Check_File or else
           Is_Regular_File (Name_Buffer (1 .. Name_Len))
         then
            if FD /= Invalid_FD then
               Name_Len := Name_Len + 1;
               Name_Buffer (Name_Len) := ASCII.LF;

               Status := Write (FD, Name_Buffer (1)'Address, Name_Len);

               if Status /= Name_Len then
                  Osint.Fail ("disk full");
               end if;
            else
               Last_Switches.Increment_Last;
               Last_Switches.Table (Last_Switches.Last) :=
                 new String'(File_Name);
            end if;
         end if;
      end Add_To_Response_File;

   --  Start of processing for Check_Files

   begin
      --  Check if there is at least one argument that is not a switch

      for Index in 1 .. Last_Switches.Last loop
         if Last_Switches.Table (Index) (1) /= '-'
           or else (Last_Switches.Table (Index).all'Length > 7
                     and then Last_Switches.Table (Index) (1 .. 7) = "-files=")
         then
            Add_Sources := False;
            exit;
         end if;
      end loop;

      --  If all arguments are switches and there is no switch -files=, add the
      --  path names of all the sources of the main project.

      if Add_Sources then
         Tempdir.Create_Temp_File (FD, Temp_File_Name);
         Last_Switches.Increment_Last;
         Last_Switches.Table (Last_Switches.Last) :=
           new String'("-files=" & Get_Name_String (Temp_File_Name));

         Unit := Units_Htable.Get_First (Project_Tree.Units_HT);
         while Unit /= No_Unit_Index loop

            --  We only need to put the library units, body or spec, but not
            --  the subunits.

            if Unit.File_Names (Impl) /= null
              and then not Unit.File_Names (Impl).Locally_Removed
            then
               --  There is a body, check if it is for this project

               if All_Projects
                 or else Unit.File_Names (Impl).Project = Project
               then
                  Subunit := False;

                  if Unit.File_Names (Spec) = null
                    or else Unit.File_Names (Spec).Locally_Removed
                  then
                     --  We have a body with no spec: we need to check if
                     --  this is a subunit, because gnatls will complain
                     --  about subunits.

                     declare
                        Src_Ind : constant Source_File_Index :=
                                    Sinput.P.Load_Project_File
                                      (Get_Name_String
                                         (Unit.File_Names (Impl).Path.Name));
                     begin
                        Subunit := Sinput.P.Source_File_Is_Subunit (Src_Ind);
                     end;
                  end if;

                  if not Subunit then
                     Add_To_Response_File
                       (Get_Name_String (Unit.File_Names (Impl).Display_File),
                        Check_File => False);
                  end if;
               end if;

            elsif Unit.File_Names (Spec) /= null
              and then not Unit.File_Names (Spec).Locally_Removed
            then
               --  We have a spec with no body. Check if it is for this project

               if All_Projects
                 or else Unit.File_Names (Spec).Project = Project
               then
                  Add_To_Response_File
                    (Get_Name_String (Unit.File_Names (Spec).Display_File),
                     Check_File => False);
               end if;
            end if;

            Unit := Units_Htable.Get_Next (Project_Tree.Units_HT);
         end loop;

         if FD /= Invalid_FD then
            Close (FD, Success);

            if not Success then
               Osint.Fail ("disk full");
            end if;
         end if;
      end if;
   end Check_Files;

   --------------------
   -- Output_Version --
   --------------------

   procedure Output_Version is
   begin
      Put ("GNAT ");
      Put_Line (Gnatvsn.Gnat_Version_String);
      Put_Line ("Copyright 1996-" & Gnatvsn.Current_Year
                & ", Free Software Foundation, Inc.");
   end Output_Version;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Output_Version;
      New_Line;
      Put_Line ("List of available commands");
      New_Line;

      for C in Command_List'Range loop
         Put ("gnat ");
         Put (To_Lower (Command_List (C).Cname.all));
         Set_Col (25);
         Put (Program_Name (Command_List (C).Unixcmd.all, "gnat").all);

         declare
            Sws : Argument_List_Access renames Command_List (C).Unixsws;
         begin
            if Sws /= null then
               for J in Sws'Range loop
                  Put (' ');
                  Put (Sws (J).all);
               end loop;
            end if;
         end;

         New_Line;
      end loop;

      New_Line;
   end Usage;

   procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

--  Start of processing for GNATCmd

begin
   --  All output from GNATCmd is debugging or error output: send to stderr

   Set_Standard_Error;

   --  Initializations

   Csets.Initialize;
   Snames.Initialize;
   Stringt.Initialize;

   Prj.Tree.Initialize (Root_Environment, Gnatmake_Flags);

   Project_Node_Tree := new Project_Node_Tree_Data;
   Prj.Tree.Initialize (Project_Node_Tree);

   Prj.Initialize (Project_Tree);

   Last_Switches.Init;
   Last_Switches.Set_Last (0);

   First_Switches.Init;
   First_Switches.Set_Last (0);

   --  Put the command line in environment variable GNAT_DRIVER_COMMAND_LINE,
   --  so that the spawned tool may know the way the GNAT driver was invoked.

   Name_Len := 0;
   Add_Str_To_Name_Buffer (Command_Name);

   for J in 1 .. Argument_Count loop
      Add_Char_To_Name_Buffer (' ');
      Add_Str_To_Name_Buffer (Argument (J));
   end loop;

   Setenv ("GNAT_DRIVER_COMMAND_LINE", Name_Buffer (1 .. Name_Len));

   --  Add the directory where the GNAT driver is invoked in front of the path,
   --  if the GNAT driver is invoked with directory information.

   declare
      Command : constant String := Command_Name;

   begin
      for Index in reverse Command'Range loop
         if Command (Index) = Directory_Separator then
            declare
               Absolute_Dir : constant String :=
                 Normalize_Pathname (Command (Command'First .. Index));
               PATH         : constant String :=
                 Absolute_Dir & Path_Separator & Getenv ("PATH").all;
            begin
               Setenv ("PATH", PATH);
            end;

            exit;
         end if;
      end loop;
   end;

   --  Scan the command line

   --  First, scan to detect --version and/or --help

   Check_Version_And_Help ("GNAT", "1996");

   begin
      loop
         if Command_Arg <= Argument_Count
           and then Argument (Command_Arg) = "-v"
         then
            Verbose_Mode := True;
            Command_Arg := Command_Arg + 1;

         elsif Command_Arg <= Argument_Count
           and then Argument (Command_Arg) = "-dn"
         then
            Keep_Temporary_Files := True;
            Command_Arg := Command_Arg + 1;

         else
            exit;
         end if;
      end loop;

      --  If there is no command, just output the usage

      if Command_Arg > Argument_Count then
         Usage;
         return;
      end if;

      The_Command := Real_Command_Type'Value (Argument (Command_Arg));

   exception
      when Constraint_Error =>

         --  Check if it is an alternate command

         declare
            Alternate : Alternate_Command;

         begin
            Alternate := Alternate_Command'Value (Argument (Command_Arg));
            The_Command := Corresponding_To (Alternate);

         exception
            when Constraint_Error =>
               Usage;
               Fail ("unknown command: " & Argument (Command_Arg));
         end;
   end;

   --  Get the arguments from the command line and from the eventual
   --  argument file(s) specified on the command line.

   for Arg in Command_Arg + 1 .. Argument_Count loop
      declare
         The_Arg : constant String := Argument (Arg);

      begin
         --  Check if an argument file is specified

         if The_Arg (The_Arg'First) = '@' then
            declare
               Arg_File : Ada.Text_IO.File_Type;
               Line     : String (1 .. 256);
               Last     : Natural;

            begin
               --  Open the file and fail if the file cannot be found

               begin
                  Open (Arg_File, In_File,
                        The_Arg (The_Arg'First + 1 .. The_Arg'Last));

               exception
                  when others =>
                     Put (Standard_Error, "Cannot open argument file """);
                     Put (Standard_Error,
                          The_Arg (The_Arg'First + 1 .. The_Arg'Last));
                     Put_Line (Standard_Error, """");
                     raise Error_Exit;
               end;

               --  Read line by line and put the content of each non-
               --  empty line in the Last_Switches table.

               while not End_Of_File (Arg_File) loop
                  Get_Line (Arg_File, Line, Last);

                  if Last /= 0 then
                     Last_Switches.Increment_Last;
                     Last_Switches.Table (Last_Switches.Last) :=
                       new String'(Line (1 .. Last));
                  end if;
               end loop;

               Close (Arg_File);
            end;

         else
            --  It is not an argument file; just put the argument in
            --  the Last_Switches table.

            Last_Switches.Increment_Last;
            Last_Switches.Table (Last_Switches.Last) := new String'(The_Arg);
         end if;
      end;
   end loop;

   declare
      Program    : String_Access;
      Exec_Path  : String_Access;
      Get_Target : Boolean := False;

   begin
      if The_Command = Stack then

         --  Never call gnatstack with a prefix

         Program := new String'(Command_List (The_Command).Unixcmd.all);

      else
         Program :=
           Program_Name (Command_List (The_Command).Unixcmd.all, "gnat");

         --  If we want to invoke gnatmake/gnatclean with -P, then check if
         --  gprbuild/gprclean is available; if it is, use gprbuild/gprclean
         --  instead of gnatmake/gnatclean.
         --  Ditto for gnatname -> gprname and gnatls -> gprls.

         if The_Command = Make
           or else The_Command = Compile
           or else The_Command = Bind
           or else The_Command = Link
           or else The_Command = Clean
           or else The_Command = Name
           or else The_Command = List
         then
            declare
               Switch            : String_Access;
               Dash_V_Switch     : constant String := "-V";

            begin
               for J in 1 .. Last_Switches.Last loop
                  Switch := Last_Switches.Table (J);

                  if The_Command = List and then Switch.all = Dash_V_Switch
                  then
                     Call_GPR_Tool := False;
                     exit;
                  end if;

                  if Switch'Length >= 2
                    and then Switch (Switch'First .. Switch'First + 1) = "-P"
                  then
                     Call_GPR_Tool := True;
                  end if;
               end loop;

               if Call_GPR_Tool then
                  case The_Command is
                     when Make | Compile | Bind | Link =>
                        if Locate_Exec_On_Path (Gprbuild) /= null  then
                           Program    := new String'(Gprbuild);
                           Get_Target := True;

                           if The_Command = Bind then
                              First_Switches.Append (new String'("-b"));
                           elsif The_Command = Link then
                              First_Switches.Append (new String'("-l"));
                           end if;

                        elsif The_Command = Bind then
                           Fail
                             ("'gnat bind -P' is no longer supported;" &
                              " use 'gprbuild -b' instead.");

                        elsif The_Command = Link then
                           Fail
                             ("'gnat Link -P' is no longer supported;" &
                              " use 'gprbuild -l' instead.");
                        end if;

                     when Clean =>
                        if Locate_Exec_On_Path (Gprclean) /= null then
                           Program := new String'(Gprclean);
                           Get_Target := True;
                        end if;

                     when Name =>
                        if Locate_Exec_On_Path (Gprname) /= null then
                           Program := new String'(Gprname);
                           Get_Target := True;
                        end if;

                     when List =>
                        if Locate_Exec_On_Path (Gprls) /= null then
                           Program := new String'(Gprls);
                           Get_Target := True;
                        end if;

                     when others =>
                        null;
                  end case;

                  if Get_Target then
                     Find_Program_Name;

                     if Name_Len > 5 then
                        First_Switches.Append
                          (new String'
                             ("--target=" & Name_Buffer (1 .. Name_Len - 5)));
                     end if;
                  end if;
               end if;
            end;
         end if;
      end if;

      --  Locate the executable for the command

      Exec_Path := Locate_Exec_On_Path (Program.all);

      if Exec_Path = null then
         Put_Line (Standard_Error, "could not locate " & Program.all);
         raise Error_Exit;
      end if;

      --  If there are switches for the executable, put them as first switches

      if Command_List (The_Command).Unixsws /= null then
         for J in Command_List (The_Command).Unixsws'Range loop
            First_Switches.Increment_Last;
            First_Switches.Table (First_Switches.Last) :=
              Command_List (The_Command).Unixsws (J);
         end loop;
      end if;

      --  For FIND and XREF, look for switch -P. If it is specified, then
      --  report an error indicating that the command is no longer supporting
      --  project files.

      if The_Command = Find or else  The_Command = Xref then
         declare
            Argv    : String_Access;
         begin
            for Arg_Num in 1 .. Last_Switches.Last loop
               Argv := Last_Switches.Table (Arg_Num);

               if Argv'Length >= 2 and then
                  Argv (Argv'First .. Argv'First + 1) = "-P"
               then
                  if The_Command = Find then
                     Fail ("'gnat find -P' is no longer supported;");
                  else
                     Fail ("'gnat xref -P' is no longer supported;");
                  end if;
               end if;
            end loop;
         end;
      end if;

      if The_Command = List and then not Call_GPR_Tool then
         Tool_Package_Name := Name_Gnatls;
         Packages_To_Check := Packages_To_Check_By_Gnatls;

         --  Check that the switches are consistent. Detect project file
         --  related switches.

         Inspect_Switches : declare
            Arg_Num : Positive := 1;
            Argv    : String_Access;

            procedure Remove_Switch (Num : Positive);
            --  Remove a project related switch from table Last_Switches

            -------------------
            -- Remove_Switch --
            -------------------

            procedure Remove_Switch (Num : Positive) is
            begin
               Last_Switches.Table (Num .. Last_Switches.Last - 1) :=
                 Last_Switches.Table (Num + 1 .. Last_Switches.Last);
               Last_Switches.Decrement_Last;
            end Remove_Switch;

         --  Start of processing for Inspect_Switches

         begin
            while Arg_Num <= Last_Switches.Last loop
               Argv := Last_Switches.Table (Arg_Num);

               if Argv (Argv'First) = '-' then
                  if Argv'Length = 1 then
                     Fail ("switch character cannot be followed by a blank");
                  end if;

                  --  --subdirs=... Specify Subdirs

                  if Argv'Length > Makeutl.Subdirs_Option'Length
                    and then
                      Argv
                       (Argv'First ..
                        Argv'First + Makeutl.Subdirs_Option'Length - 1) =
                                                        Makeutl.Subdirs_Option
                  then
                     Subdirs :=
                       new String'
                         (Argv (Argv'First + Makeutl.Subdirs_Option'Length ..
                                Argv'Last));

                     Remove_Switch (Arg_Num);

                  --  -aPdir  Add dir to the project search path

                  elsif Argv'Length > 3
                    and then Argv (Argv'First + 1 .. Argv'First + 2) = "aP"
                  then
                     Prj.Env.Add_Directories
                       (Root_Environment.Project_Path,
                        Argv (Argv'First + 3 .. Argv'Last));

                     --  Pass -aPdir to gnatls, but not to other tools

                     if The_Command = List then
                        Arg_Num := Arg_Num + 1;
                     else
                        Remove_Switch (Arg_Num);
                     end if;

                  --  -eL  Follow links for files

                  elsif Argv.all = "-eL" then
                     Follow_Links_For_Files := True;
                     Follow_Links_For_Dirs  := True;

                     Remove_Switch (Arg_Num);

                  --  -vPx  Specify verbosity while parsing project files

                  elsif Argv'Length >= 3
                    and then  Argv (Argv'First + 1 .. Argv'First + 2) = "vP"
                  then
                     if Argv'Length = 4
                       and then Argv (Argv'Last) in '0' .. '2'
                     then
                        case Argv (Argv'Last) is
                           when '0' =>
                              Current_Verbosity := Prj.Default;
                           when '1' =>
                              Current_Verbosity := Prj.Medium;
                           when '2' =>
                              Current_Verbosity := Prj.High;
                           when others =>

                              --  Cannot happen

                              raise Program_Error;
                        end case;
                     else
                        Fail ("invalid verbosity level: "
                              & Argv (Argv'First + 3 .. Argv'Last));
                     end if;

                     Remove_Switch (Arg_Num);

                  --  -Pproject_file  Specify project file to be used

                  elsif Argv (Argv'First + 1) = 'P' then

                     --  Only one -P switch can be used

                     if Project_File /= null then
                        Fail
                          (Argv.all
                           & ": second project file forbidden (first is """
                           & Project_File.all & """)");

                     elsif Argv'Length = 2 then

                        --  There is space between -P and the project file
                        --  name. -P cannot be the last option.

                        if Arg_Num = Last_Switches.Last then
                           Fail ("project file name missing after -P");

                        else
                           Remove_Switch (Arg_Num);
                           Argv := Last_Switches.Table (Arg_Num);

                           --  After -P, there must be a project file name,
                           --  not another switch.

                           if Argv (Argv'First) = '-' then
                              Fail ("project file name missing after -P");

                           else
                              Project_File := new String'(Argv.all);
                           end if;
                        end if;

                     else
                        --  No space between -P and project file name

                        Project_File :=
                          new String'(Argv (Argv'First + 2 .. Argv'Last));
                     end if;

                     Remove_Switch (Arg_Num);

                  --  -Xexternal=value Specify an external reference to be
                  --                   used in project files

                  elsif Argv'Length >= 5
                    and then Argv (Argv'First + 1) = 'X'
                  then
                     if not Check (Root_Environment.External,
                                    Argv (Argv'First + 2 .. Argv'Last))
                     then
                        Fail
                          (Argv.all & " is not a valid external assignment.");
                     end if;

                     Remove_Switch (Arg_Num);

                  --  --unchecked-shared-lib-imports

                  elsif Argv.all = "--unchecked-shared-lib-imports" then
                     Opt.Unchecked_Shared_Lib_Imports := True;
                     Remove_Switch (Arg_Num);

                  --  gnat list -U

                  elsif
                    The_Command = List
                    and then Argv'Length = 2
                    and then Argv (2) = 'U'
                  then
                     All_Projects := True;
                     Remove_Switch (Arg_Num);

                  else
                     Arg_Num := Arg_Num + 1;
                  end if;

               else
                  Arg_Num := Arg_Num + 1;
               end if;
            end loop;
         end Inspect_Switches;
      end if;

      --  Add the default project search directories now, after the directories
      --  that have been specified by switches -aP<dir>.

      Prj.Env.Initialize_Default_Project_Path
        (Root_Environment.Project_Path,
         Target_Name => Sdefault.Target_Name.all);

      --  If there is a project file specified, parse it, get the switches
      --  for the tool and setup PATH environment variables.

      if Project_File /= null then
         Prj.Pars.Set_Verbosity (To => Current_Verbosity);

         Prj.Pars.Parse
           (Project           => Project,
            In_Tree           => Project_Tree,
            In_Node_Tree      => Project_Node_Tree,
            Project_File_Name => Project_File.all,
            Env               => Root_Environment,
            Packages_To_Check => Packages_To_Check);

         --  Prj.Pars.Parse calls Set_Standard_Output, reset to stderr

         Set_Standard_Error;

         if Project = Prj.No_Project then
            Fail ("""" & Project_File.all & """ processing failed");

         elsif Project.Qualifier = Aggregate then
            Fail ("aggregate projects are not supported");

         elsif Aggregate_Libraries_In (Project_Tree) then
            Fail ("aggregate library projects are not supported");
         end if;

         --  Check if a package with the name of the tool is in the project
         --  file and if there is one, get the switches, if any, and scan them.

         declare
            Pkg : constant Prj.Package_Id :=
                    Prj.Util.Value_Of
                      (Name        => Tool_Package_Name,
                       In_Packages => Project.Decl.Packages,
                       Shared      => Project_Tree.Shared);

            Element : Package_Element;

            Switches_Array : Array_Element_Id;

            The_Switches : Prj.Variable_Value;
            Current      : Prj.String_List_Id;
            The_String   : String_Element;

            Main : String_Access := null;

         begin
            if Pkg /= No_Package then
               Element := Project_Tree.Shared.Packages.Table (Pkg);

               --  Package Gnatls has a single attribute Switches, that is not
               --  an associative array.

               if The_Command = List then
                  The_Switches :=
                    Prj.Util.Value_Of
                    (Variable_Name => Snames.Name_Switches,
                     In_Variables  => Element.Decl.Attributes,
                     Shared        => Project_Tree.Shared);

               --  Packages Binder (for gnatbind), Cross_Reference (for
               --  gnatxref), Linker (for gnatlink), Finder (for gnatfind),
               --  have an attributed Switches, an associative array, indexed
               --  by the name of the file.

               --  They also have an attribute Default_Switches, indexed by the
               --  name of the programming language.

               else
                  --  First check if there is a single main

                  for J in 1 .. Last_Switches.Last loop
                     if Last_Switches.Table (J) (1) /= '-' then
                        if Main = null then
                           Main := Last_Switches.Table (J);
                        else
                           Main := null;
                           exit;
                        end if;
                     end if;
                  end loop;

                  if Main /= null then
                     Switches_Array :=
                       Prj.Util.Value_Of
                         (Name      => Name_Switches,
                          In_Arrays => Element.Decl.Arrays,
                          Shared    => Project_Tree.Shared);
                     Name_Len := 0;

                     --  If the single main has been specified as an absolute
                     --  path, use only the simple file name. If the absolute
                     --  path is incorrect, an error will be reported by the
                     --  underlying tool and it does not make a difference
                     --  what switches are used.

                     if Is_Absolute_Path (Main.all) then
                        Add_Str_To_Name_Buffer (File_Name (Main.all));
                     else
                        Add_Str_To_Name_Buffer (Main.all);
                     end if;

                     The_Switches := Prj.Util.Value_Of
                       (Index     => Name_Find,
                        Src_Index => 0,
                        In_Array  => Switches_Array,
                        Shared    => Project_Tree.Shared);
                  end if;

                  if The_Switches.Kind = Prj.Undefined then
                     Switches_Array :=
                       Prj.Util.Value_Of
                         (Name      => Name_Default_Switches,
                          In_Arrays => Element.Decl.Arrays,
                          Shared    => Project_Tree.Shared);
                     The_Switches := Prj.Util.Value_Of
                       (Index     => Name_Ada,
                        Src_Index => 0,
                        In_Array  => Switches_Array,
                        Shared    => Project_Tree.Shared);
                  end if;
               end if;

               --  If there are switches specified in the package of the
               --  project file corresponding to the tool, scan them.

               case The_Switches.Kind is
                  when Prj.Undefined =>
                     null;

                  when Prj.Single =>
                     declare
                        Switch : constant String :=
                                   Get_Name_String (The_Switches.Value);
                     begin
                        if Switch'Length > 0 then
                           First_Switches.Increment_Last;
                           First_Switches.Table (First_Switches.Last) :=
                             new String'(Switch);
                        end if;
                     end;

                  when Prj.List =>
                     Current := The_Switches.Values;
                     while Current /= Prj.Nil_String loop
                        The_String := Project_Tree.Shared.String_Elements.
                                        Table (Current);

                        declare
                           Switch : constant String :=
                                      Get_Name_String (The_String.Value);
                        begin
                           if Switch'Length > 0 then
                              First_Switches.Increment_Last;
                              First_Switches.Table (First_Switches.Last) :=
                                new String'(Switch);
                           end if;
                        end;

                        Current := The_String.Next;
                     end loop;
               end case;
            end if;
         end;

         if The_Command = Bind or else The_Command = Link then
            if Project.Object_Directory.Name = No_Path then
               Fail ("project " & Get_Name_String (Project.Display_Name)
                     & " has no object directory");
            end if;

            Change_Dir (Get_Name_String (Project.Object_Directory.Name));
         end if;

         --  Set up the env vars for project path files

         Prj.Env.Set_Ada_Paths
           (Project, Project_Tree, Including_Libraries => True);

         if The_Command = List then
            Check_Files;
         end if;
      end if;

      --  Gather all the arguments and invoke the executable

      declare
         The_Args : Argument_List
                      (1 .. First_Switches.Last + Last_Switches.Last);
         Arg_Num  : Natural := 0;

      begin
         for J in 1 .. First_Switches.Last loop
            Arg_Num := Arg_Num + 1;
            The_Args (Arg_Num) := First_Switches.Table (J);
         end loop;

         for J in 1 .. Last_Switches.Last loop
            Arg_Num := Arg_Num + 1;
            The_Args (Arg_Num) := Last_Switches.Table (J);
         end loop;

         if Verbose_Mode then
            Put (Exec_Path.all);

            for Arg in The_Args'Range loop
               Put (" " & The_Args (Arg).all);
            end loop;

            New_Line;
         end if;

         My_Exit_Status := Exit_Status (Spawn (Exec_Path.all, The_Args));
         Set_Exit_Status (My_Exit_Status);
      end;
   end;

exception
   when Error_Exit =>
      Set_Exit_Status (Failure);
end GNATCmd;
