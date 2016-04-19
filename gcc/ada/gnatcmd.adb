------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T C M D                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2015, Free Software Foundation, Inc.         --
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

with Gnatvsn;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Switch;   use Switch;
with Table;
with Targparm; use Targparm;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

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

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Output_Version;
   --  Output the version of this program

   procedure Usage;
   --  Display usage

   --------------------
   -- Output_Version --
   --------------------

   procedure Output_Version is
   begin
      if AAMP_On_Target then
         Put ("GNAAMP ");
      else
         Put ("GNAT ");
      end if;

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
         if Targparm.AAMP_On_Target then
            Put ("gnaampcmd ");
         else
            Put ("gnat ");
         end if;

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
   --  Initializations

   Last_Switches.Init;
   Last_Switches.Set_Last (0);

   First_Switches.Init;
   First_Switches.Set_Last (0);

   --  Set AAMP_On_Target from command name, for testing in Osint.Program_Name
   --  to handle the mapping of GNAAMP tool names. We don't extract it from
   --  system.ads, as there may be no default runtime.

   Find_Program_Name;
   AAMP_On_Target := Name_Buffer (1 .. Name_Len) = "gnaampcmd";

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
               Project_File_Used : Boolean := False;
               Switch            : String_Access;

            begin
               for J in 1 .. Last_Switches.Last loop
                  Switch := Last_Switches.Table (J);
                  if Switch'Length >= 2
                    and then Switch (Switch'First .. Switch'First + 1) = "-P"
                  then
                     Project_File_Used := True;
                     exit;
                  end if;
               end loop;

               if Project_File_Used then
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
