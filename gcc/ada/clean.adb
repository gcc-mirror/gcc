------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                C L E A N                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2019, Free Software Foundation, Inc.         --
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

with ALI;       use ALI;
with Make_Util; use Make_Util;
with Namet;     use Namet;
with Opt;       use Opt;
with Osint;     use Osint;
with Osint.M;   use Osint.M;
with Switch;    use Switch;
with Table;
with Targparm;
with Types;     use Types;

with Ada.Command_Line;          use Ada.Command_Line;

with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.IO;                   use GNAT.IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

package body Clean is

   --  Suffixes of various files

   Assembly_Suffix : constant String := ".s";
   Tree_Suffix     : constant String := ".adt";
   Object_Suffix   : constant String := Get_Target_Object_Suffix.all;
   Debug_Suffix    : constant String := ".dg";
   Repinfo_Suffix  : constant String := ".rep";
   --  Suffix of representation info files

   B_Start : constant String := "b~";
   --  Prefix of binder generated file, and number of actual characters used

   Object_Directory_Path : String_Access := null;
   --  The path name of the object directory, set with switch -D

   Force_Deletions : Boolean := False;
   --  Set to True by switch -f. When True, attempts to delete non writable
   --  files will be done.

   Do_Nothing : Boolean := False;
   --  Set to True when switch -n is specified. When True, no file is deleted.
   --  gnatclean only lists the files that would have been deleted if the
   --  switch -n had not been specified.

   File_Deleted : Boolean := False;
   --  Set to True if at least one file has been deleted

   Copyright_Displayed : Boolean := False;
   Usage_Displayed     : Boolean := False;

   Project_File_Name : String_Access := null;

   package Sources is new Table.Table
     (Table_Component_Type => File_Name_Type,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Clean.Processed_Projects");
   --  Table to store all the source files of a library unit: spec, body and
   --  subunits, to detect .dg files and delete them.

   -----------------------------
   -- Other local subprograms --
   -----------------------------

   function Assembly_File_Name (Source : File_Name_Type) return String;
   --  Returns the assembly file name corresponding to Source

   procedure Clean_Executables;
   --  Do the cleaning work when no project file is specified

   function Debug_File_Name (Source : File_Name_Type) return String;
   --  Name of the expanded source file corresponding to Source

   procedure Delete (In_Directory : String; File : String);
   --  Delete one file, or list the file name if switch -n is specified

   procedure Delete_Binder_Generated_Files
     (Dir    : String;
      Source : File_Name_Type);
   --  Delete the binder generated file in directory Dir for Source, if they
   --  exist: for Unix these are b~<source>.ads, b~<source>.adb,
   --  b~<source>.ali and b~<source>.o.

   procedure Display_Copyright;
   --  Display the Copyright notice. If called several times, display the
   --  Copyright notice only the first time.

   procedure Initialize;
   --  Call the necessary package initializations

   function Object_File_Name (Source : File_Name_Type) return String;
   --  Returns the object file name corresponding to Source

   procedure Parse_Cmd_Line;
   --  Parse the command line

   function Repinfo_File_Name (Source : File_Name_Type) return String;
   --  Returns the repinfo file name corresponding to Source

   function Tree_File_Name (Source : File_Name_Type) return String;
   --  Returns the tree file name corresponding to Source

   procedure Usage;
   --  Display the usage. If called several times, the usage is displayed only
   --  the first time.

   ------------------------
   -- Assembly_File_Name --
   ------------------------

   function Assembly_File_Name (Source : File_Name_Type) return String is
      Src : constant String := Get_Name_String (Source);

   begin
      --  If the source name has an extension, then replace it with
      --  the assembly suffix.

      for Index in reverse Src'First + 1 .. Src'Last loop
         if Src (Index) = '.' then
            return Src (Src'First .. Index - 1) & Assembly_Suffix;
         end if;
      end loop;

      --  If there is no dot, or if it is the first character, just add the
      --  assembly suffix.

      return Src & Assembly_Suffix;
   end Assembly_File_Name;

   -----------------------
   -- Clean_Executables --
   -----------------------

   procedure Clean_Executables is
      Main_Source_File : File_Name_Type;
      --  Current main source

      Main_Lib_File : File_Name_Type;
      --  ALI file of the current main

      Lib_File : File_Name_Type;
      --  Current ALI file

      Full_Lib_File : File_Name_Type;
      --  Full name of the current ALI file

      Text    : Text_Buffer_Ptr;
      The_ALI : ALI_Id;
      Found   : Boolean;
      Source  : Queue.Source_Info;

   begin
      Queue.Initialize;

      --  It does not really matter if there is or not an object file
      --  corresponding to an ALI file: if there is one, it will be deleted.

      Opt.Check_Object_Consistency := False;

      --  Proceed each executable one by one. Each source is marked as it is
      --  processed, so common sources between executables will not be
      --  processed several times.

      for N_File in 1 .. Osint.Number_Of_Files loop
         Main_Source_File := Next_Main_Source;
         Main_Lib_File :=
           Osint.Lib_File_Name (Main_Source_File, Current_File_Index);

         if Main_Lib_File /= No_File then
            Queue.Insert
              ((File    => Main_Lib_File,
                Unit    => No_Unit_Name,
                Index   => 0));
         end if;

         while not Queue.Is_Empty loop
            Sources.Set_Last (0);
            Queue.Extract (Found, Source);
            pragma Assert (Found);
            pragma Assert (Source.File /= No_File);
            Lib_File := Source.File;
            Full_Lib_File := Osint.Full_Lib_File_Name (Lib_File);

            --  If we have existing ALI file that is not read-only, process it

            if Full_Lib_File /= No_File
              and then not Is_Readonly_Library (Full_Lib_File)
            then
               Text := Read_Library_Info (Lib_File);

               if Text /= null then
                  The_ALI :=
                    Scan_ALI (Lib_File, Text, Ignore_ED => False, Err => True);
                  Free (Text);

                  --  If no error was produced while loading this ALI file,
                  --  insert into the queue all the unmarked withed sources.

                  if The_ALI /= No_ALI_Id then
                     for J in ALIs.Table (The_ALI).First_Unit ..
                       ALIs.Table (The_ALI).Last_Unit
                     loop
                        Sources.Increment_Last;
                        Sources.Table (Sources.Last) :=
                          ALI.Units.Table (J).Sfile;

                        for K in ALI.Units.Table (J).First_With ..
                          ALI.Units.Table (J).Last_With
                        loop
                           if Withs.Table (K).Afile /= No_File then
                              Queue.Insert
                                ((File    => Withs.Table (K).Afile,
                                  Unit    => No_Unit_Name,
                                  Index   => 0));
                           end if;
                        end loop;
                     end loop;

                     --  Look for subunits and put them in the Sources table

                     for J in ALIs.Table (The_ALI).First_Sdep ..
                       ALIs.Table (The_ALI).Last_Sdep
                     loop
                        if Sdep.Table (J).Subunit_Name /= No_Name then
                           Sources.Increment_Last;
                           Sources.Table (Sources.Last) :=
                             Sdep.Table (J).Sfile;
                        end if;
                     end loop;
                  end if;
               end if;

               --  Now delete all existing files corresponding to this ALI file

               declare
                  Obj_Dir : constant String :=
                    Dir_Name (Get_Name_String (Full_Lib_File));
                  Obj     : constant String := Object_File_Name (Lib_File);
                  Adt     : constant String := Tree_File_Name   (Lib_File);
                  Asm     : constant String := Assembly_File_Name (Lib_File);

               begin
                  Delete (Obj_Dir, Get_Name_String (Lib_File));

                  if Is_Regular_File (Obj_Dir & Dir_Separator & Obj) then
                     Delete (Obj_Dir, Obj);
                  end if;

                  if Is_Regular_File (Obj_Dir & Dir_Separator & Adt) then
                     Delete (Obj_Dir, Adt);
                  end if;

                  if Is_Regular_File (Obj_Dir & Dir_Separator & Asm) then
                     Delete (Obj_Dir, Asm);
                  end if;

                  --  Delete expanded source files (.dg) and/or repinfo files
                  --  (.rep) if any

                  for J in 1 .. Sources.Last loop
                     declare
                        Deb : constant String :=
                          Debug_File_Name (Sources.Table (J));
                        Rep : constant String :=
                          Repinfo_File_Name (Sources.Table (J));

                     begin
                        if Is_Regular_File (Obj_Dir & Dir_Separator & Deb) then
                           Delete (Obj_Dir, Deb);
                        end if;

                        if Is_Regular_File (Obj_Dir & Dir_Separator & Rep) then
                           Delete (Obj_Dir, Rep);
                        end if;
                     end;
                  end loop;
               end;
            end if;
         end loop;

         --  Delete the executable, if it exists, and the binder generated
         --  files, if any.

         if not Compile_Only then
            declare
               Source     : constant File_Name_Type :=
                 Strip_Suffix (Main_Lib_File);
               Executable : constant String :=
                 Get_Name_String (Executable_Name (Source));
            begin
               if Is_Regular_File (Executable) then
                  Delete ("", Executable);
               end if;

               Delete_Binder_Generated_Files (Get_Current_Dir, Source);
            end;
         end if;
      end loop;
   end Clean_Executables;

   ---------------------
   -- Debug_File_Name --
   ---------------------

   function Debug_File_Name (Source : File_Name_Type) return String is
   begin
      return Get_Name_String (Source) & Debug_Suffix;
   end Debug_File_Name;

   ------------
   -- Delete --
   ------------

   procedure Delete (In_Directory : String; File : String) is
      Full_Name : String (1 .. In_Directory'Length + File'Length + 1);
      Last      : Natural := 0;
      Success   : Boolean;

   begin
      --  Indicate that at least one file is deleted or is to be deleted

      File_Deleted := True;

      --  Build the path name of the file to delete

      Last := In_Directory'Length;
      Full_Name (1 .. Last) := In_Directory;

      if Last > 0 and then Full_Name (Last) /= Directory_Separator then
         Last := Last + 1;
         Full_Name (Last) := Directory_Separator;
      end if;

      Full_Name (Last + 1 .. Last + File'Length) := File;
      Last := Last + File'Length;

      --  If switch -n was used, simply output the path name

      if Do_Nothing then
         Put_Line (Full_Name (1 .. Last));

      --  Otherwise, delete the file if it is writable

      else
         if Force_Deletions
           or else Is_Writable_File (Full_Name (1 .. Last))
           or else Is_Symbolic_Link (Full_Name (1 .. Last))
         then
            Delete_File (Full_Name (1 .. Last), Success);

         --  Here if no deletion required

         else
            Success := False;
         end if;

         if Verbose_Mode or else not Quiet_Output then
            if not Success then
               Put ("Warning: """);
               Put (Full_Name (1 .. Last));
               Put_Line (""" could not be deleted");

            else
               Put ("""");
               Put (Full_Name (1 .. Last));
               Put_Line (""" has been deleted");
            end if;
         end if;
      end if;
   end Delete;

   -----------------------------------
   -- Delete_Binder_Generated_Files --
   -----------------------------------

   procedure Delete_Binder_Generated_Files
     (Dir    : String;
      Source : File_Name_Type)
   is
      Source_Name : constant String   := Get_Name_String (Source);
      Current     : constant String   := Get_Current_Dir;
      Last        : constant Positive := B_Start'Length + Source_Name'Length;
      File_Name   : String (1 .. Last + 4);

   begin
      Change_Dir (Dir);

      --  Build the file name (before the extension)

      File_Name (1 .. B_Start'Length) := B_Start;
      File_Name (B_Start'Length + 1 .. Last) := Source_Name;

      --  Spec

      File_Name (Last + 1 .. Last + 4) := ".ads";

      if Is_Regular_File (File_Name (1 .. Last + 4)) then
         Delete (Dir, File_Name (1 .. Last + 4));
      end if;

      --  Body

      File_Name (Last + 1 .. Last + 4) := ".adb";

      if Is_Regular_File (File_Name (1 .. Last + 4)) then
         Delete (Dir, File_Name (1 .. Last + 4));
      end if;

      --  ALI file

      File_Name (Last + 1 .. Last + 4) := ".ali";

      if Is_Regular_File (File_Name (1 .. Last + 4)) then
         Delete (Dir, File_Name (1 .. Last + 4));
      end if;

      --  Object file

      File_Name (Last + 1 .. Last + Object_Suffix'Length) := Object_Suffix;

      if Is_Regular_File (File_Name (1 .. Last + Object_Suffix'Length)) then
         Delete (Dir, File_Name (1 .. Last + Object_Suffix'Length));
      end if;

      --  Change back to previous directory

      Change_Dir (Current);
   end Delete_Binder_Generated_Files;

   -----------------------
   -- Display_Copyright --
   -----------------------

   procedure Display_Copyright is
   begin
      if not Copyright_Displayed then
         Copyright_Displayed := True;
         Display_Version ("GNATCLEAN", "2003");
      end if;
   end Display_Copyright;

   ---------------
   -- Gnatclean --
   ---------------

   procedure Gnatclean is
   begin
      --  Do the necessary initializations

      Clean.Initialize;

      --  Parse the command line, getting the switches and the executable names

      Parse_Cmd_Line;

      if Verbose_Mode then
         Display_Copyright;
      end if;

      Osint.Add_Default_Search_Dirs;
      Targparm.Get_Target_Parameters;

      if Osint.Number_Of_Files = 0 then
         if Argument_Count = 0 then
            Usage;
         else
            Try_Help;
         end if;

         return;
      end if;

      if Verbose_Mode then
         New_Line;
      end if;

      if Project_File_Name /= null then
         declare
            Gprclean_Path : constant String_Access :=
              Locate_Exec_On_Path ("gprclean");
            Arg_Len : Natural       := Argument_Count;
            Pos     : Natural       := 0;
            Target  : String_Access := null;
            Success : Boolean       := False;
         begin
            if Gprclean_Path = null then
               Fail_Program
                 ("project files are no longer supported by gnatclean;" &
                    " use gprclean instead");
            end if;

            Find_Program_Name;

            if Name_Len > 10
              and then Name_Buffer (Name_Len - 8 .. Name_Len) = "gnatclean"
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

               Spawn (Gprclean_Path.all, Args, Success);

               if Success then
                  Exit_Program (E_Success);
               else
                  Exit_Program (E_Errors);
               end if;
            end;
         end;
      end if;

      Clean_Executables;

      --  In verbose mode, if Delete has not been called, indicate that no file
      --  needs to be deleted.

      if Verbose_Mode and (not File_Deleted) then
         New_Line;

         if Do_Nothing then
            Put_Line ("No file needs to be deleted");
         else
            Put_Line ("No file has been deleted");
         end if;
      end if;
   end Gnatclean;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Reset global variables

      Free (Object_Directory_Path);
      Do_Nothing := False;
      File_Deleted := False;
      Copyright_Displayed := False;
      Usage_Displayed := False;
   end Initialize;

   ----------------------
   -- Object_File_Name --
   ----------------------

   function Object_File_Name (Source : File_Name_Type) return String is
      Src : constant String := Get_Name_String (Source);

   begin
      --  If the source name has an extension, then replace it with
      --  the Object suffix.

      for Index in reverse Src'First + 1 .. Src'Last loop
         if Src (Index) = '.' then
            return Src (Src'First .. Index - 1) & Object_Suffix;
         end if;
      end loop;

      --  If there is no dot, or if it is the first character, just add the
      --  ALI suffix.

      return Src & Object_Suffix;
   end Object_File_Name;

   --------------------
   -- Parse_Cmd_Line --
   --------------------

   procedure Parse_Cmd_Line is
      Last         : constant Natural := Argument_Count;
      Index        : Positive;
      Source_Index : Int := 0;

      procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

   begin
      --  First, check for --version and --help

      Check_Version_And_Help ("GNATCLEAN", "2003");

      --  First, check for switch -P and, if found and gprclean is available,
      --  silently invoke gprclean, with switch --target if not on a native
      --  platform.

      declare
         Arg_Len       : Positive      := Argument_Count;
         Call_Gprclean : Boolean       := False;
         Gprclean      : String_Access := null;
         Pos           : Natural       := 0;
         Success       : Boolean;
         Target        : String_Access := null;

      begin
         Find_Program_Name;

         if Name_Len >= 9
           and then Name_Buffer (Name_Len - 8 .. Name_Len) = "gnatclean"
         then
            if Name_Len > 9 then
               Target  := new String'(Name_Buffer (1 .. Name_Len - 10));
               Arg_Len := Arg_Len + 1;
            end if;

            for J in 1 .. Argument_Count loop
               declare
                  Arg : constant String := Argument (J);
               begin
                  if Arg'Length >= 2
                    and then Arg (Arg'First .. Arg'First + 1) = "-P"
                  then
                     Call_Gprclean := True;
                     exit;
                  end if;
               end;
            end loop;

            if Call_Gprclean then
               Gprclean := Locate_Exec_On_Path (Exec_Name => "gprclean");

               if Gprclean /= null then
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

                     Spawn (Gprclean.all, Args, Success);

                     Free (Gprclean);

                     if Success then
                        Exit_Program (E_Success);

                     else
                        Exit_Program (E_Fatal);
                     end if;
                  end;
               end if;
            end if;
         end if;
      end;

      Index := 1;
      while Index <= Last loop
         declare
            Arg : constant String := Argument (Index);

            procedure Bad_Argument;
            pragma No_Return (Bad_Argument);
            --  Signal bad argument

            ------------------
            -- Bad_Argument --
            ------------------

            procedure Bad_Argument is
            begin
               Fail ("invalid argument """ & Arg & """");
            end Bad_Argument;

         begin
            if Arg'Length /= 0 then
               if Arg (1) = '-' then
                  if Arg'Length = 1 then
                     Bad_Argument;
                  end if;

                  case Arg (2) is
                     when '-' =>
                        if Arg'Length > Subdirs_Option'Length
                          and then
                            Arg (1 .. Subdirs_Option'Length) = Subdirs_Option
                        then
                           null;
                           --  Subdirs are only used in gprclean

                        elsif Arg = Make_Util.Unchecked_Shared_Lib_Imports then
                           Opt.Unchecked_Shared_Lib_Imports := True;

                        else
                           Bad_Argument;
                        end if;

                     when 'a' =>
                        if Arg'Length < 4 then
                           Bad_Argument;
                        end if;

                        if Arg (3) = 'O' then
                           Add_Lib_Search_Dir (Arg (4 .. Arg'Last));

                        elsif Arg (3) = 'P' then
                           null;
                           --  This is only for gprclean

                        else
                           Bad_Argument;
                        end if;

                     when 'c' =>
                        Compile_Only := True;

                     when 'D' =>
                        if Object_Directory_Path /= null then
                           Fail ("duplicate -D switch");

                        elsif Project_File_Name /= null then
                           Fail ("-P and -D cannot be used simultaneously");
                        end if;

                        if Arg'Length > 2 then
                           declare
                              Dir : constant String := Arg (3 .. Arg'Last);
                           begin
                              if not Is_Directory (Dir) then
                                 Fail (Dir & " is not a directory");
                              else
                                 Add_Lib_Search_Dir (Dir);
                              end if;
                           end;

                        else
                           if Index = Last then
                              Fail ("no directory specified after -D");
                           end if;

                           Index := Index + 1;

                           declare
                              Dir : constant String := Argument (Index);
                           begin
                              if not Is_Directory (Dir) then
                                 Fail (Dir & " is not a directory");
                              else
                                 Add_Lib_Search_Dir (Dir);
                              end if;
                           end;
                        end if;

                     when 'e' =>
                        if Arg = "-eL" then
                           Follow_Links_For_Files := True;
                           Follow_Links_For_Dirs  := True;

                        else
                           Bad_Argument;
                        end if;

                     when 'f' =>
                        Force_Deletions := True;
                        Directories_Must_Exist_In_Projects := False;

                     when 'F' =>
                        Full_Path_Name_For_Brief_Errors := True;

                     when 'h' =>
                        Usage;

                     when 'i' =>
                        if Arg'Length = 2 then
                           Bad_Argument;
                        end if;

                        Source_Index := 0;

                        for J in 3 .. Arg'Last loop
                           if Arg (J) not in '0' .. '9' then
                              Bad_Argument;
                           end if;

                           Source_Index :=
                             (20 * Source_Index) +
                             (Character'Pos (Arg (J)) - Character'Pos ('0'));
                        end loop;

                     when 'I' =>
                        if Arg = "-I-" then
                           Opt.Look_In_Primary_Dir := False;

                        else
                           if Arg'Length = 2 then
                              Bad_Argument;
                           end if;

                           Add_Lib_Search_Dir (Arg (3 .. Arg'Last));
                        end if;

                     when 'n' =>
                        Do_Nothing := True;

                     when 'P' =>
                        if Project_File_Name /= null then
                           Fail ("multiple -P switches");

                        elsif Object_Directory_Path /= null then
                           Fail ("-D and -P cannot be used simultaneously");

                        end if;

                        if Arg'Length > 2 then
                           declare
                              Prj : constant String := Arg (3 .. Arg'Last);
                           begin
                              if Prj'Length > 1
                                 and then Prj (Prj'First) = '='
                              then
                                 Project_File_Name :=
                                   new String'
                                     (Prj (Prj'First + 1 ..  Prj'Last));
                              else
                                 Project_File_Name := new String'(Prj);
                              end if;
                           end;

                        else
                           if Index = Last then
                              Fail ("no project specified after -P");
                           end if;

                           Index := Index + 1;
                           Project_File_Name := new String'(Argument (Index));
                        end if;

                     when 'q' =>
                        Quiet_Output := True;

                     when 'r' =>
                        null;
                        --  This is only for gprclean

                     when 'v' =>
                        if Arg = "-v" then
                           Verbose_Mode := True;

                        elsif Arg = "-vP0"
                          or else Arg = "-vP1"
                          or else Arg = "-vP2"
                        then
                           null;
                           --  This is only for gprclean

                        else
                           Bad_Argument;
                        end if;

                     when 'X' =>
                        if Arg'Length = 2 then
                           Bad_Argument;
                        end if;

                     when others =>
                        Bad_Argument;
                  end case;

               else
                  Add_File (Arg, Source_Index);
               end if;
            end if;
         end;

         Index := Index + 1;
      end loop;
   end Parse_Cmd_Line;

   -----------------------
   -- Repinfo_File_Name --
   -----------------------

   function Repinfo_File_Name (Source : File_Name_Type) return String is
   begin
      return Get_Name_String (Source) & Repinfo_Suffix;
   end Repinfo_File_Name;

   --------------------
   -- Tree_File_Name --
   --------------------

   function Tree_File_Name (Source : File_Name_Type) return String is
      Src : constant String := Get_Name_String (Source);

   begin
      --  If source name has an extension, then replace it with the tree suffix

      for Index in reverse Src'First + 1 .. Src'Last loop
         if Src (Index) = '.' then
            return Src (Src'First .. Index - 1) & Tree_Suffix;
         end if;
      end loop;

      --  If there is no dot, or if it is the first character, just add the
      --  tree suffix.

      return Src & Tree_Suffix;
   end Tree_File_Name;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      if not Usage_Displayed then
         Usage_Displayed := True;
         Display_Copyright;
         Put_Line ("Usage: gnatclean [switches] {[-innn] name}");
         New_Line;

         Display_Usage_Version_And_Help;

         Put_Line ("  names is one or more file names from which " &
                   "the .adb or .ads suffix may be omitted");
         Put_Line ("  names may be omitted if -P<project> is specified");
         New_Line;

         Put_Line ("  --subdirs=dir real obj/lib/exec dirs are subdirs");
         Put_Line ("  " & Make_Util.Unchecked_Shared_Lib_Imports);
         Put_Line ("       Allow shared libraries to import static libraries");
         New_Line;

         Put_Line ("  -c       Only delete compiler generated files");
         Put_Line ("  -D dir   Specify dir as the object library");
         Put_Line ("  -eL      Follow symbolic links when processing " &
                   "project files");
         Put_Line ("  -f       Force deletions of unwritable files");
         Put_Line ("  -F       Full project path name " &
                   "in brief error messages");
         Put_Line ("  -h       Display this message");
         Put_Line ("  -innn    Index of unit in source for following names");
         Put_Line ("  -n       Nothing to do: only list files to delete");
         Put_Line ("  -Pproj   Use GNAT Project File proj");
         Put_Line ("  -q       Be quiet/terse");
         Put_Line ("  -r       Clean all projects recursively");
         Put_Line ("  -v       Verbose mode");
         Put_Line ("  -vPx     Specify verbosity when parsing " &
                   "GNAT Project Files");
         Put_Line ("  -Xnm=val Specify an external reference " &
                   "for GNAT Project Files");
         New_Line;

         Put_Line ("  -aPdir   Add directory dir to project search path");
         New_Line;

         Put_Line ("  -aOdir   Specify ALI/object files search path");
         Put_Line ("  -Idir    Like -aOdir");
         Put_Line ("  -I-      Don't look for source/library files " &
                   "in the default directory");
         New_Line;
      end if;
   end Usage;

end Clean;
