------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                C L E A N                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2004, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with ALI;      use ALI;
with Csets;
with Gnatvsn;
with Hostparm;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.M;  use Osint.M;
with Prj;      use Prj;
with Prj.Com;
with Prj.Env;
with Prj.Ext;
with Prj.Pars;
with Prj.Util; use Prj.Util;
with Snames;
with System;
with Table;
with Types;    use Types;

with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.IO;                   use GNAT.IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;


package body Clean is

   Initialized : Boolean := False;
   --  Set to True by the first call to Initialize.
   --  To avoid reinitialization of some packages.

   --  Suffixes of various files

   Assembly_Suffix : constant String := ".s";
   ALI_Suffix      : constant String := ".ali";
   Tree_Suffix     : constant String := ".adt";
   Object_Suffix   : constant String := Get_Object_Suffix.all;
   Debug_Suffix    : String          := ".dg";
   --  Changed to "_dg" for VMS in the body of the package

   Repinfo_Suffix  : String          := ".rep";
   --  Changed to "_rep" for VMS in the body of the package

   B_Start : String := "b~";
   --  Prefix of binder generated file.
   --  Changed to "b$" for VMS in the body of the package.

   Object_Directory_Path : String_Access := null;
   --  The path name of the object directory, set with switch -D

   Do_Nothing : Boolean := False;
   --  Set to True when switch -n is specified.
   --  When True, no file is deleted. gnatclean only lists the files that
   --  would have been deleted if the switch -n had not been specified.

   File_Deleted : Boolean := False;
   --  Set to True if at least one file has been deleted

   Copyright_Displayed : Boolean := False;
   Usage_Displayed     : Boolean := False;

   Project_File_Name : String_Access := null;

   Main_Project : Prj.Project_Id := Prj.No_Project;

   All_Projects : Boolean := False;

   --  Packages of project files where unknown attributes are errors.

   Naming_String   : aliased String := "naming";
   Builder_String  : aliased String := "builder";
   Compiler_String : aliased String := "compiler";
   Binder_String   : aliased String := "binder";
   Linker_String   : aliased String := "linker";

   Gnatmake_Packages : aliased String_List :=
     (Naming_String   'Access,
      Builder_String  'Access,
      Compiler_String 'Access,
      Binder_String   'Access,
      Linker_String   'Access);

   Packages_To_Check_By_Gnatmake : constant String_List_Access :=
     Gnatmake_Packages'Access;

   package Processed_Projects is new Table.Table
     (Table_Component_Type => Project_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Clean.Processed_Projects");
   --  Table to keep track of what project files have been processed, when
   --  switch -r is specified.

   package Sources is new Table.Table
     (Table_Component_Type => File_Name_Type,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Clean.Processed_Projects");
   --  Table to store all the source files of a library unit: spec, body and
   --  subunits, to detect .dg files and delete them.

   ----------------------------
   -- Queue (Q) manipulation --
   ----------------------------

   procedure Init_Q;
   --  Must be called to initialize the Q

   procedure Insert_Q
     (Source_File : File_Name_Type);
   --  If Source_File is not marked, inserts it at the end of Q and mark it

   function Empty_Q return Boolean;
   --  Returns True if Q is empty.

   procedure Extract_From_Q
     (Source_File : out File_Name_Type);
   --  Extracts the first element from the Q.

   Q_Front : Natural;
   --  Points to the first valid element in the Q.

   package Q is new Table.Table (
     Table_Component_Type => File_Name_Type,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 0,
     Table_Initial        => 4000,
     Table_Increment      => 100,
     Table_Name           => "Clean.Q");
   --  This is the actual queue

   -----------------------------
   -- Other local subprograms --
   -----------------------------

   procedure Add_Source_Dir (N : String);
   --  Call Add_Src_Search_Dir.
   --  Output one line when in verbose mode.

   procedure Add_Source_Directories is
     new Prj.Env.For_All_Source_Dirs (Action => Add_Source_Dir);

   procedure Add_Object_Dir (N : String);
   --  Call Add_Lib_Search_Dir.
   --  Output one line when in verbose mode.

   procedure Add_Object_Directories is
     new Prj.Env.For_All_Object_Dirs (Action => Add_Object_Dir);

   function ALI_File_Name (Source : Name_Id) return String;
   --  Returns the name of the ALI file corresponding to Source

   function Assembly_File_Name (Source : Name_Id) return String;
   --  Returns the assembly file name corresponding to Source

   procedure Clean_Directory (Dir : Name_Id);
   --  Delete all regular files in a library directory or in a library
   --  interface dir.

   procedure Clean_Executables;
   --  Do the cleaning work when no project file is specified

   procedure Clean_Project (Project : Project_Id);
   --  Do the cleaning work when a project file is specified.
   --  This procedure calls itself recursively when there are several
   --  project files in the tree rooted at the main project file and switch -r
   --  has been specified.

   function Debug_File_Name (Source : Name_Id) return String;
   --  Name of the expanded source file corresponding to Source

   procedure Delete (In_Directory : String; File : String);
   --  Delete one file, or list the file name if switch -n is specified

   procedure Delete_Binder_Generated_Files (Dir : String; Source : Name_Id);
   --  Delete the binder generated file in directory Dir for Source, if they
   --  exist: for Unix these are b~<source>.ads, b~<source>.adb,
   --  b~<source>.ali and b~<source>.o.

   procedure Display_Copyright;
   --  Display the Copyright notice.
   --  If called several times, display the Copyright notice only the first
   --  time.

   procedure Initialize;
   --  Call the necessary package initializations

   function Object_File_Name (Source : Name_Id) return String;
   --  Returns the object file name corresponding to Source

   procedure Parse_Cmd_Line;
   --  Parse the command line

   function Repinfo_File_Name (Source : Name_Id) return String;
   --  Returns the repinfo file name corresponding to Source

   function Tree_File_Name (Source : Name_Id) return String;
   --  Returns the tree file name corresponding to Source

   function In_Extension_Chain
     (Of_Project : Project_Id;
      Prj        : Project_Id) return Boolean;
   --  Returns True iff Prj is an extension of Of_Project or if Of_Project is
   --  an extension of Prj.

   procedure Usage;
   --  Display the usage.
   --  If called several times, the usage is displayed only the first time.

   --------------------
   -- Add_Object_Dir --
   --------------------

   procedure Add_Object_Dir (N : String) is
   begin
      Add_Lib_Search_Dir (N);

      if Opt.Verbose_Mode then
         Put ("Adding object directory """);
         Put (N);
         Put (""".");
         New_Line;
      end if;
   end Add_Object_Dir;

   --------------------
   -- Add_Source_Dir --
   --------------------

   procedure Add_Source_Dir (N : String) is
   begin
      Add_Src_Search_Dir (N);

      if Opt.Verbose_Mode then
         Put ("Adding source directory """);
         Put (N);
         Put (""".");
         New_Line;
      end if;
   end Add_Source_Dir;

   -------------------
   -- ALI_File_Name --
   -------------------

   function ALI_File_Name (Source : Name_Id) return String is
      Src : constant String := Get_Name_String (Source);

   begin
      --  If the source name has an extension, then replace it with
      --  the ALI suffix.

      for Index in reverse Src'First + 1 .. Src'Last loop
         if Src (Index) = '.' then
            return Src (Src'First .. Index - 1) & ALI_Suffix;
         end if;
      end loop;

      --  If there is no dot, or if it is the first character, just add the
      --  ALI suffix.

      return Src & ALI_Suffix;
   end ALI_File_Name;

   ------------------------
   -- Assembly_File_Name --
   ------------------------

   function Assembly_File_Name (Source : Name_Id) return String is
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

   ---------------------
   -- Clean_Directory --
   ---------------------

   procedure Clean_Directory (Dir : Name_Id) is
      Directory : constant String := Get_Name_String (Dir);
      Current   : constant Dir_Name_Str := Get_Current_Dir;

      Direc : Dir_Type;

      Name : String (1 .. 200);
      Last : Natural;

      procedure Set_Writable (Name : System.Address);
      pragma Import (C, Set_Writable, "__gnat_set_writable");

   begin
      Change_Dir (Directory);
      Open (Direc, ".");

      --  For each regular file in the directory, if switch -n has not been
      --  specified, make it writable and delete the file.

      loop
         Read (Direc, Name, Last);
         exit when Last = 0;

         if Is_Regular_File (Name (1 .. Last)) then
            if not Do_Nothing then
               Name (Last + 1) := ASCII.NUL;
               Set_Writable (Name (1)'Address);
            end if;

            Delete (Directory, Name (1 .. Last));
         end if;
      end loop;

      Close (Direc);

      --  Restore the initial working directory

      Change_Dir (Current);
   end Clean_Directory;

   -----------------------
   -- Clean_Executables --
   -----------------------

   procedure Clean_Executables is
      Main_Source_File : File_Name_Type;
      --  Current main source

      Source_File : File_Name_Type;
      --  Current source file

      Lib_File : File_Name_Type;
      --  Current library file

      Full_Lib_File : File_Name_Type;
      --  Full name of the current library file

      Text : Text_Buffer_Ptr;
      The_ALI : ALI_Id;

   begin
      Init_Q;

      --  It does not really matter if there is or not an object file
      --  corresponding to an ALI file: if there is one, it will be deleted.

      Opt.Check_Object_Consistency := False;

      --  Proceed each executable one by one. Each source is marked as it is
      --  processed, so common sources between executables will not be
      --  processed several times.

      for N_File in 1 .. Osint.Number_Of_Files loop
         Main_Source_File := Next_Main_Source;
         Insert_Q (Main_Source_File);

         while not Empty_Q loop
            Sources.Set_Last (0);
            Extract_From_Q (Source_File);
            Lib_File      := Osint.Lib_File_Name (Source_File);
            Full_Lib_File := Osint.Full_Lib_File_Name (Lib_File);

            --  If we have an existing ALI file that is not read-only,
            --  process it.

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
                           Insert_Q (Withs.Table (K).Sfile);
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

               --  Now, delete all the existing files corresponding to this
               --  ALI file.

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
               Source : constant Name_Id := Strip_Suffix (Main_Source_File);
               Executable : constant String := Get_Name_String
                                              (Executable_Name (Source));
            begin
               if Is_Regular_File (Executable) then
                  Delete ("", Executable);
               end if;

               Delete_Binder_Generated_Files (Get_Current_Dir, Source);
            end;
         end if;
      end loop;
   end Clean_Executables;

   -------------------
   -- Clean_Project --
   -------------------

   procedure Clean_Project (Project : Project_Id) is
      Main_Source_File : File_Name_Type;
      --  Name of the executable on the command line, without directory
      --  information.

      Executable : Name_Id;
      --  Name of the executable file

      Current_Dir : constant Dir_Name_Str := Get_Current_Dir;
      Data        : constant Project_Data := Projects.Table (Project);
      U_Data      : Prj.Com.Unit_Data;
      File_Name1  : Name_Id;
      File_Name2  : Name_Id;

      use Prj.Com;

   begin
      --  Check that we don't specify executable on the command line for
      --  a main library project.

      if Project = Main_Project
        and then Osint.Number_Of_Files /= 0
        and then Data.Library
      then
         Osint.Fail
           ("Cannot specify executable(s) for a Library Project File");
      end if;

      if Verbose_Mode then
         Put ("Cleaning project """);
         Put (Get_Name_String (Data.Name));
         Put_Line ("""");
      end if;

      --  Add project to the list of proceesed projects

      Processed_Projects.Increment_Last;
      Processed_Projects.Table (Processed_Projects.Last) := Project;

      if Data.Object_Directory /= No_Name then
         declare
            Obj_Dir : constant String :=
                        Get_Name_String (Data.Object_Directory);

         begin
            Change_Dir (Obj_Dir);

            --  Look through the units to find those that are either immediate
            --  sources or inherited sources of the project.

            for Unit in 1 .. Prj.Com.Units.Last loop
               U_Data := Prj.Com.Units.Table (Unit);
               File_Name1 := No_Name;
               File_Name2 := No_Name;

               --  If either the spec or the body is a source of the project,
               --  check for the corresponding ALI file in the object
               --  directory.

               if In_Extension_Chain
                 (U_Data.File_Names (Body_Part).Project, Project)
                 or else
                   In_Extension_Chain
                     (U_Data.File_Names (Specification).Project, Project)
               then
                  File_Name1 := U_Data.File_Names (Body_Part).Name;
                  File_Name2 := U_Data.File_Names (Specification).Name;

                  --  If there is no body file name, then there may be only a
                  --  spec.

                  if File_Name1 = No_Name then
                     File_Name1 := File_Name2;
                     File_Name2 := No_Name;
                  end if;
               end if;

               --  If there is either a spec or a body, look for files in the
               --  object directory.

               if File_Name1 /= No_Name then
                  declare
                     Asm : constant String := Assembly_File_Name (File_Name1);
                     ALI : constant String := ALI_File_Name      (File_Name1);
                     Obj : constant String := Object_File_Name   (File_Name1);
                     Adt : constant String := Tree_File_Name     (File_Name1);
                     Deb : constant String := Debug_File_Name    (File_Name1);
                     Rep : constant String := Repinfo_File_Name  (File_Name1);
                     Del : Boolean := True;

                  begin
                     --  If the ALI file exists and is read-only, no file is
                     --  deleted.

                     if Is_Regular_File (ALI) then
                        if Is_Writable_File (ALI) then
                           Delete (Obj_Dir, ALI);

                        else
                           Del := False;

                           if Verbose_Mode then
                              Put ('"');
                              Put (Obj_Dir);

                              if Obj_Dir (Obj_Dir'Last) /= Dir_Separator then
                                 Put (Dir_Separator);
                              end if;

                              Put (ALI);
                              Put_Line (""" is read-only");
                           end if;
                        end if;
                     end if;

                     if Del then

                        --  Object file

                        if Is_Regular_File (Obj) then
                           Delete (Obj_Dir, Obj);
                        end if;

                        --  Assembly file

                        if Is_Regular_File (Asm) then
                           Delete (Obj_Dir, Asm);
                        end if;

                        --  Tree file

                        if Is_Regular_File (Adt) then
                           Delete (Obj_Dir, Adt);
                        end if;

                        --  First expanded source file

                        if Is_Regular_File (Deb) then
                           Delete (Obj_Dir, Deb);
                        end if;

                        --  Repinfo file

                        if Is_Regular_File (Rep) then
                           Delete (Obj_Dir, Rep);
                        end if;

                        --  Second expanded source file

                        if File_Name2 /= No_Name then
                           declare
                              Deb : constant String :=
                                      Debug_File_Name   (File_Name2);
                              Rep : constant String :=
                                      Repinfo_File_Name (File_Name2);
                           begin
                              if Is_Regular_File (Deb) then
                                 Delete (Obj_Dir, Deb);
                              end if;

                              if Is_Regular_File (Rep) then
                                 Delete (Obj_Dir, Rep);
                              end if;
                           end;
                        end if;
                     end if;
                  end;
               end if;
            end loop;

            if Verbose_Mode then
               New_Line;
            end if;
         end;
      end if;

      --  If switch -r is specified, call Clean_Project recursively for the
      --  imported projects and the project being extended.

      if All_Projects then
         declare
            Imported : Project_List := Data.Imported_Projects;
            Element  : Project_Element;
            Process  : Boolean;

         begin
            --  For each imported project, call Clean_Project if the project
            --  has not been processed already.

            while Imported /= Empty_Project_List loop
               Element := Project_Lists.Table (Imported);
               Imported := Element.Next;
               Process := True;

               for
                 J in Processed_Projects.First .. Processed_Projects.Last
               loop
                  if Element.Project = Processed_Projects.Table (J) then
                     Process := False;
                     exit;
                  end if;
               end loop;

               if Process then
                  Clean_Project (Element.Project);
               end if;
            end loop;

            --  If this project extends another project, call Clean_Project for
            --  the project being extended. It is guaranteed that it has not
            --  called before, because no other project may import or extend
            --  this project.

            if Data.Extends /= No_Project then
               Clean_Project (Data.Extends);
            end if;
         end;
      end if;

      --  If this is a library project, clean the library directory, the
      --  interface copy dir and, for a Stand-Alone Library, the binder
      --  generated files of the library.

      --  The directories are cleaned only if switch -c is not specified.

      if Data.Library then
         if not Compile_Only then
            Clean_Directory (Data.Library_Dir);

            if Data.Library_Src_Dir /= No_Name
              and then Data.Library_Src_Dir /= Data.Library_Dir
            then
               Clean_Directory (Data.Library_Src_Dir);
            end if;
         end if;

         if Data.Standalone_Library and then
            Data.Object_Directory /= No_Name
         then
            Delete_Binder_Generated_Files
              (Get_Name_String (Data.Object_Directory), Data.Library_Name);
         end if;

         --  Otherwise, for the main project, delete the executables and the
         --  binder generated files.

         --  The executables are deleted only if switch -c is not specified.

      elsif Project = Main_Project and then Data.Exec_Directory /= No_Name then
         declare
            Exec_Dir : constant String :=
              Get_Name_String (Data.Exec_Directory);
         begin
            Change_Dir (Exec_Dir);

            for N_File in 1 .. Osint.Number_Of_Files loop
               Main_Source_File := Next_Main_Source;

               if not Compile_Only then
                  Executable := Executable_Of (Main_Project, Main_Source_File);

                  if Is_Regular_File (Get_Name_String (Executable)) then
                     Delete (Exec_Dir, Get_Name_String (Executable));
                  end if;
               end if;

               if Data.Object_Directory /= No_Name then
                  Delete_Binder_Generated_Files
                    (Get_Name_String
                       (Data.Object_Directory),
                     Strip_Suffix (Main_Source_File));
               end if;
            end loop;
         end;
      end if;

      --  Change back to previous directory

      Change_Dir (Current_Dir);
   end Clean_Project;

   ---------------------
   -- Debug_File_Name --
   ---------------------

   function Debug_File_Name (Source : Name_Id) return String is
   begin
      return Get_Name_String (Source) & Debug_Suffix;
   end Debug_File_Name;

   ------------
   -- Delete --
   ------------

   procedure Delete (In_Directory : String; File : String) is
      Full_Name : String (1 .. In_Directory'Length + File'Length + 1);
      Last : Natural := 0;
      Success : Boolean;

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

      --  Otherwise, delete the file

      else
         Delete_File (Full_Name (1 .. Last), Success);

         if not Success then
            Put ("Warning: """);
            Put (Full_Name (1 .. Last));
            Put_Line (""" could not be deleted");

         elsif Verbose_Mode or else not Quiet_Output then
            Put ("""");
            Put (Full_Name (1 .. Last));
            Put_Line (""" has been deleted");
         end if;
      end if;
   end Delete;

   -----------------------------------
   -- Delete_Binder_Generated_Files --
   -----------------------------------

   procedure Delete_Binder_Generated_Files (Dir : String; Source : Name_Id) is
      Source_Name : constant String := Get_Name_String (Source);
      Current     : constant String := Get_Current_Dir;
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
         Put_Line ("GNATCLEAN " & Gnatvsn.Gnat_Version_String
                   & " Copyright 2003-2004 Free Software Foundation, Inc.");
      end if;
   end Display_Copyright;

   -------------
   -- Empty_Q --
   -------------

   function Empty_Q return Boolean is
   begin
      return Q_Front >= Q.Last;
   end Empty_Q;

   --------------------
   -- Extract_From_Q --
   --------------------

   procedure Extract_From_Q (Source_File : out File_Name_Type) is
      File : constant File_Name_Type := Q.Table (Q_Front);

   begin
      Q_Front := Q_Front + 1;
      Source_File := File;
   end Extract_From_Q;

   ---------------
   -- Gnatclean --
   ---------------

   procedure Gnatclean is
   begin
      --  Do the necessary initializations

      Initialize;

      --  Parse the command line, getting the switches and the executable names

      Parse_Cmd_Line;

      if Verbose_Mode then
         Display_Copyright;
      end if;

      if Project_File_Name /= null then

         --  A project file was specified by a -P switch

         if Opt.Verbose_Mode then
            New_Line;
            Put ("Parsing Project File """);
            Put (Project_File_Name.all);
            Put_Line (""".");
            New_Line;
         end if;

         --  Set the project parsing verbosity to whatever was specified
         --  by a possible -vP switch.

         Prj.Pars.Set_Verbosity (To => Prj.Com.Current_Verbosity);

         --  Parse the project file.
         --  If there is an error, Main_Project will still be No_Project.

         Prj.Pars.Parse
           (Project           => Main_Project,
            Project_File_Name => Project_File_Name.all,
            Packages_To_Check => Packages_To_Check_By_Gnatmake);

         if Main_Project = No_Project then
            Fail ("""" & Project_File_Name.all &
                  """ processing failed");
         end if;

         if Opt.Verbose_Mode then
            New_Line;
            Put ("Parsing of Project File """);
            Put (Project_File_Name.all);
            Put (""" is finished.");
            New_Line;
         end if;

         --  We add the source directories and the object directories
         --  to the search paths.

         Add_Source_Directories (Main_Project);
         Add_Object_Directories (Main_Project);

      end if;

      Osint.Add_Default_Search_Dirs;

      --  If a project file was specified, but no executable name, put all
      --  the mains of the project file (if any) as if there were on the
      --  command line.

      if Main_Project /= No_Project and then Osint.Number_Of_Files = 0 then
         declare
            Value : String_List_Id := Projects.Table (Main_Project).Mains;

         begin
            while Value /= Prj.Nil_String loop
               Get_Name_String (String_Elements.Table (Value).Value);
               Osint.Add_File (Name_Buffer (1 .. Name_Len));
               Value := String_Elements.Table (Value).Next;
            end loop;
         end;
      end if;

      --  If neither a project file nor an executable were specified,
      --  output the usage and exit.

      if Main_Project = No_Project and then Osint.Number_Of_Files = 0 then
         Usage;
         return;
      end if;

      if Verbose_Mode then
         New_Line;
      end if;

      if Main_Project /= No_Project then

         --  If a project file has been specified, call Clean_Project with the
         --  project id of this project file, after resetting the list of
         --  processed projects.

         Processed_Projects.Init;
         Clean_Project (Main_Project);

      else
         --  If no project file has been specified, the work is done in
         --  Clean_Executables.

         Clean_Executables;
      end if;

      --  In verbose mode, if Delete has not been called, indicate that
      --  no file needs to be deleted.

      if Verbose_Mode and (not File_Deleted) then
         New_Line;

         if Do_Nothing then
            Put_Line ("No file needs to be deleted");
         else
            Put_Line ("No file has been deleted");
         end if;
      end if;
   end Gnatclean;

   ------------------------
   -- In_Extension_Chain --
   ------------------------

   function In_Extension_Chain
     (Of_Project : Project_Id;
      Prj        : Project_Id) return Boolean
   is
      Data : Project_Data;

   begin
      if Of_Project = Prj then
         return True;
      end if;

      Data := Projects.Table (Of_Project);

      while Data.Extends /= No_Project loop
         if Data.Extends = Prj then
            return True;
         end if;

         Data := Projects.Table (Data.Extends);
      end loop;

      Data := Projects.Table (Prj);

      while Data.Extends /= No_Project loop
         if Data.Extends = Of_Project then
            return True;
         end if;

         Data := Projects.Table (Data.Extends);
      end loop;

      return False;
   end In_Extension_Chain;

   ------------
   -- Init_Q --
   ------------

   procedure Init_Q is
   begin
      Q_Front := Q.First;
      Q.Set_Last (Q.First);
   end Init_Q;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not Initialized then
         Initialized := True;

         --  Initialize some packages

         Csets.Initialize;
         Namet.Initialize;
         Snames.Initialize;
         Prj.Initialize;
      end if;

      --  Reset global variables

      Free (Object_Directory_Path);
      Do_Nothing := False;
      File_Deleted := False;
      Copyright_Displayed := False;
      Usage_Displayed := False;
      Free (Project_File_Name);
      Main_Project := Prj.No_Project;
      All_Projects := False;
   end Initialize;

   --------------
   -- Insert_Q --
   --------------

   procedure Insert_Q (Source_File : File_Name_Type) is
   begin
      --  Do not insert an empty name or an already marked source

      if Source_File /= No_Name
        and then Get_Name_Table_Byte (Source_File) = 0
      then
         Q.Table (Q.Last) := Source_File;
         Q.Increment_Last;

         --  Mark the source that has been just added to the Q

         Set_Name_Table_Byte (Source_File, 1);
      end if;
   end Insert_Q;

   ----------------------
   -- Object_File_Name --
   ----------------------

   function Object_File_Name (Source : Name_Id) return String is
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
   begin
      loop
         case
           GNAT.Command_Line.Getopt
             ("aO: c D: F h I: I- n P: q r v vP0 vP1 vP2 X:")
         is
            when ASCII.NUL =>
               exit;

            when 'a' =>
               Add_Lib_Search_Dir (GNAT.Command_Line.Parameter);

            when 'c'    =>
               Compile_Only := True;

            when 'D'    =>
               declare
                  Dir : constant String := GNAT.Command_Line.Parameter;

               begin
                  if Object_Directory_Path /= null then
                     Fail ("duplicate -D switch");

                  elsif Project_File_Name /= null then
                     Fail ("-P and -D cannot be used simultaneously");

                  elsif not Is_Directory (Dir) then
                     Fail (Dir, " is not a directory");

                  else
                     Add_Lib_Search_Dir (Dir);
                  end if;
               end;

            when 'F' =>
               Full_Path_Name_For_Brief_Errors := True;

            when 'h' =>
               Usage;

            when 'I' =>
               if Full_Switch = "I-" then
                  Opt.Look_In_Primary_Dir := False;

               else
                  Add_Lib_Search_Dir (GNAT.Command_Line.Parameter);
               end if;

            when 'n' =>
               Do_Nothing := True;

            when 'P' =>
               if Project_File_Name /= null then
                  Fail ("multiple -P switches");

               elsif Object_Directory_Path /= null then
                  Fail ("-D and -P cannot be used simultaneously");

               else
                  declare
                     Prj : constant String := GNAT.Command_Line.Parameter;
                  begin
                     if Prj'Length > 1 and then Prj (Prj'First) = '=' then
                        Project_File_Name :=
                          new String'(Prj (Prj'First + 1 ..  Prj'Last));

                     else
                        Project_File_Name := new String'(Prj);
                     end if;
                  end;
               end if;

            when 'q' =>
               Quiet_Output := True;

            when 'r' =>
               All_Projects := True;

            when 'v' =>
               if Full_Switch = "v" then
                  Verbose_Mode := True;

               elsif Full_Switch = "vP0" then
                  Prj.Com.Current_Verbosity := Prj.Default;

               elsif Full_Switch = "vP1" then
                  Prj.Com.Current_Verbosity := Prj.Medium;

               else
                  Prj.Com.Current_Verbosity := Prj.High;
               end if;

            when 'X' =>
               declare
                  Ext_Asgn  : constant String := GNAT.Command_Line.Parameter;
                  Start     : Positive := Ext_Asgn'First;
                  Stop      : Natural  := Ext_Asgn'Last;
                  Equal_Pos : Natural;
                  OK        : Boolean  := True;

               begin
                  if Ext_Asgn (Start) = '"' then
                     if Ext_Asgn (Stop) = '"' then
                        Start := Start + 1;
                        Stop  := Stop - 1;

                     else
                        OK := False;
                     end if;
                  end if;

                  Equal_Pos := Start;

                  while Equal_Pos <= Stop and then
                        Ext_Asgn (Equal_Pos) /= '='
                  loop
                     Equal_Pos := Equal_Pos + 1;
                  end loop;

                  if Equal_Pos = Start or else Equal_Pos > Stop then
                     OK := False;
                  end if;

                  if OK then
                     Prj.Ext.Add
                       (External_Name => Ext_Asgn (Start .. Equal_Pos - 1),
                        Value         => Ext_Asgn (Equal_Pos + 1 .. Stop));

                  else
                     Fail ("illegal external assignment '", Ext_Asgn, "'");
                  end if;
               end;

            when others =>
               Fail ("INTERNAL ERROR, please report");
         end case;
      end loop;

      --  Get the file names

      loop
         declare
            S : constant String := GNAT.Command_Line.Get_Argument;

         begin
            exit when S'Length = 0;

            Add_File (S);
         end;
      end loop;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Usage;
         Fail ("invalid switch : "& GNAT.Command_Line.Full_Switch);

      when GNAT.Command_Line.Invalid_Parameter =>
         Usage;
         Fail ("parameter missing for : " & GNAT.Command_Line.Full_Switch);
   end Parse_Cmd_Line;

   -----------------------
   -- Repinfo_File_Name --
   -----------------------

   function Repinfo_File_Name (Source : Name_Id) return String is
   begin
      return Get_Name_String (Source) & Repinfo_Suffix;
   end Repinfo_File_Name;

   --------------------
   -- Tree_File_Name --
   --------------------

   function Tree_File_Name (Source : Name_Id) return String is
      Src : constant String := Get_Name_String (Source);

   begin
      --  If the source name has an extension, then replace it with
      --  the tree suffix.

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
         Put_Line ("Usage: gnatclean [switches] names");
         New_Line;

         Put_Line ("  names is one or more file names from which " &
                   "the .adb or .ads suffix may be omitted");
         Put_Line ("  names may be omitted if -P<project> is specified");
         New_Line;

         Put_Line ("  -c       Only delete compiler generated files");
         Put_Line ("  -D dir   Specify dir as the object library");
         Put_Line ("  -F       Full project path name " &
                   "in brief error messages");
         Put_Line ("  -h       Display this message");
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

         Put_Line ("  -aOdir   Specify ALI/object files search path");
         Put_Line ("  -Idir    Like -aOdir");
         Put_Line ("  -I-      Don't look for source/library files " &
                   "in the default directory");
         New_Line;
      end if;
   end Usage;

begin
   if Hostparm.OpenVMS then
      Debug_Suffix (Debug_Suffix'First) := '_';
      Repinfo_Suffix (Repinfo_Suffix'First) := '_';
      B_Start (B_Start'Last) := '$';
   end if;
end Clean;
