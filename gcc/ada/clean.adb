------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                C L E A N                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2015, Free Software Foundation, Inc.         --
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
with Csets;
with Makeutl;  use Makeutl;
with MLib.Tgt; use MLib.Tgt;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.M;  use Osint.M;
with Prj;      use Prj;
with Prj.Env;
with Prj.Ext;
with Prj.Pars;
with Prj.Tree; use Prj.Tree;
with Prj.Util; use Prj.Util;
with Sdefault;
with Snames;
with Stringt;
with Switch;   use Switch;
with Table;
with Targparm; use Targparm;
with Types;    use Types;

with Ada.Command_Line;          use Ada.Command_Line;

with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.IO;                   use GNAT.IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

package body Clean is

   Initialized : Boolean := False;
   --  Set to True by the first call to Initialize to avoid reinitialization
   --  of some packages.

   --  Suffixes of various files

   Assembly_Suffix : constant String := ".s";
   ALI_Suffix      : constant String := ".ali";
   Tree_Suffix     : constant String := ".adt";
   Object_Suffix   : constant String := Get_Target_Object_Suffix.all;
   Debug_Suffix    : constant String := ".dg";
   Repinfo_Suffix  : constant String := ".rep";
   --  Suffix of representation info files

   B_Start : constant String := "b~";
   --  Prefix of binder generated file, and number of actual characters used

   Project_Tree : constant Project_Tree_Ref :=
     new Project_Tree_Data (Is_Root_Tree => True);
   --  The project tree

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

   Project_Node_Tree : Project_Node_Tree_Ref;

   Main_Project : Prj.Project_Id := Prj.No_Project;

   All_Projects : Boolean := False;

   --  Packages of project files where unknown attributes are errors

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
      Table_Increment      => 100,
      Table_Name           => "Clean.Processed_Projects");
   --  Table to keep track of what project files have been processed, when
   --  switch -r is specified.

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

   procedure Add_Source_Dir (N : String);
   --  Call Add_Src_Search_Dir and output one line when in verbose mode

   procedure Add_Source_Directories is
     new Prj.Env.For_All_Source_Dirs (Action => Add_Source_Dir);

   procedure Add_Object_Dir (N : String);
   --  Call Add_Lib_Search_Dir and output one line when in verbose mode

   procedure Add_Object_Directories is
     new Prj.Env.For_All_Object_Dirs (Action => Add_Object_Dir);

   function ALI_File_Name (Source : File_Name_Type) return String;
   --  Returns the name of the ALI file corresponding to Source

   function Assembly_File_Name (Source : File_Name_Type) return String;
   --  Returns the assembly file name corresponding to Source

   procedure Clean_Archive (Project : Project_Id; Global : Boolean);
   --  Delete a global archive or library project archive and the dependency
   --  file, if they exist.

   procedure Clean_Executables;
   --  Do the cleaning work when no project file is specified

   procedure Clean_Interface_Copy_Directory (Project : Project_Id);
   --  Delete files in an interface copy directory: any file that is a copy of
   --  a source of the project.

   procedure Clean_Library_Directory (Project : Project_Id);
   --  Delete the library file in a library directory and any ALI file of a
   --  source of the project in a library ALI directory.

   procedure Clean_Project (Project : Project_Id);
   --  Do the cleaning work when a project file is specified. This procedure
   --  calls itself recursively when there are several project files in the
   --  tree rooted at the main project file and switch -r has been specified.

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

   function In_Extension_Chain
     (Of_Project : Project_Id;
      Prj        : Project_Id) return Boolean;
   --  Returns True iff Prj is an extension of Of_Project or if Of_Project is
   --  an extension of Prj.

   procedure Usage;
   --  Display the usage. If called several times, the usage is displayed only
   --  the first time.

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

   function ALI_File_Name (Source : File_Name_Type) return String is
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

   -------------------
   -- Clean_Archive --
   -------------------

   procedure Clean_Archive (Project : Project_Id; Global : Boolean) is
      Current_Dir : constant Dir_Name_Str := Get_Current_Dir;

      Lib_Prefix : String_Access;
      Archive_Name : String_Access;
      --  The name of the archive file for this project

      Archive_Dep_Name : String_Access;
      --  The name of the archive dependency file for this project

      Obj_Dir : constant String :=
        Get_Name_String (Project.Object_Directory.Display_Name);

   begin
      Change_Dir (Obj_Dir);

      --  First, get the lib prefix, the archive file name and the archive
      --  dependency file name.

      if Global then
         Lib_Prefix :=
           new String'("lib" & Get_Name_String (Project.Display_Name));
      else
         Lib_Prefix :=
           new String'("lib" & Get_Name_String (Project.Library_Name));
      end if;

      Archive_Name := new String'(Lib_Prefix.all & '.' & Archive_Ext);
      Archive_Dep_Name := new String'(Lib_Prefix.all & ".deps");

      --  Delete the archive file and the archive dependency file, if they
      --  exist.

      if Is_Regular_File (Archive_Name.all) then
         Delete (Obj_Dir, Archive_Name.all);
      end if;

      if Is_Regular_File (Archive_Dep_Name.all) then
         Delete (Obj_Dir, Archive_Dep_Name.all);
      end if;

      Change_Dir (Current_Dir);
   end Clean_Archive;

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
      Queue.Initialize (Queue_Per_Obj_Dir => False);

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
              ((Format  => Format_Gnatmake,
                File    => Main_Lib_File,
                Unit    => No_Unit_Name,
                Index   => 0,
                Project => No_Project,
                Sid     => No_Source));
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
                                ((Format  => Format_Gnatmake,
                                  File    => Withs.Table (K).Afile,
                                  Unit    => No_Unit_Name,
                                  Index   => 0,
                                  Project => No_Project,
                                  Sid     => No_Source));
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

   ------------------------------------
   -- Clean_Interface_Copy_Directory --
   ------------------------------------

   procedure Clean_Interface_Copy_Directory (Project : Project_Id) is
      Current : constant String := Get_Current_Dir;

      Direc : Dir_Type;

      Name : String (1 .. 200);
      Last : Natural;

      Delete_File : Boolean;
      Unit        : Unit_Index;

   begin
      if Project.Library
        and then Project.Library_Src_Dir /= No_Path_Information
      then
         declare
            Directory : constant String :=
              Get_Name_String (Project.Library_Src_Dir.Display_Name);

         begin
            Change_Dir (Directory);
            Open (Direc, ".");

            --  For each regular file in the directory, if switch -n has not
            --  been specified, make it writable and delete the file if it is
            --  a copy of a source of the project.

            loop
               Read (Direc, Name, Last);
               exit when Last = 0;

               declare
                  Filename : constant String := Name (1 .. Last);

               begin
                  if Is_Regular_File (Filename) then
                     Canonical_Case_File_Name (Name (1 .. Last));
                     Delete_File := False;

                     Unit := Units_Htable.Get_First (Project_Tree.Units_HT);

                     --  Compare with source file names of the project

                     while Unit /= No_Unit_Index loop
                        if Unit.File_Names (Impl) /= null
                          and then Ultimate_Extending_Project_Of
                                     (Unit.File_Names (Impl).Project) = Project
                          and then
                            Get_Name_String (Unit.File_Names (Impl).File) =
                                                              Name (1 .. Last)
                        then
                           Delete_File := True;
                           exit;
                        end if;

                        if Unit.File_Names (Spec) /= null
                          and then Ultimate_Extending_Project_Of
                                     (Unit.File_Names (Spec).Project) = Project
                          and then
                            Get_Name_String
                              (Unit.File_Names (Spec).File) = Name (1 .. Last)
                        then
                           Delete_File := True;
                           exit;
                        end if;

                        Unit := Units_Htable.Get_Next (Project_Tree.Units_HT);
                     end loop;

                     if Delete_File then
                        if not Do_Nothing then
                           Set_Writable (Filename);
                        end if;

                        Delete (Directory, Filename);
                     end if;
                  end if;
               end;
            end loop;

            Close (Direc);

            --  Restore the initial working directory

            Change_Dir (Current);
         end;
      end if;
   end Clean_Interface_Copy_Directory;

   -----------------------------
   -- Clean_Library_Directory --
   -----------------------------

   Empty_String : aliased String := "";

   procedure Clean_Library_Directory (Project : Project_Id) is
      Current : constant String := Get_Current_Dir;

      Lib_Filename : constant String := Get_Name_String (Project.Library_Name);
      DLL_Name     : String :=
        DLL_Prefix & Lib_Filename & "." & DLL_Ext;
      Archive_Name : String :=
        "lib" & Lib_Filename & "." & Archive_Ext;
      Direc        : Dir_Type;

      Name : String (1 .. 200);
      Last : Natural;

      Delete_File : Boolean;

      Minor : String_Access := Empty_String'Access;
      Major : String_Access := Empty_String'Access;

   begin
      if Project.Library then
         if Project.Library_Kind /= Static
           and then MLib.Tgt.Library_Major_Minor_Id_Supported
           and then Project.Lib_Internal_Name /= No_Name
         then
            Minor := new String'(Get_Name_String (Project.Lib_Internal_Name));
            Major := new String'(MLib.Major_Id_Name (DLL_Name, Minor.all));
         end if;

         declare
            Lib_Directory     : constant String :=
              Get_Name_String (Project.Library_Dir.Display_Name);
            Lib_ALI_Directory : constant String :=
              Get_Name_String (Project.Library_ALI_Dir.Display_Name);

         begin
            Canonical_Case_File_Name (Archive_Name);
            Canonical_Case_File_Name (DLL_Name);

            if Is_Directory (Lib_Directory) then
               Change_Dir (Lib_Directory);
               Open (Direc, ".");

               --  For each regular file in the directory, if switch -n has not
               --  not been specified, make it writable and delete the file if
               --  it is the library file.

               loop
                  Read (Direc, Name, Last);
                  exit when Last = 0;

                  declare
                     Filename : constant String := Name (1 .. Last);

                  begin
                     if Is_Regular_File (Filename)
                       or else Is_Symbolic_Link (Filename)
                     then
                        Canonical_Case_File_Name (Name (1 .. Last));
                        Delete_File := False;

                        if (Project.Library_Kind = Static
                             and then Name (1 .. Last) = Archive_Name)
                          or else
                            ((Project.Library_Kind = Dynamic
                                or else
                              Project.Library_Kind = Relocatable)
                             and then
                               (Name (1 .. Last) = DLL_Name
                                  or else
                                Name (1 .. Last) = Minor.all
                                  or else
                                Name (1 .. Last) = Major.all))
                        then
                           if not Do_Nothing then
                              Set_Writable (Filename);
                           end if;

                           Delete (Lib_Directory, Filename);
                        end if;
                     end if;
                  end;
               end loop;

               Close (Direc);
            end if;

            if not Is_Directory (Lib_ALI_Directory) then
               --  Nothing more to do, return now
               return;
            end if;

            Change_Dir (Lib_ALI_Directory);
            Open (Direc, ".");

            --  For each regular file in the directory, if switch -n has not
            --  been specified, make it writable and delete the file if it is
            --  any ALI file of a source of the project.

            loop
               Read (Direc, Name, Last);
               exit when Last = 0;

               declare
                  Filename : constant String := Name (1 .. Last);
               begin
                  if Is_Regular_File (Filename) then
                     Canonical_Case_File_Name (Name (1 .. Last));
                     Delete_File := False;

                     if Last > 4 and then Name (Last - 3 .. Last) = ".ali" then
                        declare
                           Unit : Unit_Index;

                        begin
                           --  Compare with ALI file names of the project

                           Unit :=
                             Units_Htable.Get_First (Project_Tree.Units_HT);
                           while Unit /= No_Unit_Index loop
                              if Unit.File_Names (Impl) /= null
                                and then Unit.File_Names (Impl).Project /=
                                                                   No_Project
                              then
                                 if Ultimate_Extending_Project_Of
                                      (Unit.File_Names (Impl).Project) =
                                                                   Project
                                 then
                                    Get_Name_String
                                      (Unit.File_Names (Impl).File);
                                    Name_Len :=
                                      Name_Len -
                                        File_Extension
                                          (Name (1 .. Name_Len))'Length;
                                    if Name_Buffer (1 .. Name_Len) =
                                         Name (1 .. Last - 4)
                                    then
                                       Delete_File := True;
                                       exit;
                                    end if;
                                 end if;

                              elsif Unit.File_Names (Spec) /= null
                                and then Ultimate_Extending_Project_Of
                                           (Unit.File_Names (Spec).Project) =
                                                                    Project
                              then
                                 Get_Name_String (Unit.File_Names (Spec).File);
                                 Name_Len :=
                                   Name_Len -
                                     File_Extension
                                       (Name (1 .. Name_Len))'Length;

                                 if Name_Buffer (1 .. Name_Len) =
                                      Name (1 .. Last - 4)
                                 then
                                    Delete_File := True;
                                    exit;
                                 end if;
                              end if;

                              Unit :=
                                Units_Htable.Get_Next (Project_Tree.Units_HT);
                           end loop;
                        end;
                     end if;

                     if Delete_File then
                        if not Do_Nothing then
                           Set_Writable (Filename);
                        end if;

                        Delete (Lib_ALI_Directory, Filename);
                     end if;
                  end if;
               end;
            end loop;

            Close (Direc);

            --  Restore the initial working directory

            Change_Dir (Current);
         end;
      end if;
   end Clean_Library_Directory;

   -------------------
   -- Clean_Project --
   -------------------

   procedure Clean_Project (Project : Project_Id) is
      Main_Source_File : File_Name_Type;
      --  Name of executable on the command line without directory info

      Executable : File_Name_Type;
      --  Name of the executable file

      Current_Dir : constant Dir_Name_Str := Get_Current_Dir;
      Unit        : Unit_Index;
      File_Name1  : File_Name_Type;
      Index1      : Int;
      File_Name2  : File_Name_Type;
      Index2      : Int;
      Lib_File    : File_Name_Type;

      Global_Archive : Boolean := False;

   begin
      --  Check that we don't specify executable on the command line for
      --  a main library project.

      if Project = Main_Project
        and then Osint.Number_Of_Files /= 0
        and then Project.Library
      then
         Osint.Fail
           ("Cannot specify executable(s) for a Library Project File");
      end if;

      --  Nothing to clean in an externally built project

      if Project.Externally_Built then
         if Verbose_Mode then
            Put ("Nothing to do to clean externally built project """);
            Put (Get_Name_String (Project.Name));
            Put_Line ("""");
         end if;

      else
         if Verbose_Mode then
            Put ("Cleaning project """);
            Put (Get_Name_String (Project.Name));
            Put_Line ("""");
         end if;

         --  Add project to the list of processed projects

         Processed_Projects.Increment_Last;
         Processed_Projects.Table (Processed_Projects.Last) := Project;

         if Project.Object_Directory /= No_Path_Information
           and then Is_Directory
                      (Get_Name_String (Project.Object_Directory.Display_Name))
         then
            declare
               Obj_Dir : constant String :=
                 Get_Name_String (Project.Object_Directory.Display_Name);

            begin
               Change_Dir (Obj_Dir);

               --  First, deal with Ada

               --  Look through the units to find those that are either
               --  immediate sources or inherited sources of the project.
               --  Extending projects may have no language specified, if
               --  Source_Dirs or Source_Files is specified as an empty list,
               --  so always look for Ada units in extending projects.

               if Has_Ada_Sources (Project)
                 or else Project.Extends /= No_Project
               then
                  Unit := Units_Htable.Get_First (Project_Tree.Units_HT);
                  while Unit /= No_Unit_Index loop
                     File_Name1 := No_File;
                     File_Name2 := No_File;

                     --  If either the spec or the body is a source of the
                     --  project, check for the corresponding ALI file in the
                     --  object directory.

                     if (Unit.File_Names (Impl) /= null
                          and then
                            In_Extension_Chain
                              (Unit.File_Names (Impl).Project, Project))
                       or else
                         (Unit.File_Names (Spec) /= null
                           and then
                             In_Extension_Chain
                               (Unit.File_Names (Spec).Project, Project))
                     then
                        if Unit.File_Names (Impl) /= null then
                           File_Name1 := Unit.File_Names (Impl).File;
                           Index1     := Unit.File_Names (Impl).Index;
                        else
                           File_Name1 := No_File;
                           Index1     := 0;
                        end if;

                        if Unit.File_Names (Spec) /= null then
                           File_Name2 := Unit.File_Names (Spec).File;
                           Index2     := Unit.File_Names (Spec).Index;
                        else
                           File_Name2 := No_File;
                           Index2     := 0;
                        end if;

                        --  If there is no body file name, then there may be
                        --  only a spec.

                        if File_Name1 = No_File then
                           File_Name1 := File_Name2;
                           Index1     := Index2;
                           File_Name2 := No_File;
                           Index2     := 0;
                        end if;
                     end if;

                     --  If there is either a spec or a body, look for files
                     --  in the object directory.

                     if File_Name1 /= No_File then
                        Lib_File := Osint.Lib_File_Name (File_Name1, Index1);

                        declare
                           Asm : constant String :=
                                   Assembly_File_Name (Lib_File);
                           ALI : constant String :=
                                   ALI_File_Name      (Lib_File);
                           Obj : constant String :=
                                   Object_File_Name   (Lib_File);
                           Adt : constant String :=
                                   Tree_File_Name     (Lib_File);
                           Deb : constant String :=
                                   Debug_File_Name    (File_Name1);
                           Rep : constant String :=
                                   Repinfo_File_Name  (File_Name1);
                           Del : Boolean := True;

                        begin
                           --  If the ALI file exists and is read-only, no file
                           --  is deleted.

                           if Is_Regular_File (ALI) then
                              if Is_Writable_File (ALI) then
                                 Delete (Obj_Dir, ALI);

                              else
                                 Del := False;

                                 if Verbose_Mode then
                                    Put ('"');
                                    Put (Obj_Dir);

                                    if Obj_Dir (Obj_Dir'Last) /=
                                      Dir_Separator
                                    then
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

                              if File_Name2 /= No_File then
                                 declare
                                    Deb : constant String :=
                                      Debug_File_Name (File_Name2);
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

                     Unit := Units_Htable.Get_Next (Project_Tree.Units_HT);
                  end loop;
               end if;

               --  Check if a global archive and it dependency file could have
               --  been created and, if they exist, delete them.

               if Project = Main_Project and then not Project.Library then
                  Global_Archive := False;

                  declare
                     Proj : Project_List;

                  begin
                     Proj := Project_Tree.Projects;
                     while Proj /= null loop

                        --  For gnatmake, when the project specifies more than
                        --  just Ada as a language (even if course we could not
                        --  find any source file for the other languages), we
                        --  will take all the object files found in the object
                        --  directories. Since we know the project supports at
                        --  least Ada, we just have to test whether it has at
                        --  least two languages, and we do not care about the
                        --  sources.

                        if Proj.Project.Languages /= null
                          and then Proj.Project.Languages.Next /= null
                        then
                           Global_Archive := True;
                           exit;
                        end if;

                        Proj := Proj.Next;
                     end loop;
                  end;

                  if Global_Archive then
                     Clean_Archive (Project, Global => True);
                  end if;
               end if;

            end;
         end if;

         --  If this is a library project, clean the library directory, the
         --  interface copy dir and, for a Stand-Alone Library, the binder
         --  generated files of the library.

         --  The directories are cleaned only if switch -c is not specified

         if Project.Library then
            if not Compile_Only then
               Clean_Library_Directory (Project);

               if Project.Library_Src_Dir /= No_Path_Information then
                  Clean_Interface_Copy_Directory (Project);
               end if;
            end if;

            if Project.Standalone_Library /= No
              and then Project.Object_Directory /= No_Path_Information
            then
               Delete_Binder_Generated_Files
                 (Get_Name_String (Project.Object_Directory.Display_Name),
                  File_Name_Type (Project.Library_Name));
            end if;
         end if;

         if Verbose_Mode then
            New_Line;
         end if;
      end if;

      --  If switch -r is specified, call Clean_Project recursively for the
      --  imported projects and the project being extended.

      if All_Projects then
         declare
            Imported : Project_List;
            Process  : Boolean;

         begin
            --  For each imported project, call Clean_Project if the project
            --  has not been processed already.

            Imported := Project.Imported_Projects;
            while Imported /= null loop
               Process := True;

               for
                 J in Processed_Projects.First .. Processed_Projects.Last
               loop
                  if Imported.Project = Processed_Projects.Table (J) then
                     Process := False;
                     exit;
                  end if;
               end loop;

               if Process then
                  Clean_Project (Imported.Project);
               end if;

               Imported := Imported.Next;
            end loop;

            --  If this project extends another project, call Clean_Project for
            --  the project being extended. It is guaranteed that it has not
            --  called before, because no other project may import or extend
            --  this project.

            if Project.Extends /= No_Project then
               Clean_Project (Project.Extends);
            end if;
         end;
      end if;

         --  For the main project, delete the executables and the binder
         --  generated files.

         --  The executables are deleted only if switch -c is not specified

      if Project = Main_Project
        and then Project.Exec_Directory /= No_Path_Information
      then
         declare
            Exec_Dir : constant String :=
              Get_Name_String (Project.Exec_Directory.Display_Name);

         begin
            Change_Dir (Exec_Dir);

            for N_File in 1 .. Osint.Number_Of_Files loop
               Main_Source_File := Next_Main_Source;

               if not Compile_Only then
                  Executable :=
                    Executable_Of
                      (Main_Project,
                       Project_Tree.Shared,
                       Main_Source_File,
                       Current_File_Index);

                  declare
                     Exec_File_Name : constant String :=
                       Get_Name_String (Executable);

                  begin
                     if Is_Absolute_Path (Name => Exec_File_Name) then
                        if Is_Regular_File (Exec_File_Name) then
                           Delete ("", Exec_File_Name);
                        end if;

                     else
                        if Is_Regular_File (Exec_File_Name) then
                           Delete (Exec_Dir, Exec_File_Name);
                        end if;
                     end if;
                  end;
               end if;

               if Project.Object_Directory /= No_Path_Information
                 and then
                   Is_Directory
                     (Get_Name_String (Project.Object_Directory.Display_Name))
               then
                  Delete_Binder_Generated_Files
                    (Get_Name_String (Project.Object_Directory.Display_Name),
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

      --  Add the default project search directories now, after the directories
      --  that have been specified by switches -aP<dir>.

      Prj.Env.Initialize_Default_Project_Path
        (Root_Environment.Project_Path,
         Target_Name => Sdefault.Target_Name.all);

      if Verbose_Mode then
         Display_Copyright;
      end if;

      if Project_File_Name /= null then

         --  Warn about 'gnatclean -P'

         if Project_File_Name /= null then
            Put_Line
              ("warning: gnatclean -P is obsolete and will not be available" &
               " in the next release; use gprclean instead.");
         end if;

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

         Prj.Pars.Set_Verbosity (To => Current_Verbosity);

         --  Parse the project file. If there is an error, Main_Project
         --  will still be No_Project.

         Prj.Pars.Parse
           (Project           => Main_Project,
            In_Tree           => Project_Tree,
            In_Node_Tree      => Project_Node_Tree,
            Project_File_Name => Project_File_Name.all,
            Env               => Root_Environment,
            Packages_To_Check => Packages_To_Check_By_Gnatmake);

         if Main_Project = No_Project then
            Fail ("""" & Project_File_Name.all & """ processing failed");

         elsif Main_Project.Qualifier = Aggregate then
            Fail ("aggregate projects are not supported");

         elsif Aggregate_Libraries_In (Project_Tree) then
            Fail ("aggregate library projects are not supported");
         end if;

         if Opt.Verbose_Mode then
            New_Line;
            Put ("Parsing of Project File """);
            Put (Project_File_Name.all);
            Put (""" is finished.");
            New_Line;
         end if;

         --  Add source directories and object directories to the search paths

         Add_Source_Directories (Main_Project, Project_Tree);
         Add_Object_Directories (Main_Project, Project_Tree);
      end if;

      Osint.Add_Default_Search_Dirs;

      --  If a project file was specified, but no executable name, put all
      --  the mains of the project file (if any) as if there were on the
      --  command line.

      if Main_Project /= No_Project and then Osint.Number_Of_Files = 0 then
         declare
            Main  : String_Element;
            Value : String_List_Id := Main_Project.Mains;
         begin
            while Value /= Prj.Nil_String loop
               Main := Project_Tree.Shared.String_Elements.Table (Value);
               Osint.Add_File
                 (File_Name => Get_Name_String (Main.Value),
                  Index     => Main.Index);
               Value := Main.Next;
            end loop;
         end;
      end if;

      --  If neither a project file nor an executable were specified, exit
      --  displaying the usage if there were no arguments on the command line.

      if Main_Project = No_Project and then Osint.Number_Of_Files = 0 then
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

   ------------------------
   -- In_Extension_Chain --
   ------------------------

   function In_Extension_Chain
     (Of_Project : Project_Id;
      Prj        : Project_Id) return Boolean
   is
      Proj : Project_Id;

   begin
      if Prj = No_Project or else Of_Project = No_Project then
         return False;
      end if;

      if Of_Project = Prj then
         return True;
      end if;

      Proj := Of_Project;
      while Proj.Extends /= No_Project loop
         if Proj.Extends = Prj then
            return True;
         end if;

         Proj := Proj.Extends;
      end loop;

      Proj := Prj;
      while Proj.Extends /= No_Project loop
         if Proj.Extends = Of_Project then
            return True;
         end if;

         Proj := Proj.Extends;
      end loop;

      return False;
   end In_Extension_Chain;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not Initialized then
         Initialized := True;

         --  Get default search directories to locate system.ads when calling
         --  Targparm.Get_Target_Parameters.

         Osint.Add_Default_Search_Dirs;

         --  Initialize some packages

         Csets.Initialize;
         Snames.Initialize;
         Stringt.Initialize;

         Prj.Tree.Initialize (Root_Environment, Gnatmake_Flags);

         Project_Node_Tree := new Project_Node_Tree_Data;
         Prj.Tree.Initialize (Project_Node_Tree);

         Prj.Initialize (Project_Tree);
         Targparm.Get_Target_Parameters;
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
      Source_Index : Int := 0;
      Index        : Positive;

      procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

   begin
      --  First, check for --version and --help

      Check_Version_And_Help ("GNATCLEAN", "2003");

      --  First, for native gnatclean, check for switch -P and, if found and
      --  gprclean is available, silently invoke gprclean.

      Find_Program_Name;

      if Name_Buffer (1 .. Name_Len) = "gnatclean" then
         declare
            Call_Gprclean : Boolean := False;

         begin
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
               declare
                  Gprclean : String_Access :=
                               Locate_Exec_On_Path (Exec_Name => "gprclean");
                  Args     : Argument_List (1 .. Argument_Count);
                  Success  : Boolean;

               begin
                  if Gprclean /= null then
                     for J in 1 .. Argument_Count loop
                        Args (J) := new String'(Argument (J));
                     end loop;

                     Spawn (Gprclean.all, Args, Success);

                     Free (Gprclean);

                     if Success then
                        Exit_Program (E_Success);
                     end if;
                  end if;
               end;
            end if;
         end;
      end if;

      Index := 1;
      while Index <= Last loop
         declare
            Arg : constant String := Argument (Index);

            procedure Bad_Argument;
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
                           Subdirs :=
                             new String'
                               (Arg (Subdirs_Option'Length + 1 .. Arg'Last));

                        elsif Arg = Makeutl.Unchecked_Shared_Lib_Imports then
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
                           Prj.Env.Add_Directories
                             (Root_Environment.Project_Path,
                              Arg (4 .. Arg'Last));

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
                        All_Projects := True;

                     when 'v' =>
                        if Arg = "-v" then
                           Verbose_Mode := True;

                        elsif Arg = "-vP0" then
                           Current_Verbosity := Prj.Default;

                        elsif Arg = "-vP1" then
                           Current_Verbosity := Prj.Medium;

                        elsif Arg = "-vP2" then
                           Current_Verbosity := Prj.High;

                        else
                           Bad_Argument;
                        end if;

                     when 'X' =>
                        if Arg'Length = 2 then
                           Bad_Argument;
                        end if;

                        declare
                           Ext_Asgn  : constant String := Arg (3 .. Arg'Last);
                           Start     : Positive := Ext_Asgn'First;
                           Stop      : Natural  := Ext_Asgn'Last;
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

                           if not OK
                             or else not
                               Prj.Ext.Check (Root_Environment.External,
                                              Ext_Asgn (Start .. Stop))
                           then
                              Fail
                                ("illegal external assignment '"
                                 & Ext_Asgn
                                 & "'");
                           end if;
                        end;

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
         Put_Line ("  " & Makeutl.Unchecked_Shared_Lib_Imports);
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
