------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E N V                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2009, Free Software Foundation, Inc.         --
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

with Fmap;
with Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Com;  use Prj.Com;
with Tempdir;

package body Prj.Env is

   Default_Naming    : constant Naming_Id := Naming_Table.First;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_To_Path
     (Source_Dirs : String_List_Id;
      In_Tree     : Project_Tree_Ref);
   --  Add to Ada_Path_Buffer all the source directories in string list
   --  Source_Dirs, if any. Increment Ada_Path_Length.

   procedure Add_To_Path (Dir : String; In_Tree : Project_Tree_Ref);
   --  If Dir is not already in the global variable Ada_Path_Buffer, add it.
   --  Increment Ada_Path_Length.
   --  If Ada_Path_Length /= 0, prepend a Path_Separator character to
   --  Path.

   procedure Add_To_Source_Path
     (Source_Dirs : String_List_Id; In_Tree : Project_Tree_Ref);
   --  Add to Ada_Path_B all the source directories in string list
   --  Source_Dirs, if any. Increment Ada_Path_Length.

   procedure Add_To_Object_Path
     (Object_Dir : Path_Name_Type;
      In_Tree    : Project_Tree_Ref);
   --  Add Object_Dir to object path table. Make sure it is not duplicate
   --  and it is the last one in the current table.

   procedure Set_Path_File_Var (Name : String; Value : String);
   --  Call Setenv, after calling To_Host_File_Spec

   function Ultimate_Extension_Of
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return Project_Id;
   --  Return a project that is either Project or an extended ancestor of
   --  Project that itself is not extended.

   ----------------------
   -- Ada_Include_Path --
   ----------------------

   function Ada_Include_Path
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return String_Access
   is
      procedure Add (Project : Project_Id; Dummy : in out Boolean);
      --  Add source dirs of Project to the path

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id; Dummy : in out Boolean) is
         pragma Unreferenced (Dummy);
      begin
         Add_To_Path (In_Tree.Projects.Table (Project).Source_Dirs, In_Tree);
      end Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Add);
      Dummy : Boolean := False;

   --  Start of processing for Ada_Include_Path

   begin
      --  If it is the first time we call this function for
      --  this project, compute the source path

      if In_Tree.Projects.Table (Project).Ada_Include_Path = null then
         In_Tree.Private_Part.Ada_Path_Length := 0;
         For_All_Projects (Project, In_Tree, Dummy);

         In_Tree.Projects.Table (Project).Ada_Include_Path :=
           new String'
             (In_Tree.Private_Part.Ada_Path_Buffer
                  (1 .. In_Tree.Private_Part.Ada_Path_Length));
      end if;

      return In_Tree.Projects.Table (Project).Ada_Include_Path;
   end Ada_Include_Path;

   ----------------------
   -- Ada_Include_Path --
   ----------------------

   function Ada_Include_Path
     (Project   : Project_Id;
      In_Tree   : Project_Tree_Ref;
      Recursive : Boolean) return String
   is
   begin
      if Recursive then
         return Ada_Include_Path (Project, In_Tree).all;
      else
         In_Tree.Private_Part.Ada_Path_Length := 0;
         Add_To_Path
           (In_Tree.Projects.Table (Project).Source_Dirs, In_Tree);
         return
           In_Tree.Private_Part.Ada_Path_Buffer
             (1 .. In_Tree.Private_Part.Ada_Path_Length);
      end if;
   end Ada_Include_Path;

   ----------------------
   -- Ada_Objects_Path --
   ----------------------

   function Ada_Objects_Path
     (Project             : Project_Id;
      In_Tree             : Project_Tree_Ref;
      Including_Libraries : Boolean := True) return String_Access
   is
      procedure Add (Project : Project_Id; Dummy : in out Boolean);
      --  Add all the object directories of a project to the path

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id; Dummy : in out Boolean) is
         pragma Unreferenced (Dummy);
         Path : constant Path_Name_Type :=
                  Get_Object_Directory
                    (In_Tree, Project,
                     Including_Libraries => Including_Libraries,
                     Only_If_Ada         => False);
      begin
         if Path /= No_Path then
            Add_To_Path (Get_Name_String (Path), In_Tree);
         end if;
      end Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Add);
      Dummy : Boolean := False;

   --  Start of processing for Ada_Objects_Path

   begin
      --  If it is the first time we call this function for
      --  this project, compute the objects path

      if In_Tree.Projects.Table (Project).Ada_Objects_Path = null then
         In_Tree.Private_Part.Ada_Path_Length := 0;
         For_All_Projects (Project, In_Tree, Dummy);

         In_Tree.Projects.Table (Project).Ada_Objects_Path :=
           new String'
             (In_Tree.Private_Part.Ada_Path_Buffer
                  (1 .. In_Tree.Private_Part.Ada_Path_Length));
      end if;

      return In_Tree.Projects.Table (Project).Ada_Objects_Path;
   end Ada_Objects_Path;

   ------------------------
   -- Add_To_Object_Path --
   ------------------------

   procedure Add_To_Object_Path
     (Object_Dir : Path_Name_Type; In_Tree : Project_Tree_Ref)
   is
   begin
      --  Check if the directory is already in the table

      for Index in Object_Path_Table.First ..
                   Object_Path_Table.Last (In_Tree.Private_Part.Object_Paths)
      loop

         --  If it is, remove it, and add it as the last one

         if In_Tree.Private_Part.Object_Paths.Table (Index) = Object_Dir then
            for Index2 in Index + 1 ..
                          Object_Path_Table.Last
                            (In_Tree.Private_Part.Object_Paths)
            loop
               In_Tree.Private_Part.Object_Paths.Table (Index2 - 1) :=
                 In_Tree.Private_Part.Object_Paths.Table (Index2);
            end loop;

            In_Tree.Private_Part.Object_Paths.Table
              (Object_Path_Table.Last (In_Tree.Private_Part.Object_Paths)) :=
                 Object_Dir;
            return;
         end if;
      end loop;

      --  The directory is not already in the table, add it

      Object_Path_Table.Increment_Last (In_Tree.Private_Part.Object_Paths);
      In_Tree.Private_Part.Object_Paths.Table
        (Object_Path_Table.Last (In_Tree.Private_Part.Object_Paths)) :=
           Object_Dir;
   end Add_To_Object_Path;

   -----------------
   -- Add_To_Path --
   -----------------

   procedure Add_To_Path
     (Source_Dirs : String_List_Id;
      In_Tree     : Project_Tree_Ref)
   is
      Current    : String_List_Id := Source_Dirs;
      Source_Dir : String_Element;
   begin
      while Current /= Nil_String loop
         Source_Dir := In_Tree.String_Elements.Table (Current);
         Add_To_Path (Get_Name_String (Source_Dir.Display_Value), In_Tree);
         Current := Source_Dir.Next;
      end loop;
   end Add_To_Path;

   procedure Add_To_Path (Dir : String; In_Tree : Project_Tree_Ref) is
      Len        : Natural;
      New_Buffer : String_Access;
      Min_Len    : Natural;

      function Is_Present (Path : String; Dir : String) return Boolean;
      --  Return True if Dir is part of Path

      ----------------
      -- Is_Present --
      ----------------

      function Is_Present (Path : String; Dir : String) return Boolean is
         Last : constant Integer := Path'Last - Dir'Length + 1;

      begin
         for J in Path'First .. Last loop

            --  Note: the order of the conditions below is important, since
            --  it ensures a minimal number of string comparisons.

            if (J = Path'First
                or else Path (J - 1) = Path_Separator)
              and then
                (J + Dir'Length > Path'Last
                 or else Path (J + Dir'Length) = Path_Separator)
              and then Dir = Path (J .. J + Dir'Length - 1)
            then
               return True;
            end if;
         end loop;

         return False;
      end Is_Present;

   --  Start of processing for Add_To_Path

   begin
      if Is_Present (In_Tree.Private_Part.Ada_Path_Buffer
                       (1 .. In_Tree.Private_Part.Ada_Path_Length),
                     Dir)
      then

         --  Dir is already in the path, nothing to do

         return;
      end if;

      Min_Len := In_Tree.Private_Part.Ada_Path_Length + Dir'Length;

      if In_Tree.Private_Part.Ada_Path_Length > 0 then

         --  Add 1 for the Path_Separator character

         Min_Len := Min_Len + 1;
      end if;

      --  If Ada_Path_Buffer is too small, increase it

      Len := In_Tree.Private_Part.Ada_Path_Buffer'Last;

      if Len < Min_Len then
         loop
            Len := Len * 2;
            exit when Len >= Min_Len;
         end loop;

         New_Buffer := new String (1 .. Len);
         New_Buffer (1 .. In_Tree.Private_Part.Ada_Path_Length) :=
           In_Tree.Private_Part.Ada_Path_Buffer
             (1 .. In_Tree.Private_Part.Ada_Path_Length);
         Free (In_Tree.Private_Part.Ada_Path_Buffer);
         In_Tree.Private_Part.Ada_Path_Buffer := New_Buffer;
      end if;

      if In_Tree.Private_Part.Ada_Path_Length > 0 then
         In_Tree.Private_Part.Ada_Path_Length :=
           In_Tree.Private_Part.Ada_Path_Length + 1;
         In_Tree.Private_Part.Ada_Path_Buffer
           (In_Tree.Private_Part.Ada_Path_Length) := Path_Separator;
      end if;

      In_Tree.Private_Part.Ada_Path_Buffer
        (In_Tree.Private_Part.Ada_Path_Length + 1 ..
           In_Tree.Private_Part.Ada_Path_Length + Dir'Length) := Dir;
      In_Tree.Private_Part.Ada_Path_Length :=
        In_Tree.Private_Part.Ada_Path_Length + Dir'Length;
   end Add_To_Path;

   ------------------------
   -- Add_To_Source_Path --
   ------------------------

   procedure Add_To_Source_Path
     (Source_Dirs : String_List_Id; In_Tree : Project_Tree_Ref)
   is
      Current    : String_List_Id := Source_Dirs;
      Source_Dir : String_Element;
      Add_It     : Boolean;

   begin
      --  Add each source directory

      while Current /= Nil_String loop
         Source_Dir := In_Tree.String_Elements.Table (Current);
         Add_It := True;

         --  Check if the source directory is already in the table

         for Index in Source_Path_Table.First ..
                      Source_Path_Table.Last
                                          (In_Tree.Private_Part.Source_Paths)
         loop
            --  If it is already, no need to add it

            if In_Tree.Private_Part.Source_Paths.Table (Index) =
                        Source_Dir.Value
            then
               Add_It := False;
               exit;
            end if;
         end loop;

         if Add_It then
            Source_Path_Table.Increment_Last
              (In_Tree.Private_Part.Source_Paths);
            In_Tree.Private_Part.Source_Paths.Table
              (Source_Path_Table.Last (In_Tree.Private_Part.Source_Paths)) :=
              Source_Dir.Value;
         end if;

         --  Next source directory

         Current := Source_Dir.Next;
      end loop;
   end Add_To_Source_Path;

   --------------------------------
   -- Create_Config_Pragmas_File --
   --------------------------------

   procedure Create_Config_Pragmas_File
     (For_Project          : Project_Id;
      Main_Project         : Project_Id;
      In_Tree              : Project_Tree_Ref;
      Include_Config_Files : Boolean := True)
   is
      pragma Unreferenced (Main_Project);
      pragma Unreferenced (Include_Config_Files);

      File_Name : Path_Name_Type  := No_Path;
      File      : File_Descriptor := Invalid_FD;

      Current_Unit : Unit_Index := Unit_Table.First;

      First_Project : Project_List;

      Current_Project : Project_List;
      Current_Naming  : Naming_Id;

      Status : Boolean;
      --  For call to Close

      procedure Check (Project : Project_Id);
      --  Recursive procedure that put in the config pragmas file any non
      --  standard naming schemes, if it is not already in the file, then call
      --  itself for any imported project.

      procedure Check_Temp_File;
      --  Check that a temporary file has been opened.
      --  If not, create one, and put its name in the project data,
      --  with the indication that it is a temporary file.

      procedure Put
        (Unit_Name : Name_Id;
         File_Name : File_Name_Type;
         Unit_Kind : Spec_Or_Body;
         Index     : Int);
      --  Put an SFN pragma in the temporary file

      procedure Put (File : File_Descriptor; S : String);
      procedure Put_Line (File : File_Descriptor; S : String);
      --  Output procedures, analogous to normal Text_IO procs of same name

      -----------
      -- Check --
      -----------

      procedure Check (Project : Project_Id) is
         Data : constant Project_Data :=
           In_Tree.Projects.Table (Project);

      begin
         if Current_Verbosity = High then
            Write_Str ("Checking project file """);
            Write_Str (Namet.Get_Name_String (Data.Name));
            Write_Str (""".");
            Write_Eol;
         end if;

         --  Is this project in the list of the visited project?

         Current_Project := First_Project;
         while Current_Project /= null
           and then Current_Project.Project /= Project
         loop
            Current_Project := Current_Project.Next;
         end loop;

         --  If it is not, put it in the list, and visit it

         if Current_Project = null then
            First_Project := new Project_List_Element'
              (Project => Project,
               Next    => First_Project);

            --  Is the naming scheme of this project one that we know?

            Current_Naming := Default_Naming;
            while Current_Naming <=
                    Naming_Table.Last (In_Tree.Private_Part.Namings)
              and then not Same_Naming_Scheme
              (Left => In_Tree.Private_Part.Namings.Table (Current_Naming),
               Right => Data.Naming) loop
               Current_Naming := Current_Naming + 1;
            end loop;

            --  If we don't know it, add it

            if Current_Naming >
                 Naming_Table.Last (In_Tree.Private_Part.Namings)
            then
               Naming_Table.Increment_Last (In_Tree.Private_Part.Namings);
               In_Tree.Private_Part.Namings.Table
                 (Naming_Table.Last (In_Tree.Private_Part.Namings)) :=
                    Data.Naming;

               --  We need a temporary file to be created

               Check_Temp_File;

               --  Put the SFN pragmas for the naming scheme

               --  Spec

               Put_Line
                 (File, "pragma Source_File_Name_Project");
               Put_Line
                 (File, "  (Spec_File_Name  => ""*" &
                  Spec_Suffix_Of (In_Tree, "ada", Data.Naming) &
                  """,");
               Put_Line
                 (File, "   Casing          => " &
                  Image (Data.Naming.Casing) & ",");
               Put_Line
                 (File, "   Dot_Replacement => """ &
                 Namet.Get_Name_String (Data.Naming.Dot_Replacement) &
                  """);");

               --  and body

               Put_Line
                 (File, "pragma Source_File_Name_Project");
               Put_Line
                 (File, "  (Body_File_Name  => ""*" &
                  Body_Suffix_Of (In_Tree, "ada", Data.Naming) &
                  """,");
               Put_Line
                 (File, "   Casing          => " &
                  Image (Data.Naming.Casing) & ",");
               Put_Line
                 (File, "   Dot_Replacement => """ &
                  Namet.Get_Name_String (Data.Naming.Dot_Replacement) &
                  """);");

               --  and maybe separate

               if Body_Suffix_Of (In_Tree, "ada", Data.Naming) /=
                  Get_Name_String (Data.Naming.Separate_Suffix)
               then
                  Put_Line
                    (File, "pragma Source_File_Name_Project");
                  Put_Line
                    (File, "  (Subunit_File_Name  => ""*" &
                     Namet.Get_Name_String (Data.Naming.Separate_Suffix) &
                     """,");
                  Put_Line
                    (File, "   Casing          => " &
                     Image (Data.Naming.Casing) &
                     ",");
                  Put_Line
                    (File, "   Dot_Replacement => """ &
                     Namet.Get_Name_String (Data.Naming.Dot_Replacement) &
                     """);");
               end if;
            end if;

            if Data.Extends /= No_Project then
               Check (Data.Extends);
            end if;

            declare
               Current : Project_List := Data.Imported_Projects;
            begin
               while Current /= null loop
                  Check (Current.Project);
                  Current := Current.Next;
               end loop;
            end;
         end if;
      end Check;

      ---------------------
      -- Check_Temp_File --
      ---------------------

      procedure Check_Temp_File is
      begin
         if File = Invalid_FD then
            Tempdir.Create_Temp_File (File, Name => File_Name);

            if File = Invalid_FD then
               Prj.Com.Fail
                 ("unable to create temporary configuration pragmas file");

            else
               Record_Temp_File (File_Name);

               if Opt.Verbose_Mode then
                  Write_Str ("Creating temp file """);
                  Write_Str (Get_Name_String (File_Name));
                  Write_Line ("""");
               end if;
            end if;
         end if;
      end Check_Temp_File;

      ---------
      -- Put --
      ---------

      procedure Put
        (Unit_Name : Name_Id;
         File_Name : File_Name_Type;
         Unit_Kind : Spec_Or_Body;
         Index     : Int)
      is
      begin
         --  A temporary file needs to be open

         Check_Temp_File;

         --  Put the pragma SFN for the unit kind (spec or body)

         Put (File, "pragma Source_File_Name_Project (");
         Put (File, Namet.Get_Name_String (Unit_Name));

         if Unit_Kind = Specification then
            Put (File, ", Spec_File_Name => """);
         else
            Put (File, ", Body_File_Name => """);
         end if;

         Put (File, Namet.Get_Name_String (File_Name));
         Put (File, """");

         if Index /= 0 then
            Put (File, ", Index =>");
            Put (File, Index'Img);
         end if;

         Put_Line (File, ");");
      end Put;

      procedure Put (File : File_Descriptor; S : String) is
         Last : Natural;

      begin
         Last := Write (File, S (S'First)'Address, S'Length);

         if Last /= S'Length then
            Prj.Com.Fail ("Disk full");
         end if;

         if Current_Verbosity = High then
            Write_Str (S);
         end if;
      end Put;

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (File : File_Descriptor; S : String) is
         S0   : String (1 .. S'Length + 1);
         Last : Natural;

      begin
         --  Add an ASCII.LF to the string. As this config file is supposed to
         --  be used only by the compiler, we don't care about the characters
         --  for the end of line. In fact we could have put a space, but
         --  it is more convenient to be able to read gnat.adc during
         --  development, for which the ASCII.LF is fine.

         S0 (1 .. S'Length) := S;
         S0 (S0'Last) := ASCII.LF;
         Last := Write (File, S0'Address, S0'Length);

         if Last /= S'Length + 1 then
            Prj.Com.Fail ("Disk full");
         end if;

         if Current_Verbosity = High then
            Write_Line (S);
         end if;
      end Put_Line;

   --  Start of processing for Create_Config_Pragmas_File

   begin
      if not
        In_Tree.Projects.Table (For_Project).Config_Checked
      then

         --  Remove any memory of processed naming schemes, if any

         Naming_Table.Set_Last (In_Tree.Private_Part.Namings, Default_Naming);

         --  Check the naming schemes

         Check (For_Project);

         --  Visit all the units and process those that need an SFN pragma

         while
           Current_Unit <= Unit_Table.Last (In_Tree.Units)
         loop
            declare
               Unit : constant Unit_Data :=
                 In_Tree.Units.Table (Current_Unit);

            begin
               if Unit.File_Names (Specification).Needs_Pragma then
                  Put (Unit.Name,
                       Unit.File_Names (Specification).Name,
                       Specification,
                       Unit.File_Names (Specification).Index);
               end if;

               if Unit.File_Names (Body_Part).Needs_Pragma then
                  Put (Unit.Name,
                       Unit.File_Names (Body_Part).Name,
                       Body_Part,
                       Unit.File_Names (Body_Part).Index);
               end if;

               Current_Unit := Current_Unit + 1;
            end;
         end loop;

         --  If there are no non standard naming scheme, issue the GNAT
         --  standard naming scheme. This will tell the compiler that
         --  a project file is used and will forbid any pragma SFN.

         if File = Invalid_FD then
            Check_Temp_File;

            Put_Line (File, "pragma Source_File_Name_Project");
            Put_Line (File, "   (Spec_File_Name  => ""*.ads"",");
            Put_Line (File, "    Dot_Replacement => ""-"",");
            Put_Line (File, "    Casing          => lowercase);");

            Put_Line (File, "pragma Source_File_Name_Project");
            Put_Line (File, "   (Body_File_Name  => ""*.adb"",");
            Put_Line (File, "    Dot_Replacement => ""-"",");
            Put_Line (File, "    Casing          => lowercase);");
         end if;

         --  Close the temporary file

         GNAT.OS_Lib.Close (File, Status);

         if not Status then
            Prj.Com.Fail ("disk full");
         end if;

         if Opt.Verbose_Mode then
            Write_Str ("Closing configuration file """);
            Write_Str (Get_Name_String (File_Name));
            Write_Line ("""");
         end if;

         In_Tree.Projects.Table (For_Project).Config_File_Name :=
           File_Name;
         In_Tree.Projects.Table (For_Project).Config_File_Temp :=
           True;

         In_Tree.Projects.Table (For_Project).Config_Checked :=
           True;
      end if;
   end Create_Config_Pragmas_File;

   --------------------
   -- Create_Mapping --
   --------------------

   procedure Create_Mapping (In_Tree : Project_Tree_Ref) is
      The_Unit_Data : Unit_Data;
      Data          : File_Name_Data;

   begin
      Fmap.Reset_Tables;

      for Unit in 1 .. Unit_Table.Last (In_Tree.Units) loop
         The_Unit_Data := In_Tree.Units.Table (Unit);

         --  Process only if the unit has a valid name

         if The_Unit_Data.Name /= No_Name then
            Data := The_Unit_Data.File_Names (Specification);

            --  If there is a spec, put it in the mapping

            if Data.Name /= No_File then
               if Data.Path.Name = Slash then
                  Fmap.Add_Forbidden_File_Name (Data.Name);
               else
                  Fmap.Add_To_File_Map
                    (Unit_Name => Unit_Name_Type (The_Unit_Data.Name),
                     File_Name => Data.Name,
                     Path_Name => File_Name_Type (Data.Path.Name));
               end if;
            end if;

            Data := The_Unit_Data.File_Names (Body_Part);

            --  If there is a body (or subunit) put it in the mapping

            if Data.Name /= No_File then
               if Data.Path.Name = Slash then
                  Fmap.Add_Forbidden_File_Name (Data.Name);
               else
                  Fmap.Add_To_File_Map
                    (Unit_Name => Unit_Name_Type (The_Unit_Data.Name),
                     File_Name => Data.Name,
                     Path_Name => File_Name_Type (Data.Path.Name));
               end if;
            end if;
         end if;
      end loop;
   end Create_Mapping;

   -------------------------
   -- Create_Mapping_File --
   -------------------------

   procedure Create_Mapping_File
     (Project  : Project_Id;
      Language : Name_Id := No_Name;
      In_Tree  : Project_Tree_Ref;
      Name     : out Path_Name_Type)
   is
      File   : File_Descriptor := Invalid_FD;
      Status : Boolean;

      Present : array (No_Project .. Project_Table.Last (In_Tree.Projects))
                  of Boolean := (others => False);
      --  For each project in the closure of Project, the corresponding flag
      --  will be set to True.

      Source        : Source_Id;
      Suffix        : File_Name_Type;
      The_Unit_Data : Unit_Data;
      Data          : File_Name_Data;
      Iter          : Source_Iterator;

      procedure Put_Name_Buffer;
      --  Put the line contained in the Name_Buffer in the mapping file

      procedure Put_Data (Spec : Boolean);
      --  Put the mapping of the spec or body contained in Data in the file
      --  (3 lines).

      procedure Recursive_Flag (Prj : Project_Id);
      --  Set the flags corresponding to Prj, the projects it imports
      --  (directly or indirectly) or extends to True. Call itself recursively.

      ---------
      -- Put --
      ---------

      procedure Put_Name_Buffer is
         Last : Natural;

      begin
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ASCII.LF;
         Last := Write (File, Name_Buffer (1)'Address, Name_Len);

         if Last /= Name_Len then
            Prj.Com.Fail ("Disk full, cannot write mapping file");
         end if;
      end Put_Name_Buffer;

      --------------
      -- Put_Data --
      --------------

      procedure Put_Data (Spec : Boolean) is
      begin
         --  Line with the unit name

         Get_Name_String (The_Unit_Data.Name);
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := '%';
         Name_Len := Name_Len + 1;

         if Spec then
            Name_Buffer (Name_Len) := 's';
         else
            Name_Buffer (Name_Len) := 'b';
         end if;

         Put_Name_Buffer;

         --  Line with the file name

         Get_Name_String (Data.Name);
         Put_Name_Buffer;

         --  Line with the path name

         Get_Name_String (Data.Path.Name);
         Put_Name_Buffer;
      end Put_Data;

      --------------------
      -- Recursive_Flag --
      --------------------

      procedure Recursive_Flag (Prj : Project_Id) is
         Imported : Project_List;

      begin
         --  Nothing to do for non existent project or project that has already
         --  been flagged.

         if Prj /= No_Project and then not Present (Prj) then
            Present (Prj) := True;

            Imported := In_Tree.Projects.Table (Prj).Imported_Projects;
            while Imported /= null loop
               Recursive_Flag (Imported.Project);
               Imported := Imported.Next;
            end loop;

            Recursive_Flag (In_Tree.Projects.Table (Prj).Extends);
         end if;
      end Recursive_Flag;

   --  Start of processing for Create_Mapping_File

   begin
      --  Flag the necessary projects

      Recursive_Flag (Project);

      --  Create the temporary file

      Tempdir.Create_Temp_File (File, Name => Name);

      if File = Invalid_FD then
         Prj.Com.Fail ("unable to create temporary mapping file");

      else
         Record_Temp_File (Name);

         if Opt.Verbose_Mode then
            Write_Str ("Creating temp mapping file """);
            Write_Str (Get_Name_String (Name));
            Write_Line ("""");
         end if;
      end if;

      if Language = No_Name then
         if In_Tree.Private_Part.Fill_Mapping_File then
            for Unit in 1 .. Unit_Table.Last (In_Tree.Units) loop
               The_Unit_Data := In_Tree.Units.Table (Unit);

               --  Case of unit has a valid name

               if The_Unit_Data.Name /= No_Name then
                  Data := The_Unit_Data.File_Names (Specification);

                  --  If there is a spec, put it mapping in the file if it is
                  --  from a project in the closure of Project.

                  if Data.Name /= No_File and then Present (Data.Project) then
                     Put_Data (Spec => True);
                  end if;

                  Data := The_Unit_Data.File_Names (Body_Part);

                  --  If there is a body (or subunit) put its mapping in the
                  --  file if it is from a project in the closure of Project.

                  if Data.Name /= No_File and then Present (Data.Project) then
                     Put_Data (Spec => False);
                  end if;
               end if;
            end loop;
         end if;

      --  If language is defined
      else
         --  For all source of the Language of all projects in the closure

         for Proj in Present'Range loop
            if Present (Proj) then

               Iter := For_Each_Source (In_Tree, Proj);
               loop
                  Source := Prj.Element (Iter);
                  exit when Source = No_Source;

                  if Source.Language.Name = Language
                    and then not Source.Locally_Removed
                    and then Source.Replaced_By = No_Source
                    and then Source.Path.Name /= No_Path
                  then
                     if Source.Unit /= No_Name then
                        Get_Name_String (Source.Unit);

                        if Source.Kind = Spec then
                           Suffix :=
                             Source.Language.Config.Mapping_Spec_Suffix;
                        else
                           Suffix :=
                             Source.Language.Config.Mapping_Body_Suffix;
                        end if;

                        if Suffix /= No_File then
                           Add_Str_To_Name_Buffer (Get_Name_String (Suffix));
                        end if;

                        Put_Name_Buffer;
                     end if;

                     Get_Name_String (Source.File);
                     Put_Name_Buffer;

                     Get_Name_String (Source.Path.Name);
                     Put_Name_Buffer;
                  end if;

                  Next (Iter);
               end loop;
            end if;
         end loop;
      end if;

      GNAT.OS_Lib.Close (File, Status);

      if not Status then

         --  We were able to create the temporary file, so there is no problem
         --  of protection. However, we are not able to close it, so there must
         --  be a capacity problem that we express using "disk full".

         Prj.Com.Fail ("disk full, could not write mapping file");
      end if;
   end Create_Mapping_File;

   --------------------------
   -- Create_New_Path_File --
   --------------------------

   procedure Create_New_Path_File
     (In_Tree   : Project_Tree_Ref;
      Path_FD   : out File_Descriptor;
      Path_Name : out Path_Name_Type)
   is
   begin
      Tempdir.Create_Temp_File (Path_FD, Path_Name);

      if Path_Name /= No_Path then
         Record_Temp_File (Path_Name);

         --  Record the name, so that the temp path file will be deleted at the
         --  end of the program.

         Path_File_Table.Increment_Last (In_Tree.Private_Part.Path_Files);
         In_Tree.Private_Part.Path_Files.Table
           (Path_File_Table.Last (In_Tree.Private_Part.Path_Files)) :=
              Path_Name;
      end if;
   end Create_New_Path_File;

   ---------------------------
   -- Delete_All_Path_Files --
   ---------------------------

   procedure Delete_All_Path_Files (In_Tree : Project_Tree_Ref) is
      Disregard : Boolean := True;
      pragma Warnings (Off, Disregard);

   begin
      for Index in Path_File_Table.First ..
                   Path_File_Table.Last (In_Tree.Private_Part.Path_Files)
      loop
         if In_Tree.Private_Part.Path_Files.Table (Index) /= No_Path then
            Delete_File
              (Get_Name_String
                 (In_Tree.Private_Part.Path_Files.Table (Index)),
               Disregard);
         end if;
      end loop;

      --  If any of the environment variables ADA_PRJ_INCLUDE_FILE or
      --  ADA_PRJ_OBJECTS_FILE has been set, then reset their value to
      --  the empty string. On VMS, this has the effect of deassigning
      --  the logical names.

      if In_Tree.Private_Part.Ada_Prj_Include_File_Set then
         Setenv (Project_Include_Path_File, "");
         In_Tree.Private_Part.Ada_Prj_Include_File_Set := False;
      end if;

      if In_Tree.Private_Part.Ada_Prj_Objects_File_Set then
         Setenv (Project_Objects_Path_File, "");
         In_Tree.Private_Part.Ada_Prj_Objects_File_Set := False;
      end if;
   end Delete_All_Path_Files;

   ------------------------------------
   -- File_Name_Of_Library_Unit_Body --
   ------------------------------------

   function File_Name_Of_Library_Unit_Body
     (Name              : String;
      Project           : Project_Id;
      In_Tree           : Project_Tree_Ref;
      Main_Project_Only : Boolean := True;
      Full_Path         : Boolean := False) return String
   is
      The_Project   : Project_Id := Project;
      Data          : Project_Data :=
                        In_Tree.Projects.Table (Project);
      Original_Name : String := Name;

      Extended_Spec_Name : String :=
                             Name &
                             Spec_Suffix_Of (In_Tree, "ada", Data.Naming);
      Extended_Body_Name : String :=
                             Name &
                             Body_Suffix_Of (In_Tree, "ada", Data.Naming);

      Unit : Unit_Data;

      The_Original_Name : Name_Id;
      The_Spec_Name     : Name_Id;
      The_Body_Name     : Name_Id;

   begin
      Canonical_Case_File_Name (Original_Name);
      Name_Len := Original_Name'Length;
      Name_Buffer (1 .. Name_Len) := Original_Name;
      The_Original_Name := Name_Find;

      Canonical_Case_File_Name (Extended_Spec_Name);
      Name_Len := Extended_Spec_Name'Length;
      Name_Buffer (1 .. Name_Len) := Extended_Spec_Name;
      The_Spec_Name := Name_Find;

      Canonical_Case_File_Name (Extended_Body_Name);
      Name_Len := Extended_Body_Name'Length;
      Name_Buffer (1 .. Name_Len) := Extended_Body_Name;
      The_Body_Name := Name_Find;

      if Current_Verbosity = High then
         Write_Str  ("Looking for file name of """);
         Write_Str  (Name);
         Write_Char ('"');
         Write_Eol;
         Write_Str  ("   Extended Spec Name = """);
         Write_Str  (Extended_Spec_Name);
         Write_Char ('"');
         Write_Eol;
         Write_Str  ("   Extended Body Name = """);
         Write_Str  (Extended_Body_Name);
         Write_Char ('"');
         Write_Eol;
      end if;

      --  For extending project, search in the extended project if the source
      --  is not found. For non extending projects, this loop will be run only
      --  once.

      loop
         --  Loop through units
         --  Should have comment explaining reverse ???

         for Current in reverse Unit_Table.First ..
                                Unit_Table.Last (In_Tree.Units)
         loop
            Unit := In_Tree.Units.Table (Current);

            --  Check for body

            if not Main_Project_Only
              or else Unit.File_Names (Body_Part).Project = The_Project
            then
               declare
                  Current_Name : constant File_Name_Type :=
                                   Unit.File_Names (Body_Part).Name;

               begin
                  --  Case of a body present

                  if Current_Name /= No_File then
                     if Current_Verbosity = High then
                        Write_Str  ("   Comparing with """);
                        Write_Str  (Get_Name_String (Current_Name));
                        Write_Char ('"');
                        Write_Eol;
                     end if;

                     --  If it has the name of the original name, return the
                     --  original name.

                     if Unit.Name = The_Original_Name
                       or else
                         Current_Name = File_Name_Type (The_Original_Name)
                     then
                        if Current_Verbosity = High then
                           Write_Line ("   OK");
                        end if;

                        if Full_Path then
                           return Get_Name_String
                             (Unit.File_Names (Body_Part).Path.Name);

                        else
                           return Get_Name_String (Current_Name);
                        end if;

                        --  If it has the name of the extended body name,
                        --  return the extended body name

                     elsif Current_Name = File_Name_Type (The_Body_Name) then
                        if Current_Verbosity = High then
                           Write_Line ("   OK");
                        end if;

                        if Full_Path then
                           return Get_Name_String
                             (Unit.File_Names (Body_Part).Path.Name);

                        else
                           return Extended_Body_Name;
                        end if;

                     else
                        if Current_Verbosity = High then
                           Write_Line ("   not good");
                        end if;
                     end if;
                  end if;
               end;
            end if;

            --  Check for spec

            if not Main_Project_Only
              or else Unit.File_Names (Specification).Project = The_Project
            then
               declare
                  Current_Name : constant File_Name_Type :=
                                   Unit.File_Names (Specification).Name;

               begin
                  --  Case of spec present

                  if Current_Name /= No_File then
                     if Current_Verbosity = High then
                        Write_Str  ("   Comparing with """);
                        Write_Str  (Get_Name_String (Current_Name));
                        Write_Char ('"');
                        Write_Eol;
                     end if;

                     --  If name same as original name, return original name

                     if Unit.Name = The_Original_Name
                       or else
                         Current_Name = File_Name_Type (The_Original_Name)
                     then
                        if Current_Verbosity = High then
                           Write_Line ("   OK");
                        end if;

                        if Full_Path then
                           return Get_Name_String
                             (Unit.File_Names (Specification).Path.Name);
                        else
                           return Get_Name_String (Current_Name);
                        end if;

                        --  If it has the same name as the extended spec name,
                        --  return the extended spec name.

                     elsif Current_Name = File_Name_Type (The_Spec_Name) then
                        if Current_Verbosity = High then
                           Write_Line ("   OK");
                        end if;

                        if Full_Path then
                           return Get_Name_String
                             (Unit.File_Names (Specification).Path.Name);
                        else
                           return Extended_Spec_Name;
                        end if;

                     else
                        if Current_Verbosity = High then
                           Write_Line ("   not good");
                        end if;
                     end if;
                  end if;
               end;
            end if;
         end loop;

         --  If we are not in an extending project, give up

         exit when (not Main_Project_Only) or else Data.Extends = No_Project;

         --  Otherwise, look in the project we are extending

         The_Project := Data.Extends;
         Data := In_Tree.Projects.Table (The_Project);
      end loop;

      --  We don't know this file name, return an empty string

      return "";
   end File_Name_Of_Library_Unit_Body;

   -------------------------
   -- For_All_Object_Dirs --
   -------------------------

   procedure For_All_Object_Dirs
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref)
   is
      procedure For_Project (Prj : Project_Id; Dummy : in out Integer);
      --  Get all object directories of Prj

      -----------------
      -- For_Project --
      -----------------

      procedure For_Project (Prj : Project_Id; Dummy : in out Integer) is
         pragma Unreferenced (Dummy);

         Data : Project_Data renames In_Tree.Projects.Table (Prj);

      begin
         --  ??? Set_Ada_Paths has a different behavior for library project
         --  files, should we have the same ?

         if Data.Object_Directory /= No_Path_Information then
            Get_Name_String (Data.Object_Directory.Display_Name);
            Action (Name_Buffer (1 .. Name_Len));
         end if;
      end For_Project;

      procedure Get_Object_Dirs is
        new For_Every_Project_Imported (Integer, For_Project);
      Dummy : Integer := 1;

   --  Start of processing for For_All_Object_Dirs

   begin
      Get_Object_Dirs (Project, In_Tree, Dummy);
   end For_All_Object_Dirs;

   -------------------------
   -- For_All_Source_Dirs --
   -------------------------

   procedure For_All_Source_Dirs
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref)
   is
      procedure For_Project (Prj : Project_Id; Dummy : in out Integer);
      --  Get all object directories of Prj

      -----------------
      -- For_Project --
      -----------------

      procedure For_Project (Prj : Project_Id; Dummy : in out Integer) is
         pragma Unreferenced (Dummy);

         Data       : Project_Data renames In_Tree.Projects.Table (Prj);
         Current    : String_List_Id := Data.Source_Dirs;
         The_String : String_Element;

      begin
         --  If there are Ada sources, call action with the name of every
         --  source directory.

         if Has_Ada_Sources (In_Tree.Projects.Table (Project)) then
            while Current /= Nil_String loop
               The_String := In_Tree.String_Elements.Table (Current);
               Action (Get_Name_String (The_String.Display_Value));
               Current := The_String.Next;
            end loop;
         end if;
      end For_Project;

      procedure Get_Source_Dirs is
        new For_Every_Project_Imported (Integer, For_Project);
      Dummy : Integer := 1;

   --  Start of processing for For_All_Source_Dirs

   begin
      Get_Source_Dirs (Project, In_Tree, Dummy);
   end For_All_Source_Dirs;

   -------------------
   -- Get_Reference --
   -------------------

   procedure Get_Reference
     (Source_File_Name : String;
      In_Tree          : Project_Tree_Ref;
      Project          : out Project_Id;
      Path             : out Path_Name_Type)
   is
   begin
      --  Body below could use some comments ???

      if Current_Verbosity > Default then
         Write_Str ("Getting Reference_Of (""");
         Write_Str (Source_File_Name);
         Write_Str (""") ... ");
      end if;

      declare
         Original_Name : String := Source_File_Name;
         Unit          : Unit_Data;

      begin
         Canonical_Case_File_Name (Original_Name);

         for Id in Unit_Table.First ..
                   Unit_Table.Last (In_Tree.Units)
         loop
            Unit := In_Tree.Units.Table (Id);

            if (Unit.File_Names (Specification).Name /= No_File
                 and then
                   Namet.Get_Name_String
                     (Unit.File_Names (Specification).Name) = Original_Name)
              or else (Unit.File_Names (Specification).Path /=
                                                         No_Path_Information
                         and then
                           Namet.Get_Name_String
                           (Unit.File_Names (Specification).Path.Name) =
                                                              Original_Name)
            then
               Project := Ultimate_Extension_Of
                           (Project => Unit.File_Names (Specification).Project,
                            In_Tree => In_Tree);
               Path := Unit.File_Names (Specification).Path.Display_Name;

               if Current_Verbosity > Default then
                  Write_Str ("Done: Specification.");
                  Write_Eol;
               end if;

               return;

            elsif (Unit.File_Names (Body_Part).Name /= No_File
                    and then
                      Namet.Get_Name_String
                        (Unit.File_Names (Body_Part).Name) = Original_Name)
              or else (Unit.File_Names (Body_Part).Path /= No_Path_Information
                         and then Namet.Get_Name_String
                                    (Unit.File_Names (Body_Part).Path.Name) =
                                                             Original_Name)
            then
               Project := Ultimate_Extension_Of
                            (Project => Unit.File_Names (Body_Part).Project,
                             In_Tree => In_Tree);
               Path := Unit.File_Names (Body_Part).Path.Display_Name;

               if Current_Verbosity > Default then
                  Write_Str ("Done: Body.");
                  Write_Eol;
               end if;

               return;
            end if;
         end loop;
      end;

      Project := No_Project;
      Path    := No_Path;

      if Current_Verbosity > Default then
         Write_Str ("Cannot be found.");
         Write_Eol;
      end if;
   end Get_Reference;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (In_Tree : Project_Tree_Ref) is
   begin
      In_Tree.Private_Part.Fill_Mapping_File := True;
      In_Tree.Private_Part.Current_Source_Path_File := No_Path;
      In_Tree.Private_Part.Current_Object_Path_File := No_Path;
   end Initialize;

   -------------------
   -- Print_Sources --
   -------------------

   --  Could use some comments in this body ???

   procedure Print_Sources (In_Tree : Project_Tree_Ref) is
      Unit : Unit_Data;

   begin
      Write_Line ("List of Sources:");

      for Id in Unit_Table.First ..
                Unit_Table.Last (In_Tree.Units)
      loop
         Unit := In_Tree.Units.Table (Id);
         Write_Str  ("   ");
         Write_Line (Namet.Get_Name_String (Unit.Name));

         if Unit.File_Names (Specification).Name /= No_File then
            if Unit.File_Names (Specification).Project = No_Project then
               Write_Line ("   No project");

            else
               Write_Str  ("   Project: ");
               Get_Name_String
                 (In_Tree.Projects.Table
                   (Unit.File_Names (Specification).Project).Path.Name);
               Write_Line (Name_Buffer (1 .. Name_Len));
            end if;

            Write_Str  ("      spec: ");
            Write_Line
              (Namet.Get_Name_String
               (Unit.File_Names (Specification).Name));
         end if;

         if Unit.File_Names (Body_Part).Name /= No_File then
            if Unit.File_Names (Body_Part).Project = No_Project then
               Write_Line ("   No project");

            else
               Write_Str  ("   Project: ");
               Get_Name_String
                 (In_Tree.Projects.Table
                   (Unit.File_Names (Body_Part).Project).Path.Name);
               Write_Line (Name_Buffer (1 .. Name_Len));
            end if;

            Write_Str  ("      body: ");
            Write_Line
              (Namet.Get_Name_String
               (Unit.File_Names (Body_Part).Name));
         end if;
      end loop;

      Write_Line ("end of List of Sources.");
   end Print_Sources;

   ----------------
   -- Project_Of --
   ----------------

   function Project_Of
     (Name         : String;
      Main_Project : Project_Id;
      In_Tree      : Project_Tree_Ref) return Project_Id
   is
      Result : Project_Id := No_Project;

      Original_Name : String := Name;

      Data   : constant Project_Data :=
        In_Tree.Projects.Table (Main_Project);

      Extended_Spec_Name : String :=
                             Name &
                             Spec_Suffix_Of (In_Tree, "ada", Data.Naming);
      Extended_Body_Name : String :=
                             Name &
                             Body_Suffix_Of (In_Tree, "ada", Data.Naming);

      Unit : Unit_Data;

      Current_Name      : File_Name_Type;
      The_Original_Name : File_Name_Type;
      The_Spec_Name     : File_Name_Type;
      The_Body_Name     : File_Name_Type;

   begin
      Canonical_Case_File_Name (Original_Name);
      Name_Len := Original_Name'Length;
      Name_Buffer (1 .. Name_Len) := Original_Name;
      The_Original_Name := Name_Find;

      Canonical_Case_File_Name (Extended_Spec_Name);
      Name_Len := Extended_Spec_Name'Length;
      Name_Buffer (1 .. Name_Len) := Extended_Spec_Name;
      The_Spec_Name := Name_Find;

      Canonical_Case_File_Name (Extended_Body_Name);
      Name_Len := Extended_Body_Name'Length;
      Name_Buffer (1 .. Name_Len) := Extended_Body_Name;
      The_Body_Name := Name_Find;

      for Current in reverse Unit_Table.First ..
                             Unit_Table.Last (In_Tree.Units)
      loop
         Unit := In_Tree.Units.Table (Current);

         --  Check for body

         Current_Name := Unit.File_Names (Body_Part).Name;

         --  Case of a body present

         if Current_Name /= No_File then

            --  If it has the name of the original name or the body name,
            --  we have found the project.

            if Unit.Name = Name_Id (The_Original_Name)
              or else Current_Name = The_Original_Name
              or else Current_Name = The_Body_Name
            then
               Result := Unit.File_Names (Body_Part).Project;
               exit;
            end if;
         end if;

         --  Check for spec

         Current_Name := Unit.File_Names (Specification).Name;

         if Current_Name /= No_File then

            --  If name same as the original name, or the spec name, we have
            --  found the project.

            if Unit.Name = Name_Id (The_Original_Name)
              or else Current_Name = The_Original_Name
              or else Current_Name = The_Spec_Name
            then
               Result := Unit.File_Names (Specification).Project;
               exit;
            end if;
         end if;
      end loop;

      --  Get the ultimate extending project

      if Result /= No_Project then
         while In_Tree.Projects.Table (Result).Extended_By /=
           No_Project
         loop
            Result := In_Tree.Projects.Table (Result).Extended_By;
         end loop;
      end if;

      return Result;
   end Project_Of;

   -------------------
   -- Set_Ada_Paths --
   -------------------

   procedure Set_Ada_Paths
     (Project             : Project_Id;
      In_Tree             : Project_Tree_Ref;
      Including_Libraries : Boolean)

   is
      Source_FD : File_Descriptor := Invalid_FD;
      Object_FD : File_Descriptor := Invalid_FD;

      Process_Source_Dirs : Boolean := False;
      Process_Object_Dirs : Boolean := False;

      Status : Boolean;
      --  For calls to Close

      Len : Natural;

      procedure Recursive_Add (Project : Project_Id; Dummy : in out Boolean);
      --  Recursive procedure to add the source/object paths of extended/
      --  imported projects.

      -------------------
      -- Recursive_Add --
      -------------------

      procedure Recursive_Add (Project : Project_Id; Dummy : in out Boolean) is
         pragma Unreferenced (Dummy);

         Data : constant Project_Data := In_Tree.Projects.Table (Project);
         Path : Path_Name_Type;

      begin
         --  ??? This is almost the equivalent of For_All_Source_Dirs

         if Process_Source_Dirs then

            --  Add to path all source directories of this project if there are
            --  Ada sources.

            if Has_Ada_Sources (In_Tree.Projects.Table (Project)) then
               Add_To_Source_Path (Data.Source_Dirs, In_Tree);
            end if;
         end if;

         if Process_Object_Dirs then
            Path := Get_Object_Directory
              (In_Tree, Project,
               Including_Libraries => Including_Libraries,
               Only_If_Ada         => True);

            if Path /= No_Path then
               Add_To_Object_Path (Path, In_Tree);
            end if;
         end if;
      end Recursive_Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Recursive_Add);
      Dummy : Boolean := False;

   --  Start of processing for Set_Ada_Paths

   begin
      --  If it is the first time we call this procedure for this project,
      --  compute the source path and/or the object path.

      if In_Tree.Projects.Table (Project).Include_Path_File = No_Path then
         Process_Source_Dirs := True;
         Create_New_Path_File
           (In_Tree, Source_FD,
            In_Tree.Projects.Table (Project).Include_Path_File);
      end if;

      --  For the object path, we make a distinction depending on
      --  Including_Libraries.

      if Including_Libraries then
         if In_Tree.Projects.Table
           (Project).Objects_Path_File_With_Libs = No_Path
         then
            Process_Object_Dirs := True;
            Create_New_Path_File
              (In_Tree, Object_FD, In_Tree.Projects.Table (Project).
                                           Objects_Path_File_With_Libs);
         end if;

      else
         if In_Tree.Projects.Table
              (Project).Objects_Path_File_Without_Libs = No_Path
         then
            Process_Object_Dirs := True;
            Create_New_Path_File
              (In_Tree, Object_FD, In_Tree.Projects.Table (Project).
                                           Objects_Path_File_Without_Libs);
         end if;
      end if;

      --  If there is something to do, set Seen to False for all projects,
      --  then call the recursive procedure Add for Project.

      if Process_Source_Dirs or Process_Object_Dirs then
         Source_Path_Table.Set_Last (In_Tree.Private_Part.Source_Paths, 0);
         Object_Path_Table.Set_Last (In_Tree.Private_Part.Object_Paths, 0);
         For_All_Projects (Project, In_Tree, Dummy);
      end if;

      --  Write and close any file that has been created

      if Source_FD /= Invalid_FD then
         for Index in Source_Path_Table.First ..
                      Source_Path_Table.Last
                        (In_Tree.Private_Part.Source_Paths)
         loop
            Get_Name_String (In_Tree.Private_Part.Source_Paths.Table (Index));
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := ASCII.LF;
            Len := Write (Source_FD, Name_Buffer (1)'Address, Name_Len);

            if Len /= Name_Len then
               Prj.Com.Fail ("disk full");
            end if;
         end loop;

         Close (Source_FD, Status);

         if not Status then
            Prj.Com.Fail ("disk full");
         end if;
      end if;

      if Object_FD /= Invalid_FD then
         for Index in Object_Path_Table.First ..
                      Object_Path_Table.Last
                        (In_Tree.Private_Part.Object_Paths)
         loop
            Get_Name_String (In_Tree.Private_Part.Object_Paths.Table (Index));
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := ASCII.LF;
            Len := Write (Object_FD, Name_Buffer (1)'Address, Name_Len);

            if Len /= Name_Len then
               Prj.Com.Fail ("disk full");
            end if;
         end loop;

         Close (Object_FD, Status);

         if not Status then
            Prj.Com.Fail ("disk full");
         end if;
      end if;

      --  Set the env vars, if they need to be changed, and set the
      --  corresponding flags.

      if In_Tree.Private_Part.Current_Source_Path_File /=
           In_Tree.Projects.Table (Project).Include_Path_File
      then
         In_Tree.Private_Part.Current_Source_Path_File :=
           In_Tree.Projects.Table (Project).Include_Path_File;
         Set_Path_File_Var
           (Project_Include_Path_File,
            Get_Name_String (In_Tree.Private_Part.Current_Source_Path_File));
         In_Tree.Private_Part.Ada_Prj_Include_File_Set := True;
      end if;

      if Including_Libraries then
         if In_Tree.Private_Part.Current_Object_Path_File /=
            In_Tree.Projects.Table (Project).Objects_Path_File_With_Libs
         then
            In_Tree.Private_Part.Current_Object_Path_File :=
              In_Tree.Projects.Table
                (Project).Objects_Path_File_With_Libs;
            Set_Path_File_Var
              (Project_Objects_Path_File,
               Get_Name_String
                 (In_Tree.Private_Part.Current_Object_Path_File));
            In_Tree.Private_Part.Ada_Prj_Objects_File_Set := True;
         end if;

      else
         if In_Tree.Private_Part.Current_Object_Path_File /=
            In_Tree.Projects.Table (Project).Objects_Path_File_Without_Libs
         then
            In_Tree.Private_Part.Current_Object_Path_File :=
              In_Tree.Projects.Table
                (Project).Objects_Path_File_Without_Libs;
            Set_Path_File_Var
              (Project_Objects_Path_File,
               Get_Name_String
                 (In_Tree.Private_Part.Current_Object_Path_File));
            In_Tree.Private_Part.Ada_Prj_Objects_File_Set := True;
         end if;
      end if;
   end Set_Ada_Paths;

   ---------------------------------------------
   -- Set_Mapping_File_Initial_State_To_Empty --
   ---------------------------------------------

   procedure Set_Mapping_File_Initial_State_To_Empty
     (In_Tree : Project_Tree_Ref)
   is
   begin
      In_Tree.Private_Part.Fill_Mapping_File := False;
   end Set_Mapping_File_Initial_State_To_Empty;

   -----------------------
   -- Set_Path_File_Var --
   -----------------------

   procedure Set_Path_File_Var (Name : String; Value : String) is
      Host_Spec : String_Access := To_Host_File_Spec (Value);
   begin
      if Host_Spec = null then
         Prj.Com.Fail
           ("could not convert file name """ & Value & """ to host spec");
      else
         Setenv (Name, Host_Spec.all);
         Free (Host_Spec);
      end if;
   end Set_Path_File_Var;

   ---------------------------
   -- Ultimate_Extension_Of --
   ---------------------------

   function Ultimate_Extension_Of
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return Project_Id
   is
      Result : Project_Id := Project;

   begin
      while In_Tree.Projects.Table (Result).Extended_By /= No_Project loop
         Result := In_Tree.Projects.Table (Result).Extended_By;
      end loop;

      return Result;
   end Ultimate_Extension_Of;

end Prj.Env;
