------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E N V                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2003 Free Software Foundation, Inc.          --
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

with Namet;    use Namet;
with Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Com;  use Prj.Com;
with Table;
with Tempdir;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Prj.Env is

   type Naming_Id is new Nat;

   Current_Source_Path_File : Name_Id := No_Name;
   --  Current value of project source path file env var.
   --  Used to avoid setting the env var to the same value.

   Current_Object_Path_File : Name_Id := No_Name;
   --  Current value of project object path file env var.
   --  Used to avoid setting the env var to the same value.

   Ada_Path_Buffer : String_Access := new String (1 .. 1024);
   --  A buffer where values for ADA_INCLUDE_PATH
   --  and ADA_OBJECTS_PATH are stored.

   Ada_Path_Length : Natural := 0;
   --  Index of the last valid character in Ada_Path_Buffer.

   Ada_Prj_Include_File_Set : Boolean := False;
   Ada_Prj_Objects_File_Set : Boolean := False;
   --  These flags are set to True when the corresponding environment variables
   --  are set and are used to give these environment variables an empty string
   --  value at the end of the program. This has no practical effect on most
   --  platforms, except on VMS where the logical names are deassigned, thus
   --  avoiding the pollution of the environment of the caller.

   package Namings is new Table.Table (
     Table_Component_Type => Naming_Data,
     Table_Index_Type     => Naming_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 5,
     Table_Increment      => 100,
     Table_Name           => "Prj.Env.Namings");

   Default_Naming : constant Naming_Id := Namings.First;

   Fill_Mapping_File : Boolean := True;

   package Path_Files is new Table.Table (
     Table_Component_Type => Name_Id,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 50,
     Table_Increment      => 50,
     Table_Name           => "Prj.Env.Path_Files");
   --  Table storing all the temp path file names.
   --  Used by Delete_All_Path_Files.

   type Project_Flags is array (Project_Id range <>) of Boolean;
   --  A Boolean array type used in Create_Mapping_File to select the projects
   --  in the closure of a specific project.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Body_Path_Name_Of (Unit : Unit_Id) return String;
   --  Returns the path name of the body of a unit.
   --  Compute it first, if necessary.

   function Spec_Path_Name_Of (Unit : Unit_Id) return String;
   --  Returns the path name of the spec of a unit.
   --  Compute it first, if necessary.

   procedure Add_To_Path (Source_Dirs : String_List_Id);
   --  Add to Ada_Path_Buffer all the source directories in string list
   --  Source_Dirs, if any. Increment Ada_Path_Length.

   procedure Add_To_Path (Dir : String);
   --  If Dir is not already in the global variable Ada_Path_Buffer, add it.
   --  Increment Ada_Path_Length.
   --  If Ada_Path_Length /= 0, prepend a Path_Separator character to
   --  Path.

   procedure Add_To_Path_File
     (Source_Dirs : String_List_Id;
      Path_File   : File_Descriptor);
   --  Add to Ada_Path_Buffer all the source directories in string list
   --  Source_Dirs, if any. Increment Ada_Path_Length.

   procedure Add_To_Path_File
     (Path      : String;
      Path_File : File_Descriptor);
   --  Add Path to path file

   procedure Create_New_Path_File
     (Path_FD   : out File_Descriptor;
      Path_Name : out Name_Id);
   --  Create a new temporary path file. Get the file name in Path_Name.
   --  The name is normally obtained by increasing the number in
   --  Temp_Path_File_Name by 1.

   procedure Set_Path_File_Var (Name : String; Value : String);
   --  Call Setenv, after calling To_Host_File_Spec

   function Ultimate_Extension_Of (Project : in Project_Id) return Project_Id;
   --  Return a project that is either Project or an extended ancestor of
   --  Project that itself is not extended.

   ----------------------
   -- Ada_Include_Path --
   ----------------------

   function Ada_Include_Path (Project : Project_Id) return String_Access is

      procedure Add (Project : Project_Id);
      --  Add all the source directories of a project to the path only if
      --  this project has not been visited. Calls itself recursively for
      --  projects being extended, and imported projects. Adds the project
      --  to the list Seen if this is the call to Add for this project.

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id) is
      begin
         --  If Seen is empty, then the project cannot have been visited

         if not Projects.Table (Project).Seen then
            Projects.Table (Project).Seen := True;

            declare
               Data : constant Project_Data := Projects.Table (Project);
               List : Project_List := Data.Imported_Projects;

            begin
               --  Add to path all source directories of this project

               Add_To_Path (Data.Source_Dirs);

               --  Call Add to the project being extended, if any

               if Data.Extends /= No_Project then
                  Add (Data.Extends);
               end if;

               --  Call Add for each imported project, if any

               while List /= Empty_Project_List loop
                  Add (Project_Lists.Table (List).Project);
                  List := Project_Lists.Table (List).Next;
               end loop;
            end;
         end if;
      end Add;

   --  Start of processing for Ada_Include_Path

   begin
      --  If it is the first time we call this function for
      --  this project, compute the source path

      if Projects.Table (Project).Ada_Include_Path = null then
         Ada_Path_Length := 0;

         for Index in 1 .. Projects.Last loop
            Projects.Table (Index).Seen := False;
         end loop;

         Add (Project);
         Projects.Table (Project).Ada_Include_Path :=
           new String'(Ada_Path_Buffer (1 .. Ada_Path_Length));
      end if;

      return Projects.Table (Project).Ada_Include_Path;
   end Ada_Include_Path;

   ----------------------
   -- Ada_Include_Path --
   ----------------------

   function Ada_Include_Path
     (Project   : Project_Id;
      Recursive : Boolean) return String
   is
   begin
      if Recursive then
         return Ada_Include_Path (Project).all;
      else
         Ada_Path_Length := 0;
         Add_To_Path (Projects.Table (Project).Source_Dirs);
         return Ada_Path_Buffer (1 .. Ada_Path_Length);
      end if;
   end Ada_Include_Path;

   ----------------------
   -- Ada_Objects_Path --
   ----------------------

   function Ada_Objects_Path
     (Project             : Project_Id;
      Including_Libraries : Boolean := True) return String_Access
   is
      procedure Add (Project : Project_Id);
      --  Add all the object directories of a project to the path only if
      --  this project has not been visited. Calls itself recursively for
      --  projects being extended, and imported projects. Adds the project
      --  to the list Seen if this is the first call to Add for this project.

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id) is
      begin
         --  If this project has not been seen yet

         if not Projects.Table (Project).Seen then
            Projects.Table (Project).Seen := True;

            declare
               Data : constant Project_Data := Projects.Table (Project);
               List : Project_List := Data.Imported_Projects;

            begin
               --  Add to path the object directory of this project
               --  except if we don't include library project and
               --  this is a library project.

               if (Data.Library and then Including_Libraries)
                 or else
                 (Data.Object_Directory /= No_Name
                   and then
                   (not Including_Libraries or else not Data.Library))
               then
                  --  For a library project, add the library directory

                  if Data.Library then
                     Add_To_Path (Get_Name_String (Data.Library_Dir));

                  else
                     --  For a non library project, add the object directory

                     Add_To_Path (Get_Name_String (Data.Object_Directory));
                  end if;
               end if;

               --  Call Add to the project being extended, if any

               if Data.Extends /= No_Project then
                  Add (Data.Extends);
               end if;

               --  Call Add for each imported project, if any

               while List /= Empty_Project_List loop
                  Add (Project_Lists.Table (List).Project);
                  List := Project_Lists.Table (List).Next;
               end loop;
            end;

         end if;
      end Add;

   --  Start of processing for Ada_Objects_Path

   begin
      --  If it is the first time we call this function for
      --  this project, compute the objects path

      if Projects.Table (Project).Ada_Objects_Path = null then
         Ada_Path_Length := 0;

         for Index in 1 .. Projects.Last loop
            Projects.Table (Index).Seen := False;
         end loop;

         Add (Project);
         Projects.Table (Project).Ada_Objects_Path :=
           new String'(Ada_Path_Buffer (1 .. Ada_Path_Length));
      end if;

      return Projects.Table (Project).Ada_Objects_Path;
   end Ada_Objects_Path;

   -----------------
   -- Add_To_Path --
   -----------------

   procedure Add_To_Path (Source_Dirs : String_List_Id) is
      Current    : String_List_Id := Source_Dirs;
      Source_Dir : String_Element;

   begin
      while Current /= Nil_String loop
         Source_Dir := String_Elements.Table (Current);
         Add_To_Path (Get_Name_String (Source_Dir.Value));
         Current := Source_Dir.Next;
      end loop;
   end Add_To_Path;

   procedure Add_To_Path (Dir : String) is
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

   begin
      if Is_Present (Ada_Path_Buffer (1 .. Ada_Path_Length), Dir) then
         --  Dir is already in the path, nothing to do

         return;
      end if;

      Min_Len := Ada_Path_Length + Dir'Length;

      if Ada_Path_Length > 0 then
         --  Add 1 for the Path_Separator character

         Min_Len := Min_Len + 1;
      end if;

      --  If Ada_Path_Buffer is too small, increase it

      Len := Ada_Path_Buffer'Last;

      if Len < Min_Len then
         loop
            Len := Len * 2;
            exit when Len >= Min_Len;
         end loop;

         New_Buffer := new String (1 .. Len);
         New_Buffer (1 .. Ada_Path_Length) :=
           Ada_Path_Buffer (1 .. Ada_Path_Length);
         Free (Ada_Path_Buffer);
         Ada_Path_Buffer := New_Buffer;
      end if;

      if Ada_Path_Length > 0 then
         Ada_Path_Length := Ada_Path_Length + 1;
         Ada_Path_Buffer (Ada_Path_Length) := Path_Separator;
      end if;

      Ada_Path_Buffer
        (Ada_Path_Length + 1 .. Ada_Path_Length + Dir'Length) := Dir;
      Ada_Path_Length := Ada_Path_Length + Dir'Length;
   end Add_To_Path;

   ----------------------
   -- Add_To_Path_File --
   ----------------------

   procedure Add_To_Path_File
     (Source_Dirs : String_List_Id;
      Path_File   : File_Descriptor)
   is
      Current    : String_List_Id := Source_Dirs;
      Source_Dir : String_Element;

   begin
      while Current /= Nil_String loop
         Source_Dir := String_Elements.Table (Current);
         Add_To_Path_File (Get_Name_String (Source_Dir.Value), Path_File);
         Current := Source_Dir.Next;
      end loop;
   end Add_To_Path_File;

   procedure Add_To_Path_File
     (Path      : String;
      Path_File : File_Descriptor)
   is
      Line : String (1 .. Path'Length + 1);
      Len  : Natural;

   begin
      Line (1 .. Path'Length) := Path;
      Line (Line'Last) := ASCII.LF;
      Len := Write (Path_File, Line (1)'Address, Line'Length);

      if Len /= Line'Length then
         Prj.Com.Fail ("disk full");
      end if;
   end Add_To_Path_File;

   -----------------------
   -- Body_Path_Name_Of --
   -----------------------

   function Body_Path_Name_Of (Unit : Unit_Id) return String is
      Data : Unit_Data := Units.Table (Unit);

   begin
      --  If we don't know the path name of the body of this unit,
      --  we compute it, and we store it.

      if Data.File_Names (Body_Part).Path = No_Name then
         declare
            Current_Source : String_List_Id :=
              Projects.Table (Data.File_Names (Body_Part).Project).Sources;
            Path : GNAT.OS_Lib.String_Access;

         begin
            --  By default, put the file name

            Data.File_Names (Body_Part).Path :=
              Data.File_Names (Body_Part).Name;

            --  For each source directory

            while Current_Source /= Nil_String loop
               Path :=
                 Locate_Regular_File
                   (Namet.Get_Name_String
                      (Data.File_Names (Body_Part).Name),
                    Namet.Get_Name_String
                       (String_Elements.Table (Current_Source).Value));

               --  If the file is in this directory,
               --  then we store the path, and we are done.

               if Path /= null then
                  Name_Len := Path'Length;
                  Name_Buffer (1 .. Name_Len) := Path.all;
                  Data.File_Names (Body_Part).Path := Name_Enter;
                  exit;

               else
                  Current_Source :=
                    String_Elements.Table (Current_Source).Next;
               end if;
            end loop;

            Units.Table (Unit) := Data;
         end;
      end if;

      --  Returned the value stored

      return Namet.Get_Name_String (Data.File_Names (Body_Part).Path);
   end Body_Path_Name_Of;

   --------------------------------
   -- Create_Config_Pragmas_File --
   --------------------------------

   procedure Create_Config_Pragmas_File
     (For_Project          : Project_Id;
      Main_Project         : Project_Id;
      Include_Config_Files : Boolean := True)
   is
      pragma Unreferenced (Main_Project);
      pragma Unreferenced (Include_Config_Files);

      File_Name : Name_Id         := No_Name;
      File      : File_Descriptor := Invalid_FD;

      Current_Unit : Unit_Id := Units.First;

      First_Project : Project_List := Empty_Project_List;

      Current_Project : Project_List;
      Current_Naming  : Naming_Id;

      Status : Boolean;
      --  For call to Close

      procedure Check (Project : Project_Id);

      procedure Check_Temp_File;
      --  Check that a temporary file has been opened.
      --  If not, create one, and put its name in the project data,
      --  with the indication that it is a temporary file.

      procedure Put
        (Unit_Name : Name_Id;
         File_Name : Name_Id;
         Unit_Kind : Spec_Or_Body);
      --  Put an SFN pragma in the temporary file.

      procedure Put (File : File_Descriptor; S : String);

      procedure Put_Line (File : File_Descriptor; S : String);

      -----------
      -- Check --
      -----------

      procedure Check (Project : Project_Id) is
         Data : constant Project_Data := Projects.Table (Project);

      begin
         if Current_Verbosity = High then
            Write_Str ("Checking project file """);
            Write_Str (Namet.Get_Name_String (Data.Name));
            Write_Str (""".");
            Write_Eol;
         end if;

         --  Is this project in the list of the visited project?

         Current_Project := First_Project;
         while Current_Project /= Empty_Project_List
           and then Project_Lists.Table (Current_Project).Project /= Project
         loop
            Current_Project := Project_Lists.Table (Current_Project).Next;
         end loop;

         --  If it is not, put it in the list, and visit it

         if Current_Project = Empty_Project_List then
            Project_Lists.Increment_Last;
            Project_Lists.Table (Project_Lists.Last) :=
              (Project => Project, Next => First_Project);
            First_Project := Project_Lists.Last;

            --  Is the naming scheme of this project one that we know?

            Current_Naming := Default_Naming;
            while Current_Naming <= Namings.Last and then
              not Same_Naming_Scheme
              (Left => Namings.Table (Current_Naming),
               Right => Data.Naming) loop
               Current_Naming := Current_Naming + 1;
            end loop;

            --  If we don't know it, add it

            if Current_Naming > Namings.Last then
               Namings.Increment_Last;
               Namings.Table (Namings.Last) := Data.Naming;

               --  We need a temporary file to be created

               Check_Temp_File;

               --  Put the SFN pragmas for the naming scheme

               --  Spec

               Put_Line
                 (File, "pragma Source_File_Name_Project");
               Put_Line
                 (File, "  (Spec_File_Name  => ""*" &
                  Namet.Get_Name_String (Data.Naming.Current_Spec_Suffix) &
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
                  Namet.Get_Name_String (Data.Naming.Current_Body_Suffix) &
                  """,");
               Put_Line
                 (File, "   Casing          => " &
                  Image (Data.Naming.Casing) & ",");
               Put_Line
                 (File, "   Dot_Replacement => """ &
                  Namet.Get_Name_String (Data.Naming.Dot_Replacement) &
                  """);");

               --  and maybe separate

               if
                 Data.Naming.Current_Body_Suffix /= Data.Naming.Separate_Suffix
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
               while Current /= Empty_Project_List loop
                  Check (Project_Lists.Table (Current).Project);
                  Current := Project_Lists.Table (Current).Next;
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
            elsif Opt.Verbose_Mode then
               Write_Str ("Creating temp file """);
               Write_Str (Get_Name_String (File_Name));
               Write_Line ("""");
            end if;
         end if;
      end Check_Temp_File;

      ---------
      -- Put --
      ---------

      procedure Put
        (Unit_Name : Name_Id;
         File_Name : Name_Id;
         Unit_Kind : Spec_Or_Body)
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
         Put_Line (File, """);");
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
         --  Add an ASCII.LF to the string. As this gnat.adc is supposed to
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
      if not Projects.Table (For_Project).Config_Checked then

         --  Remove any memory of processed naming schemes, if any

         Namings.Set_Last (Default_Naming);

         --  Check the naming schemes

         Check (For_Project);

         --  Visit all the units and process those that need an SFN pragma

         while Current_Unit <= Units.Last loop
            declare
               Unit : constant Unit_Data :=
                 Units.Table (Current_Unit);

            begin
               if Unit.File_Names (Specification).Needs_Pragma then
                  Put (Unit.Name,
                       Unit.File_Names (Specification).Name,
                       Specification);
               end if;

               if Unit.File_Names (Body_Part).Needs_Pragma then
                  Put (Unit.Name,
                       Unit.File_Names (Body_Part).Name,
                       Body_Part);
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

         Projects.Table (For_Project).Config_File_Name := File_Name;
         Projects.Table (For_Project).Config_File_Temp := True;

         Projects.Table (For_Project).Config_Checked := True;
      end if;
   end Create_Config_Pragmas_File;

   -------------------------
   -- Create_Mapping_File --
   -------------------------

   procedure Create_Mapping_File
     (Project : Project_Id;
      Name    : out Name_Id)
   is
      File          : File_Descriptor := Invalid_FD;
      The_Unit_Data : Unit_Data;
      Data          : File_Name_Data;

      Status : Boolean;
      --  For call to Close

      Present : Project_Flags (No_Project .. Projects.Last) :=
        (others => False);
      --  For each project in the closure of Project, the corresponding flag
      --  will be set to True;

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
            Prj.Com.Fail ("Disk full");
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

         Get_Name_String (Data.Path);
         Put_Name_Buffer;

      end Put_Data;

      --------------------
      -- Recursive_Flag --
      --------------------

      procedure Recursive_Flag (Prj : Project_Id) is
         Imported : Project_List;
         Proj     : Project_Id;

      begin
         --  Nothing to do for non existent project or project that has
         --  already been flagged.

         if Prj = No_Project or else Present (Prj) then
            return;
         end if;

         --  Flag the current project

         Present (Prj) := True;
         Imported := Projects.Table (Prj).Imported_Projects;

         --  Call itself for each project directly imported

         while Imported /= Empty_Project_List loop
            Proj := Project_Lists.Table (Imported).Project;
            Imported := Project_Lists.Table (Imported).Next;
            Recursive_Flag (Proj);
         end loop;

         --  Call itself for an eventual project being extended

         Recursive_Flag (Projects.Table (Prj).Extends);
      end Recursive_Flag;

   --  Start of processing for Create_Mapping_File

   begin
      --  Flag the necessary projects

      Recursive_Flag (Project);

      --  Create the temporary file

      Tempdir.Create_Temp_File (File, Name => Name);

      if File = Invalid_FD then
         Prj.Com.Fail ("unable to create temporary mapping file");

      elsif Opt.Verbose_Mode then
         Write_Str ("Creating temp mapping file """);
         Write_Str (Get_Name_String (Name));
         Write_Line ("""");
      end if;

      if Fill_Mapping_File then
         --  For all units in table Units

         for Unit in 1 .. Units.Last loop
            The_Unit_Data := Units.Table (Unit);

            --  If the unit has a valid name

            if The_Unit_Data.Name /= No_Name then
               Data := The_Unit_Data.File_Names (Specification);

               --  If there is a spec, put it mapping in the file if it is
               --  from a project in the closure of Project.

               if Data.Name /= No_Name and then Present (Data.Project) then
                  Put_Data (Spec => True);
               end if;

               Data := The_Unit_Data.File_Names (Body_Part);

               --  If there is a body (or subunit) put its mapping in the file
               --  if it is from a project in the closure of Project.

               if Data.Name /= No_Name and then Present (Data.Project) then
                  Put_Data (Spec => False);
               end if;

            end if;
         end loop;
      end if;

      GNAT.OS_Lib.Close (File, Status);

      if not Status then
         Prj.Com.Fail ("disk full");
      end if;

   end Create_Mapping_File;

   --------------------------
   -- Create_New_Path_File --
   --------------------------

   procedure Create_New_Path_File
     (Path_FD   : out File_Descriptor;
      Path_Name : out Name_Id)
   is
   begin
      Tempdir.Create_Temp_File (Path_FD, Path_Name);

      if Path_Name /= No_Name then

         --  Record the name, so that the temp path file will be deleted
         --  at the end of the program.

         Path_Files.Increment_Last;
         Path_Files.Table (Path_Files.Last) := Path_Name;
      end if;
   end Create_New_Path_File;

   ---------------------------
   -- Delete_All_Path_Files --
   ---------------------------

   procedure Delete_All_Path_Files is
      Disregard : Boolean := True;

   begin
      for Index in 1 .. Path_Files.Last loop
         if Path_Files.Table (Index) /= No_Name then
            Delete_File
              (Get_Name_String (Path_Files.Table (Index)), Disregard);
         end if;
      end loop;

      --  If any of the environment variables ADA_PRJ_INCLUDE_FILE or
      --  ADA_PRJ_OBJECTS_FILE has been set, then reset their value to
      --  the empty string. On VMS, this has the effect of deassigning
      --  the logical names.

      if Ada_Prj_Include_File_Set then
         Setenv (Project_Include_Path_File, "");
         Ada_Prj_Include_File_Set := False;
      end if;

      if Ada_Prj_Objects_File_Set then
         Setenv (Project_Objects_Path_File, "");
         Ada_Prj_Objects_File_Set := False;
      end if;
   end Delete_All_Path_Files;

   ------------------------------------
   -- File_Name_Of_Library_Unit_Body --
   ------------------------------------

   function File_Name_Of_Library_Unit_Body
     (Name              : String;
      Project           : Project_Id;
      Main_Project_Only : Boolean := True;
      Full_Path         : Boolean := False) return String
   is
      The_Project   : Project_Id := Project;
      Data          : Project_Data := Projects.Table (Project);
      Original_Name : String := Name;

      Extended_Spec_Name : String :=
                             Name & Namet.Get_Name_String
                                      (Data.Naming.Current_Spec_Suffix);
      Extended_Body_Name : String :=
                             Name & Namet.Get_Name_String
                                      (Data.Naming.Current_Body_Suffix);

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

      --  For extending project, search in the extended project
      --  if the source is not found. For non extending projects,
      --  this loop will be run only once.

      loop
         --  For every unit

         for Current in reverse Units.First .. Units.Last loop
            Unit := Units.Table (Current);

            --  Check for body

            if not Main_Project_Only
              or else Unit.File_Names (Body_Part).Project = The_Project
            then
               declare
                  Current_Name : constant Name_Id :=
                    Unit.File_Names (Body_Part).Name;

               begin
                  --  Case of a body present

                  if Current_Name /= No_Name then
                     if Current_Verbosity = High then
                        Write_Str  ("   Comparing with """);
                        Write_Str  (Get_Name_String (Current_Name));
                        Write_Char ('"');
                        Write_Eol;
                     end if;

                     --  If it has the name of the original name,
                     --  return the original name

                     if Unit.Name = The_Original_Name
                       or else Current_Name = The_Original_Name
                     then
                        if Current_Verbosity = High then
                           Write_Line ("   OK");
                        end if;

                        if Full_Path then
                           return Get_Name_String
                             (Unit.File_Names (Body_Part).Path);

                        else
                           return Get_Name_String (Current_Name);
                        end if;

                        --  If it has the name of the extended body name,
                        --  return the extended body name

                     elsif Current_Name = The_Body_Name then
                        if Current_Verbosity = High then
                           Write_Line ("   OK");
                        end if;

                        if Full_Path then
                           return Get_Name_String
                             (Unit.File_Names (Body_Part).Path);

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
                  Current_Name : constant Name_Id :=
                    Unit.File_Names (Specification).Name;

               begin
                  --  Case of spec present

                  if Current_Name /= No_Name then
                     if Current_Verbosity = High then
                        Write_Str  ("   Comparing with """);
                        Write_Str  (Get_Name_String (Current_Name));
                        Write_Char ('"');
                        Write_Eol;
                     end if;

                     --  If name same as the original name, return original
                     --  name.

                     if Unit.Name = The_Original_Name
                       or else Current_Name = The_Original_Name
                     then
                        if Current_Verbosity = High then
                           Write_Line ("   OK");
                        end if;


                        if Full_Path then
                           return Get_Name_String
                             (Unit.File_Names (Specification).Path);

                        else
                           return Get_Name_String (Current_Name);
                        end if;

                        --  If it has the same name as the extended spec name,
                        --  return the extended spec name.

                     elsif Current_Name = The_Spec_Name then
                        if Current_Verbosity = High then
                           Write_Line ("   OK");
                        end if;

                        if Full_Path then
                           return Get_Name_String
                             (Unit.File_Names (Specification).Path);

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
         Data := Projects.Table (The_Project);
      end loop;

      --  We don't know this file name, return an empty string

      return "";
   end File_Name_Of_Library_Unit_Body;

   -------------------------
   -- For_All_Object_Dirs --
   -------------------------

   procedure For_All_Object_Dirs (Project : Project_Id) is
      Seen : Project_List := Empty_Project_List;

      procedure Add (Project : Project_Id);
      --  Process a project. Remember the processes visited to avoid
      --  processing a project twice. Recursively process an eventual
      --  extended project, and all imported projects.

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id) is
         Data : constant Project_Data := Projects.Table (Project);
         List : Project_List := Data.Imported_Projects;

      begin
         --  If the list of visited project is empty, then
         --  for sure we never visited this project.

         if Seen = Empty_Project_List then
            Project_Lists.Increment_Last;
            Seen := Project_Lists.Last;
            Project_Lists.Table (Seen) :=
              (Project => Project, Next => Empty_Project_List);

         else
            --  Check if the project is in the list

            declare
               Current : Project_List := Seen;

            begin
               loop
                  --  If it is, then there is nothing else to do

                  if Project_Lists.Table (Current).Project = Project then
                     return;
                  end if;

                  exit when Project_Lists.Table (Current).Next =
                    Empty_Project_List;
                  Current := Project_Lists.Table (Current).Next;
               end loop;

               --  This project has never been visited, add it
               --  to the list.

               Project_Lists.Increment_Last;
               Project_Lists.Table (Current).Next := Project_Lists.Last;
               Project_Lists.Table (Project_Lists.Last) :=
                 (Project => Project, Next => Empty_Project_List);
            end;
         end if;

         --  If there is an object directory, call Action
         --  with its name

         if Data.Object_Directory /= No_Name then
            Get_Name_String (Data.Object_Directory);
            Action (Name_Buffer (1 .. Name_Len));
         end if;

         --  If we are extending a project, visit it

         if Data.Extends /= No_Project then
            Add (Data.Extends);
         end if;

         --  And visit all imported projects

         while List /= Empty_Project_List loop
            Add (Project_Lists.Table (List).Project);
            List := Project_Lists.Table (List).Next;
         end loop;
      end Add;

   --  Start of processing for For_All_Object_Dirs

   begin
      --  Visit this project, and its imported projects,
      --  recursively

      Add (Project);
   end For_All_Object_Dirs;

   -------------------------
   -- For_All_Source_Dirs --
   -------------------------

   procedure For_All_Source_Dirs (Project : Project_Id) is
      Seen : Project_List := Empty_Project_List;

      procedure Add (Project : Project_Id);
      --  Process a project. Remember the processes visited to avoid
      --  processing a project twice. Recursively process an eventual
      --  extended project, and all imported projects.

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id) is
         Data : constant Project_Data := Projects.Table (Project);
         List : Project_List := Data.Imported_Projects;

      begin
         --  If the list of visited project is empty, then
         --  for sure we never visited this project.

         if Seen = Empty_Project_List then
            Project_Lists.Increment_Last;
            Seen := Project_Lists.Last;
            Project_Lists.Table (Seen) :=
              (Project => Project, Next => Empty_Project_List);

         else
            --  Check if the project is in the list

            declare
               Current : Project_List := Seen;

            begin
               loop
                  --  If it is, then there is nothing else to do

                  if Project_Lists.Table (Current).Project = Project then
                     return;
                  end if;

                  exit when Project_Lists.Table (Current).Next =
                    Empty_Project_List;
                  Current := Project_Lists.Table (Current).Next;
               end loop;

               --  This project has never been visited, add it
               --  to the list.

               Project_Lists.Increment_Last;
               Project_Lists.Table (Current).Next := Project_Lists.Last;
               Project_Lists.Table (Project_Lists.Last) :=
                 (Project => Project, Next => Empty_Project_List);
            end;
         end if;

         declare
            Current    : String_List_Id := Data.Source_Dirs;
            The_String : String_Element;

         begin
            --  Call action with the name of every source directorie

            while Current /= Nil_String loop
               The_String := String_Elements.Table (Current);
               Action (Get_Name_String (The_String.Value));
               Current := The_String.Next;
            end loop;
         end;

         --  If we are extending a project, visit it

         if Data.Extends /= No_Project then
            Add (Data.Extends);
         end if;

         --  And visit all imported projects

         while List /= Empty_Project_List loop
            Add (Project_Lists.Table (List).Project);
            List := Project_Lists.Table (List).Next;
         end loop;
      end Add;

   --  Start of processing for For_All_Source_Dirs

   begin
      --  Visit this project, and its imported projects recursively

      Add (Project);
   end For_All_Source_Dirs;

   -------------------
   -- Get_Reference --
   -------------------

   procedure Get_Reference
     (Source_File_Name : String;
      Project          : out Project_Id;
      Path             : out Name_Id)
   is
   begin
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

         for Id in Units.First .. Units.Last loop
            Unit := Units.Table (Id);

            if (Unit.File_Names (Specification).Name /= No_Name
                 and then
                   Namet.Get_Name_String
                     (Unit.File_Names (Specification).Name) = Original_Name)
              or else (Unit.File_Names (Specification).Path /= No_Name
                         and then
                           Namet.Get_Name_String
                           (Unit.File_Names (Specification).Path) =
                                                              Original_Name)
            then
               Project := Ultimate_Extension_Of
                            (Unit.File_Names (Specification).Project);
               Path := Unit.File_Names (Specification).Display_Path;

               if Current_Verbosity > Default then
                  Write_Str ("Done: Specification.");
                  Write_Eol;
               end if;

               return;

            elsif (Unit.File_Names (Body_Part).Name /= No_Name
                    and then
                      Namet.Get_Name_String
                        (Unit.File_Names (Body_Part).Name) = Original_Name)
              or else (Unit.File_Names (Body_Part).Path /= No_Name
                         and then Namet.Get_Name_String
                                    (Unit.File_Names (Body_Part).Path) =
                                                             Original_Name)
            then
               Project := Ultimate_Extension_Of
                            (Unit.File_Names (Body_Part).Project);
               Path := Unit.File_Names (Body_Part).Display_Path;

               if Current_Verbosity > Default then
                  Write_Str ("Done: Body.");
                  Write_Eol;
               end if;

               return;
            end if;

         end loop;
      end;

      Project := No_Project;
      Path    := No_Name;

      if Current_Verbosity > Default then
         Write_Str ("Cannot be found.");
         Write_Eol;
      end if;
   end Get_Reference;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  There is nothing to do anymore

      null;
   end Initialize;

   ------------------------------------
   -- Path_Name_Of_Library_Unit_Body --
   ------------------------------------

   function Path_Name_Of_Library_Unit_Body
     (Name    : String;
      Project : Project_Id) return String
   is
      Data : constant Project_Data := Projects.Table (Project);
      Original_Name : String := Name;

      Extended_Spec_Name : String :=
                             Name & Namet.Get_Name_String
                                     (Data.Naming.Current_Spec_Suffix);
      Extended_Body_Name : String :=
                             Name & Namet.Get_Name_String
                                     (Data.Naming.Current_Body_Suffix);

      First   : Unit_Id := Units.First;
      Current : Unit_Id;
      Unit    : Unit_Data;

   begin
      Canonical_Case_File_Name (Original_Name);
      Canonical_Case_File_Name (Extended_Spec_Name);
      Canonical_Case_File_Name (Extended_Body_Name);

      if Current_Verbosity = High then
         Write_Str  ("Looking for path name of """);
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

      while First <= Units.Last
        and then Units.Table (First).File_Names (Body_Part).Project /= Project
      loop
         First := First + 1;
      end loop;

      Current := First;
      while Current <= Units.Last loop
         Unit := Units.Table (Current);

         if Unit.File_Names (Body_Part).Project = Project
           and then Unit.File_Names (Body_Part).Name /= No_Name
         then
            declare
               Current_Name : constant String :=
                 Namet.Get_Name_String (Unit.File_Names (Body_Part).Name);
            begin
               if Current_Verbosity = High then
                  Write_Str  ("   Comparing with """);
                  Write_Str  (Current_Name);
                  Write_Char ('"');
                  Write_Eol;
               end if;

               if Current_Name = Original_Name then
                  if Current_Verbosity = High then
                     Write_Line ("   OK");
                  end if;

                  return Body_Path_Name_Of (Current);

               elsif Current_Name = Extended_Body_Name then
                  if Current_Verbosity = High then
                     Write_Line ("   OK");
                  end if;

                  return Body_Path_Name_Of (Current);

               else
                  if Current_Verbosity = High then
                     Write_Line ("   not good");
                  end if;
               end if;
            end;

         elsif Unit.File_Names (Specification).Name /= No_Name then
            declare
               Current_Name : constant String :=
                                Namet.Get_Name_String
                                  (Unit.File_Names (Specification).Name);

            begin
               if Current_Verbosity = High then
                  Write_Str  ("   Comparing with """);
                  Write_Str  (Current_Name);
                  Write_Char ('"');
                  Write_Eol;
               end if;

               if Current_Name = Original_Name then
                  if Current_Verbosity = High then
                     Write_Line ("   OK");
                  end if;

                  return Spec_Path_Name_Of (Current);

               elsif Current_Name = Extended_Spec_Name then

                  if Current_Verbosity = High then
                     Write_Line ("   OK");
                  end if;

                  return Spec_Path_Name_Of (Current);

               else
                  if Current_Verbosity = High then
                     Write_Line ("   not good");
                  end if;
               end if;
            end;
         end if;
         Current := Current + 1;
      end loop;

      return "";
   end Path_Name_Of_Library_Unit_Body;

   -------------------
   -- Print_Sources --
   -------------------

   procedure Print_Sources is
      Unit : Unit_Data;

   begin
      Write_Line ("List of Sources:");

      for Id in Units.First .. Units.Last loop
         Unit := Units.Table (Id);
         Write_Str  ("   ");
         Write_Line (Namet.Get_Name_String (Unit.Name));

         if Unit.File_Names (Specification).Name /= No_Name then
            if Unit.File_Names (Specification).Project = No_Project then
               Write_Line ("   No project");

            else
               Write_Str  ("   Project: ");
               Get_Name_String
                 (Projects.Table
                   (Unit.File_Names (Specification).Project).Path_Name);
               Write_Line (Name_Buffer (1 .. Name_Len));
            end if;

            Write_Str  ("      spec: ");
            Write_Line
              (Namet.Get_Name_String
               (Unit.File_Names (Specification).Name));
         end if;

         if Unit.File_Names (Body_Part).Name /= No_Name then
            if Unit.File_Names (Body_Part).Project = No_Project then
               Write_Line ("   No project");

            else
               Write_Str  ("   Project: ");
               Get_Name_String
                 (Projects.Table
                   (Unit.File_Names (Body_Part).Project).Path_Name);
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
      Main_Project : Project_Id) return Project_Id
   is
      Result : Project_Id := No_Project;

      Original_Name : String := Name;

      Data : constant Project_Data := Projects.Table (Main_Project);

      Extended_Spec_Name : String :=
                             Name & Namet.Get_Name_String
                                      (Data.Naming.Current_Spec_Suffix);
      Extended_Body_Name : String :=
                             Name & Namet.Get_Name_String
                                      (Data.Naming.Current_Body_Suffix);

      Unit : Unit_Data;

      Current_Name : Name_Id;

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

      for Current in reverse Units.First .. Units.Last loop
         Unit := Units.Table (Current);

         --  Check for body

         Current_Name := Unit.File_Names (Body_Part).Name;

         --  Case of a body present

         if Current_Name /= No_Name then

            --  If it has the name of the original name or the body name,
            --  we have found the project.

            if Unit.Name = The_Original_Name
              or else Current_Name = The_Original_Name
              or else Current_Name = The_Body_Name
            then
               Result := Unit.File_Names (Body_Part).Project;
               exit;
            end if;
         end if;

         --  Check for spec

         Current_Name := Unit.File_Names (Specification).Name;

         if Current_Name /= No_Name then

            --  If name same as the original name, or the spec name, we have
            --  found the project.

            if Unit.Name = The_Original_Name
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
         while Projects.Table (Result).Extended_By /= No_Project loop
            Result := Projects.Table (Result).Extended_By;
         end loop;
      end if;

      return Result;
   end Project_Of;

   -------------------
   -- Set_Ada_Paths --
   -------------------

   procedure Set_Ada_Paths
     (Project             : Project_Id;
      Including_Libraries : Boolean)
   is
      Source_FD : File_Descriptor := Invalid_FD;
      Object_FD : File_Descriptor := Invalid_FD;

      Process_Source_Dirs : Boolean := False;
      Process_Object_Dirs : Boolean := False;

      Status : Boolean;
      --  For calls to Close

      procedure Add (Project : Project_Id);
      --  Add all the source/object directories of a project to the path only
      --  if this project has not been visited. Calls itself recursively for
      --  projects being extended, and imported projects.

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id) is
      begin
         --  If Seen is False, then the project has not yet been visited

         if not Projects.Table (Project).Seen then
            Projects.Table (Project).Seen := True;

            declare
               Data : constant Project_Data := Projects.Table (Project);
               List : Project_List := Data.Imported_Projects;

            begin
               if Process_Source_Dirs then

                  --  Add to path all source directories of this project

                  Add_To_Path_File (Data.Source_Dirs, Source_FD);
               end if;

               if Process_Object_Dirs then

                  --  Add to path the object directory of this project
                  --  except if we don't include library project and
                  --  this is a library project.

                  if (Data.Library and then Including_Libraries)
                    or else
                     (Data.Object_Directory /= No_Name
                        and then
                         (not Including_Libraries or else not Data.Library))
                  then
                     --  For a library project, add the library directory

                     if Data.Library then
                        declare
                           New_Path : constant String :=
                                        Get_Name_String (Data.Library_Dir);

                        begin
                           Add_To_Path_File (New_Path, Object_FD);
                        end;

                     else
                        --  For a non library project, add the object directory

                        declare
                           New_Path : constant String :=
                             Get_Name_String (Data.Object_Directory);
                        begin
                           Add_To_Path_File (New_Path, Object_FD);
                        end;
                     end if;
                  end if;
               end if;

               --  Call Add to the project being extended, if any

               if Data.Extends /= No_Project then
                  Add (Data.Extends);
               end if;

               --  Call Add for each imported project, if any

               while List /= Empty_Project_List loop
                  Add (Project_Lists.Table (List).Project);
                  List := Project_Lists.Table (List).Next;
               end loop;
            end;
         end if;
      end Add;

   --  Start of processing for Set_Ada_Paths

   begin
      --  If it is the first time we call this procedure for
      --  this project, compute the source path and/or the object path.

      if Projects.Table (Project).Include_Path_File = No_Name then
         Process_Source_Dirs := True;
         Create_New_Path_File
           (Source_FD, Projects.Table (Project).Include_Path_File);
      end if;

      --  For the object path, we make a distinction depending on
      --  Including_Libraries.

      if Including_Libraries then
         if Projects.Table (Project).Objects_Path_File_With_Libs = No_Name then
            Process_Object_Dirs := True;
            Create_New_Path_File
              (Object_FD, Projects.Table (Project).
                                           Objects_Path_File_With_Libs);
         end if;

      else
         if
           Projects.Table (Project).Objects_Path_File_Without_Libs = No_Name
         then
            Process_Object_Dirs := True;
            Create_New_Path_File
              (Object_FD, Projects.Table (Project).
                                           Objects_Path_File_Without_Libs);
         end if;
      end if;

      --  If there is something to do, set Seen to False for all projects,
      --  then call the recursive procedure Add for Project.

      if Process_Source_Dirs or Process_Object_Dirs then
         for Index in 1 .. Projects.Last loop
            Projects.Table (Index).Seen := False;
         end loop;

         Add (Project);
      end if;

      --  Close any file that has been created.

      if Source_FD /= Invalid_FD then
         Close (Source_FD, Status);

         if not Status then
            Prj.Com.Fail ("disk full");
         end if;
      end if;

      if Object_FD /= Invalid_FD then
         Close (Object_FD, Status);

         if not Status then
            Prj.Com.Fail ("disk full");
         end if;
      end if;

      --  Set the env vars, if they need to be changed, and set the
      --  corresponding flags.

      if
        Current_Source_Path_File /= Projects.Table (Project).Include_Path_File
      then
         Current_Source_Path_File :=
           Projects.Table (Project).Include_Path_File;
         Set_Path_File_Var
           (Project_Include_Path_File,
            Get_Name_String (Current_Source_Path_File));
         Ada_Prj_Include_File_Set := True;
      end if;

      if Including_Libraries then
         if Current_Object_Path_File
              /= Projects.Table (Project).Objects_Path_File_With_Libs
         then
            Current_Object_Path_File :=
              Projects.Table (Project).Objects_Path_File_With_Libs;
            Set_Path_File_Var
              (Project_Objects_Path_File,
               Get_Name_String (Current_Object_Path_File));
            Ada_Prj_Objects_File_Set := True;
         end if;

      else
         if Current_Object_Path_File
              /= Projects.Table (Project).Objects_Path_File_Without_Libs
         then
            Current_Object_Path_File :=
              Projects.Table (Project).Objects_Path_File_Without_Libs;
            Set_Path_File_Var
              (Project_Objects_Path_File,
               Get_Name_String (Current_Object_Path_File));
            Ada_Prj_Objects_File_Set := True;
         end if;
      end if;
   end Set_Ada_Paths;

   ---------------------------------------------
   -- Set_Mapping_File_Initial_State_To_Empty --
   ---------------------------------------------

   procedure Set_Mapping_File_Initial_State_To_Empty is
   begin
      Fill_Mapping_File := False;
   end Set_Mapping_File_Initial_State_To_Empty;

   -----------------------
   -- Set_Path_File_Var --
   -----------------------

   procedure Set_Path_File_Var (Name : String; Value : String) is
      Host_Spec : String_Access := To_Host_File_Spec (Value);

   begin
      if Host_Spec = null then
         Prj.Com.Fail
           ("could not convert file name """, Value, """ to host spec");
      else
         Setenv (Name, Host_Spec.all);
         Free (Host_Spec);
      end if;
   end Set_Path_File_Var;

   -----------------------
   -- Spec_Path_Name_Of --
   -----------------------

   function Spec_Path_Name_Of (Unit : Unit_Id) return String is
      Data : Unit_Data := Units.Table (Unit);

   begin
      if Data.File_Names (Specification).Path = No_Name then
         declare
            Current_Source : String_List_Id :=
              Projects.Table (Data.File_Names (Specification).Project).Sources;
            Path : GNAT.OS_Lib.String_Access;

         begin
            Data.File_Names (Specification).Path :=
              Data.File_Names (Specification).Name;

            while Current_Source /= Nil_String loop
               Path := Locate_Regular_File
                 (Namet.Get_Name_String
                  (Data.File_Names (Specification).Name),
                  Namet.Get_Name_String
                   (String_Elements.Table (Current_Source).Value));

               if Path /= null then
                  Name_Len := Path'Length;
                  Name_Buffer (1 .. Name_Len) := Path.all;
                  Data.File_Names (Specification).Path := Name_Enter;
                  exit;
               else
                  Current_Source :=
                    String_Elements.Table (Current_Source).Next;
               end if;
            end loop;

            Units.Table (Unit) := Data;
         end;
      end if;

      return Namet.Get_Name_String (Data.File_Names (Specification).Path);
   end Spec_Path_Name_Of;

   ---------------------------
   -- Ultimate_Extension_Of --
   ---------------------------

   function Ultimate_Extension_Of (Project : in Project_Id) return Project_Id
   is
      Result : Project_Id := Project;

   begin
      while Projects.Table (Result).Extended_By /= No_Project loop
         Result := Projects.Table (Result).Extended_By;
      end loop;

      return Result;
   end Ultimate_Extension_Of;

begin
   Path_Files.Set_Last (0);
end Prj.Env;
