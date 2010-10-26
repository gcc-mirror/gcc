------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E N V                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2010, Free Software Foundation, Inc.         --
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
with Hostparm;
with Makeutl;  use Makeutl;
with Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Com;  use Prj.Com;
with Sdefault;
with Tempdir;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Prj.Env is

   Buffer_Initial : constant := 1_000;
   --  Initial size of Buffer

   Uninitialized_Prefix : constant String := '#' & Path_Separator;
   --  Prefix to indicate that the project path has not been initilized yet.
   --  Must be two characters long

   No_Project_Default_Dir : constant String := "-";
   --  Indicator in the project path to indicate that the default search
   --  directories should not be added to the path

   -----------------------
   -- Local Subprograms --
   -----------------------

   package Source_Path_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 100);
   --  A table to store the source dirs before creating the source path file

   package Object_Path_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Path_Name_Type,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 100);
   --  A table to store the object dirs, before creating the object path file

   procedure Add_To_Buffer
     (S           : String;
      Buffer      : in out String_Access;
      Buffer_Last : in out Natural);
   --  Add a string to Buffer, extending Buffer if needed

   procedure Add_To_Path
     (Source_Dirs : String_List_Id;
      In_Tree     : Project_Tree_Ref;
      Buffer      : in out String_Access;
      Buffer_Last : in out Natural);
   --  Add to Ada_Path_Buffer all the source directories in string list
   --  Source_Dirs, if any.

   procedure Add_To_Path
     (Dir         : String;
      Buffer      : in out String_Access;
      Buffer_Last : in out Natural);
   --  If Dir is not already in the global variable Ada_Path_Buffer, add it.
   --  If Buffer_Last /= 0, prepend a Path_Separator character to Path.

   procedure Add_To_Source_Path
     (Source_Dirs  : String_List_Id;
      In_Tree      : Project_Tree_Ref;
      Source_Paths : in out Source_Path_Table.Instance);
   --  Add to Ada_Path_B all the source directories in string list
   --  Source_Dirs, if any. Increment Ada_Path_Length.

   procedure Add_To_Object_Path
     (Object_Dir   : Path_Name_Type;
      Object_Paths : in out Object_Path_Table.Instance);
   --  Add Object_Dir to object path table. Make sure it is not duplicate
   --  and it is the last one in the current table.

   procedure Set_Path_File_Var (Name : String; Value : String);
   --  Call Setenv, after calling To_Host_File_Spec

   function Ultimate_Extension_Of
     (Project : Project_Id) return Project_Id;
   --  Return a project that is either Project or an extended ancestor of
   --  Project that itself is not extended.

   procedure Initialize_Project_Path
     (Self        : in out Project_Search_Path;
      Target_Name : String);
   --  Initialize Current_Project_Path. Does nothing if the path has already
   --  been initialized properly.

   ----------------------
   -- Ada_Include_Path --
   ----------------------

   function Ada_Include_Path
     (Project   : Project_Id;
      In_Tree   : Project_Tree_Ref;
      Recursive : Boolean := False) return String
   is
      Buffer      : String_Access;
      Buffer_Last : Natural := 0;

      procedure Add (Project : Project_Id; Dummy : in out Boolean);
      --  Add source dirs of Project to the path

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id; Dummy : in out Boolean) is
         pragma Unreferenced (Dummy);
      begin
         Add_To_Path (Project.Source_Dirs, In_Tree, Buffer, Buffer_Last);
      end Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Add);

      Dummy : Boolean := False;

   --  Start of processing for Ada_Include_Path

   begin
      if Recursive then

         --  If it is the first time we call this function for
         --  this project, compute the source path

         if Project.Ada_Include_Path = null then
            Buffer := new String (1 .. 4096);
            For_All_Projects (Project, Dummy);
            Project.Ada_Include_Path := new String'(Buffer (1 .. Buffer_Last));
            Free (Buffer);
         end if;

         return Project.Ada_Include_Path.all;

      else
         Buffer := new String (1 .. 4096);
         Add_To_Path (Project.Source_Dirs, In_Tree, Buffer, Buffer_Last);

         declare
            Result : constant String := Buffer (1 .. Buffer_Last);
         begin
            Free (Buffer);
            return Result;
         end;
      end if;
   end Ada_Include_Path;

   ----------------------
   -- Ada_Objects_Path --
   ----------------------

   function Ada_Objects_Path
     (Project             : Project_Id;
      Including_Libraries : Boolean := True) return String_Access
   is
      Buffer      : String_Access;
      Buffer_Last : Natural := 0;

      procedure Add (Project : Project_Id; Dummy : in out Boolean);
      --  Add all the object directories of a project to the path

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id; Dummy : in out Boolean) is
         pragma Unreferenced (Dummy);
         Path : constant Path_Name_Type :=
                  Get_Object_Directory
                    (Project,
                     Including_Libraries => Including_Libraries,
                     Only_If_Ada         => False);
      begin
         if Path /= No_Path then
            Add_To_Path (Get_Name_String (Path), Buffer, Buffer_Last);
         end if;
      end Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Add);

      Dummy : Boolean := False;

   --  Start of processing for Ada_Objects_Path

   begin
      --  If it is the first time we call this function for
      --  this project, compute the objects path

      if Project.Ada_Objects_Path = null then
         Buffer := new String (1 .. 4096);
         For_All_Projects (Project, Dummy);

         Project.Ada_Objects_Path := new String'(Buffer (1 .. Buffer_Last));
         Free (Buffer);
      end if;

      return Project.Ada_Objects_Path;
   end Ada_Objects_Path;

   -------------------
   -- Add_To_Buffer --
   -------------------

   procedure Add_To_Buffer
     (S           : String;
      Buffer      : in out String_Access;
      Buffer_Last : in out Natural)
   is
      Last : constant Natural := Buffer_Last + S'Length;

   begin
      while Last > Buffer'Last loop
         declare
            New_Buffer : constant String_Access :=
                           new String (1 .. 2 * Buffer'Last);
         begin
            New_Buffer (1 .. Buffer_Last) := Buffer (1 .. Buffer_Last);
            Free (Buffer);
            Buffer := New_Buffer;
         end;
      end loop;

      Buffer (Buffer_Last + 1 .. Last) := S;
      Buffer_Last := Last;
   end Add_To_Buffer;

   ------------------------
   -- Add_To_Object_Path --
   ------------------------

   procedure Add_To_Object_Path
     (Object_Dir   : Path_Name_Type;
      Object_Paths : in out Object_Path_Table.Instance)
   is
   begin
      --  Check if the directory is already in the table

      for Index in Object_Path_Table.First ..
                   Object_Path_Table.Last (Object_Paths)
      loop

         --  If it is, remove it, and add it as the last one

         if Object_Paths.Table (Index) = Object_Dir then
            for Index2 in Index + 1 ..
                          Object_Path_Table.Last (Object_Paths)
            loop
               Object_Paths.Table (Index2 - 1) := Object_Paths.Table (Index2);
            end loop;

            Object_Paths.Table
              (Object_Path_Table.Last (Object_Paths)) := Object_Dir;
            return;
         end if;
      end loop;

      --  The directory is not already in the table, add it

      Object_Path_Table.Append (Object_Paths, Object_Dir);
   end Add_To_Object_Path;

   -----------------
   -- Add_To_Path --
   -----------------

   procedure Add_To_Path
     (Source_Dirs : String_List_Id;
      In_Tree     : Project_Tree_Ref;
      Buffer      : in out String_Access;
      Buffer_Last : in out Natural)
   is
      Current    : String_List_Id := Source_Dirs;
      Source_Dir : String_Element;
   begin
      while Current /= Nil_String loop
         Source_Dir := In_Tree.String_Elements.Table (Current);
         Add_To_Path (Get_Name_String (Source_Dir.Display_Value),
                      Buffer, Buffer_Last);
         Current := Source_Dir.Next;
      end loop;
   end Add_To_Path;

   procedure Add_To_Path
     (Dir         : String;
      Buffer      : in out String_Access;
      Buffer_Last : in out Natural)
   is
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
      if Is_Present (Buffer (1 .. Buffer_Last), Dir) then

         --  Dir is already in the path, nothing to do

         return;
      end if;

      Min_Len := Buffer_Last + Dir'Length;

      if Buffer_Last > 0 then

         --  Add 1 for the Path_Separator character

         Min_Len := Min_Len + 1;
      end if;

      --  If Ada_Path_Buffer is too small, increase it

      Len := Buffer'Last;

      if Len < Min_Len then
         loop
            Len := Len * 2;
            exit when Len >= Min_Len;
         end loop;

         New_Buffer := new String (1 .. Len);
         New_Buffer (1 .. Buffer_Last) := Buffer (1 .. Buffer_Last);
         Free (Buffer);
         Buffer := New_Buffer;
      end if;

      if Buffer_Last > 0 then
         Buffer_Last := Buffer_Last + 1;
         Buffer (Buffer_Last) := Path_Separator;
      end if;

      Buffer (Buffer_Last + 1 .. Buffer_Last + Dir'Length) := Dir;
      Buffer_Last := Buffer_Last + Dir'Length;
   end Add_To_Path;

   ------------------------
   -- Add_To_Source_Path --
   ------------------------

   procedure Add_To_Source_Path
     (Source_Dirs  : String_List_Id;
      In_Tree      : Project_Tree_Ref;
      Source_Paths : in out Source_Path_Table.Instance)
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
                      Source_Path_Table.Last (Source_Paths)
         loop
            --  If it is already, no need to add it

            if Source_Paths.Table (Index) = Source_Dir.Value then
               Add_It := False;
               exit;
            end if;
         end loop;

         if Add_It then
            Source_Path_Table.Append (Source_Paths, Source_Dir.Display_Value);
         end if;

         --  Next source directory

         Current := Source_Dir.Next;
      end loop;
   end Add_To_Source_Path;

   --------------------------------
   -- Create_Config_Pragmas_File --
   --------------------------------

   procedure Create_Config_Pragmas_File
     (For_Project : Project_Id;
      In_Tree     : Project_Tree_Ref)
   is
      type Naming_Id is new Nat;
      package Naming_Table is new GNAT.Dynamic_Tables
        (Table_Component_Type => Lang_Naming_Data,
         Table_Index_Type     => Naming_Id,
         Table_Low_Bound      => 1,
         Table_Initial        => 5,
         Table_Increment      => 100);
      Default_Naming : constant Naming_Id := Naming_Table.First;
      Namings        : Naming_Table.Instance;
      --  Table storing the naming data for gnatmake/gprmake

      Buffer      : String_Access := new String (1 .. Buffer_Initial);
      Buffer_Last : Natural := 0;

      File_Name : Path_Name_Type  := No_Path;
      File      : File_Descriptor := Invalid_FD;

      Current_Naming  : Naming_Id;
      Iter            : Source_Iterator;
      Source          : Source_Id;

      procedure Check (Project : Project_Id; State : in out Integer);
      --  Recursive procedure that put in the config pragmas file any non
      --  standard naming schemes, if it is not already in the file, then call
      --  itself for any imported project.

      procedure Put (Source : Source_Id);
      --  Put an SFN pragma in the temporary file

      procedure Put (S : String);
      procedure Put_Line (S : String);
      --  Output procedures, analogous to normal Text_IO procs of same name.
      --  The text is put in Buffer, then it will be writen into a temporary
      --  file with procedure Write_Temp_File below.

      procedure Write_Temp_File;
      --  Create a temporary file and put the content of the buffer in it

      -----------
      -- Check --
      -----------

      procedure Check (Project : Project_Id; State : in out Integer) is
         pragma Unreferenced (State);
         Lang   : constant Language_Ptr :=
                    Get_Language_From_Name (Project, "ada");
         Naming : Lang_Naming_Data;

      begin
         if Current_Verbosity = High then
            Write_Str ("Checking project file """);
            Write_Str (Namet.Get_Name_String (Project.Name));
            Write_Str (""".");
            Write_Eol;
         end if;

         if Lang = null then
            if Current_Verbosity = High then
               Write_Line ("   Languages does not contain Ada, nothing to do");
            end if;

            return;
         end if;

         Naming := Lang.Config.Naming_Data;

         --  Is the naming scheme of this project one that we know?

         Current_Naming := Default_Naming;
         while Current_Naming <= Naming_Table.Last (Namings)
           and then Namings.Table (Current_Naming).Dot_Replacement =
                                                    Naming.Dot_Replacement
           and then Namings.Table (Current_Naming).Casing =
                                                    Naming.Casing
           and then Namings.Table (Current_Naming).Separate_Suffix =
                                                    Naming.Separate_Suffix
         loop
            Current_Naming := Current_Naming + 1;
         end loop;

         --  If we don't know it, add it

         if Current_Naming > Naming_Table.Last (Namings) then
            Naming_Table.Increment_Last (Namings);
            Namings.Table (Naming_Table.Last (Namings)) := Naming;

            --  Put the SFN pragmas for the naming scheme

            --  Spec

            Put_Line
              ("pragma Source_File_Name_Project");
            Put_Line
              ("  (Spec_File_Name  => ""*" &
               Get_Name_String (Naming.Spec_Suffix) & """,");
            Put_Line
              ("   Casing          => " &
               Image (Naming.Casing) & ",");
            Put_Line
              ("   Dot_Replacement => """ &
               Get_Name_String (Naming.Dot_Replacement) & """);");

            --  and body

            Put_Line
              ("pragma Source_File_Name_Project");
            Put_Line
              ("  (Body_File_Name  => ""*" &
               Get_Name_String (Naming.Body_Suffix) & """,");
            Put_Line
              ("   Casing          => " &
               Image (Naming.Casing) & ",");
            Put_Line
              ("   Dot_Replacement => """ &
               Get_Name_String (Naming.Dot_Replacement) &
               """);");

            --  and maybe separate

            if Naming.Body_Suffix /= Naming.Separate_Suffix then
               Put_Line ("pragma Source_File_Name_Project");
               Put_Line
                 ("  (Subunit_File_Name  => ""*" &
                  Get_Name_String (Naming.Separate_Suffix) & """,");
               Put_Line
                 ("   Casing          => " &
                  Image (Naming.Casing) & ",");
               Put_Line
                 ("   Dot_Replacement => """ &
                  Get_Name_String (Naming.Dot_Replacement) &
                  """);");
            end if;
         end if;
      end Check;

      ---------
      -- Put --
      ---------

      procedure Put (Source : Source_Id) is
      begin
         --  Put the pragma SFN for the unit kind (spec or body)

         Put ("pragma Source_File_Name_Project (");
         Put (Namet.Get_Name_String (Source.Unit.Name));

         if Source.Kind = Spec then
            Put (", Spec_File_Name => """);
         else
            Put (", Body_File_Name => """);
         end if;

         Put (Namet.Get_Name_String (Source.File));
         Put ("""");

         if Source.Index /= 0 then
            Put (", Index =>");
            Put (Source.Index'Img);
         end if;

         Put_Line (");");
      end Put;

      procedure Put (S : String) is
      begin
         Add_To_Buffer (S, Buffer, Buffer_Last);

         if Current_Verbosity = High then
            Write_Str (S);
         end if;
      end Put;

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (S : String) is
      begin
         --  Add an ASCII.LF to the string. As this config file is supposed to
         --  be used only by the compiler, we don't care about the characters
         --  for the end of line. In fact we could have put a space, but
         --  it is more convenient to be able to read gnat.adc during
         --  development, for which the ASCII.LF is fine.

         Put (S);
         Put (S => (1 => ASCII.LF));
      end Put_Line;

      ---------------------
      -- Write_Temp_File --
      ---------------------

      procedure Write_Temp_File is
         Status : Boolean := False;
         Last   : Natural;

      begin
         Tempdir.Create_Temp_File (File, File_Name);

         if File /= Invalid_FD then
            Last := Write (File, Buffer (1)'Address, Buffer_Last);

            if Last = Buffer_Last then
               Close (File, Status);
            end if;
         end if;

         if not Status then
            Prj.Com.Fail ("unable to create temporary file");
         end if;
      end Write_Temp_File;

      procedure Check_Imported_Projects is
        new For_Every_Project_Imported (Integer, Check);

      Dummy : Integer := 0;

   --  Start of processing for Create_Config_Pragmas_File

   begin
      if not For_Project.Config_Checked then
         Naming_Table.Init (Namings);

         --  Check the naming schemes

         Check_Imported_Projects (For_Project, Dummy, Imported_First => False);

         --  Visit all the files and process those that need an SFN pragma

         Iter := For_Each_Source (In_Tree, For_Project);
         while Element (Iter) /= No_Source loop
            Source := Element (Iter);

            if Source.Index >= 1
              and then not Source.Locally_Removed
              and then Source.Unit /= null
            then
               Put (Source);
            end if;

            Next (Iter);
         end loop;

         --  If there are no non standard naming scheme, issue the GNAT
         --  standard naming scheme. This will tell the compiler that
         --  a project file is used and will forbid any pragma SFN.

         if Buffer_Last = 0 then

            Put_Line ("pragma Source_File_Name_Project");
            Put_Line ("   (Spec_File_Name  => ""*.ads"",");
            Put_Line ("    Dot_Replacement => ""-"",");
            Put_Line ("    Casing          => lowercase);");

            Put_Line ("pragma Source_File_Name_Project");
            Put_Line ("   (Body_File_Name  => ""*.adb"",");
            Put_Line ("    Dot_Replacement => ""-"",");
            Put_Line ("    Casing          => lowercase);");
         end if;

         --  Close the temporary file

         Write_Temp_File;

         if Opt.Verbose_Mode then
            Write_Str ("Created configuration file """);
            Write_Str (Get_Name_String (File_Name));
            Write_Line ("""");
         end if;

         For_Project.Config_File_Name := File_Name;
         For_Project.Config_File_Temp := True;
         For_Project.Config_Checked   := True;
      end if;

      Free (Buffer);
   end Create_Config_Pragmas_File;

   --------------------
   -- Create_Mapping --
   --------------------

   procedure Create_Mapping (In_Tree : Project_Tree_Ref) is
      Data : Source_Id;
      Iter : Source_Iterator;

   begin
      Fmap.Reset_Tables;

      Iter := For_Each_Source (In_Tree);
      loop
         Data := Element (Iter);
         exit when Data = No_Source;

         if Data.Unit /= No_Unit_Index then
            if Data.Locally_Removed then
               Fmap.Add_Forbidden_File_Name (Data.File);
            else
               Fmap.Add_To_File_Map
                 (Unit_Name => Unit_Name_Type (Data.Unit.Name),
                  File_Name => Data.File,
                  Path_Name => File_Name_Type (Data.Path.Display_Name));
            end if;
         end if;

         Next (Iter);
      end loop;
   end Create_Mapping;

   -------------------------
   -- Create_Mapping_File --
   -------------------------

   procedure Create_Mapping_File
     (Project  : Project_Id;
      Language : Name_Id;
      In_Tree  : Project_Tree_Ref;
      Name     : out Path_Name_Type)
   is
      File   : File_Descriptor := Invalid_FD;

      Buffer : String_Access := new String (1 .. Buffer_Initial);
      Buffer_Last : Natural := 0;

      procedure Put_Name_Buffer;
      --  Put the line contained in the Name_Buffer in the global buffer

      procedure Process (Project : Project_Id; State : in out Integer);
      --  Generate the mapping file for Project (not recursively)

      ---------------------
      -- Put_Name_Buffer --
      ---------------------

      procedure Put_Name_Buffer is
      begin
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ASCII.LF;

         if Current_Verbosity = High then
            Write_Str ("Mapping file: " & Name_Buffer (1 .. Name_Len));
         end if;

         Add_To_Buffer (Name_Buffer (1 .. Name_Len), Buffer, Buffer_Last);
      end Put_Name_Buffer;

      -------------
      -- Process --
      -------------

      procedure Process (Project : Project_Id; State : in out Integer) is
         pragma Unreferenced (State);
         Source : Source_Id;
         Suffix : File_Name_Type;
         Iter   : Source_Iterator;

      begin
         Iter := For_Each_Source (In_Tree, Project, Language => Language);

         loop
            Source := Prj.Element (Iter);
            exit when Source = No_Source;

            if Source.Replaced_By = No_Source
              and then Source.Path.Name /= No_Path
              and then
                (Source.Language.Config.Kind = File_Based
                  or else Source.Unit /= No_Unit_Index)
            then
               if Source.Unit /= No_Unit_Index then
                  Get_Name_String (Source.Unit.Name);

                  if Source.Language.Config.Kind = Unit_Based then

                     --  ??? Mapping_Spec_Suffix could be set in the case of
                     --  gnatmake as well

                     Add_Char_To_Name_Buffer ('%');

                     if Source.Kind = Spec then
                        Add_Char_To_Name_Buffer ('s');
                     else
                        Add_Char_To_Name_Buffer ('b');
                     end if;

                  else
                     case Source.Kind is
                        when Spec =>
                           Suffix :=
                             Source.Language.Config.Mapping_Spec_Suffix;
                        when Impl | Sep =>
                           Suffix :=
                             Source.Language.Config.Mapping_Body_Suffix;
                     end case;

                     if Suffix /= No_File then
                        Add_Str_To_Name_Buffer
                          (Get_Name_String (Suffix));
                     end if;
                  end if;

                  Put_Name_Buffer;
               end if;

               Get_Name_String (Source.Display_File);
               Put_Name_Buffer;

               if Source.Locally_Removed then
                  Name_Len := 1;
                  Name_Buffer (1) := '/';
               else
                  Get_Name_String (Source.Path.Display_Name);
               end if;

               Put_Name_Buffer;
            end if;

            Next (Iter);
         end loop;
      end Process;

      procedure For_Every_Imported_Project is new
        For_Every_Project_Imported (State => Integer, Action => Process);

      Dummy : Integer := 0;

   --  Start of processing for Create_Mapping_File

   begin
      For_Every_Imported_Project (Project, Dummy);

      declare
         Last   : Natural;
         Status : Boolean := False;

      begin
         Create_Temp_File (In_Tree, File, Name, "mapping");

         if File /= Invalid_FD then
            Last := Write (File, Buffer (1)'Address, Buffer_Last);

            if Last = Buffer_Last then
               GNAT.OS_Lib.Close (File, Status);
            end if;
         end if;

         if not Status then
            Prj.Com.Fail ("could not write mapping file");
         end if;
      end;

      Free (Buffer);
   end Create_Mapping_File;

   ----------------------
   -- Create_Temp_File --
   ----------------------

   procedure Create_Temp_File
     (In_Tree   : Project_Tree_Ref;
      Path_FD   : out File_Descriptor;
      Path_Name : out Path_Name_Type;
      File_Use  : String)
   is
   begin
      Tempdir.Create_Temp_File (Path_FD, Path_Name);

      if Path_Name /= No_Path then
         if Current_Verbosity = High then
            Write_Line ("Create temp file (" & File_Use & ") "
                        & Get_Name_String (Path_Name));
         end if;

         Record_Temp_File (In_Tree, Path_Name);

      else
         Prj.Com.Fail
           ("unable to create temporary " & File_Use & " file");
      end if;
   end Create_Temp_File;

   --------------------------
   -- Create_New_Path_File --
   --------------------------

   procedure Create_New_Path_File
     (In_Tree   : Project_Tree_Ref;
      Path_FD   : out File_Descriptor;
      Path_Name : out Path_Name_Type)
   is
   begin
      Create_Temp_File (In_Tree, Path_FD, Path_Name, "path file");
   end Create_New_Path_File;

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
      Original_Name : String := Name;

      Lang   : constant Language_Ptr :=
        Get_Language_From_Name (Project, "ada");

      Unit              : Unit_Index;
      The_Original_Name : Name_Id;
      The_Spec_Name     : Name_Id;
      The_Body_Name     : Name_Id;

   begin
      --  ??? Same block in Project_Of
      Canonical_Case_File_Name (Original_Name);
      Name_Len := Original_Name'Length;
      Name_Buffer (1 .. Name_Len) := Original_Name;
      The_Original_Name := Name_Find;

      if Lang /= null then
         declare
            Naming : constant Lang_Naming_Data := Lang.Config.Naming_Data;
            Extended_Spec_Name : String :=
                                   Name & Namet.Get_Name_String
                                            (Naming.Spec_Suffix);
            Extended_Body_Name : String :=
                                   Name & Namet.Get_Name_String
                                            (Naming.Body_Suffix);

         begin
            Canonical_Case_File_Name (Extended_Spec_Name);
            Name_Len := Extended_Spec_Name'Length;
            Name_Buffer (1 .. Name_Len) := Extended_Spec_Name;
            The_Spec_Name := Name_Find;

            Canonical_Case_File_Name (Extended_Body_Name);
            Name_Len := Extended_Body_Name'Length;
            Name_Buffer (1 .. Name_Len) := Extended_Body_Name;
            The_Body_Name := Name_Find;
         end;

      else
         Name_Len := Name'Length;
         Name_Buffer (1 .. Name_Len) := Name;
         Canonical_Case_File_Name (Name_Buffer);
         The_Spec_Name := Name_Find;
         The_Body_Name := The_Spec_Name;
      end if;

      if Current_Verbosity = High then
         Write_Str  ("Looking for file name of """);
         Write_Str  (Name);
         Write_Char ('"');
         Write_Eol;
         Write_Str  ("   Extended Spec Name = """);
         Write_Str  (Get_Name_String (The_Spec_Name));
         Write_Char ('"');
         Write_Eol;
         Write_Str  ("   Extended Body Name = """);
         Write_Str  (Get_Name_String (The_Body_Name));
         Write_Char ('"');
         Write_Eol;
      end if;

      --  For extending project, search in the extended project if the source
      --  is not found. For non extending projects, this loop will be run only
      --  once.

      loop
         --  Loop through units

         Unit := Units_Htable.Get_First (In_Tree.Units_HT);
         while Unit /= null loop
            --  Check for body

            if not Main_Project_Only
              or else
                (Unit.File_Names (Impl) /= null
                 and then Unit.File_Names (Impl).Project = The_Project)
            then
               declare
                  Current_Name : File_Name_Type;
               begin
                  --  Case of a body present

                  if Unit.File_Names (Impl) /= null then
                     Current_Name := Unit.File_Names (Impl).File;

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
                             (Unit.File_Names (Impl).Path.Name);

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
                             (Unit.File_Names (Impl).Path.Name);

                        else
                           return Get_Name_String (The_Body_Name);
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
              or else
                (Unit.File_Names (Spec) /= null
                 and then Unit.File_Names (Spec).Project =
                   The_Project)
            then
               declare
                  Current_Name : File_Name_Type;

               begin
                  --  Case of spec present

                  if Unit.File_Names (Spec) /= null then
                     Current_Name := Unit.File_Names (Spec).File;
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
                             (Unit.File_Names (Spec).Path.Name);
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
                             (Unit.File_Names (Spec).Path.Name);
                        else
                           return Get_Name_String (The_Spec_Name);
                        end if;

                     else
                        if Current_Verbosity = High then
                           Write_Line ("   not good");
                        end if;
                     end if;
                  end if;
               end;
            end if;

            Unit := Units_Htable.Get_Next (In_Tree.Units_HT);
         end loop;

         --  If we are not in an extending project, give up

         exit when not Main_Project_Only
           or else The_Project.Extends = No_Project;

         --  Otherwise, look in the project we are extending

         The_Project := The_Project.Extends;
      end loop;

      --  We don't know this file name, return an empty string

      return "";
   end File_Name_Of_Library_Unit_Body;

   -------------------------
   -- For_All_Object_Dirs --
   -------------------------

   procedure For_All_Object_Dirs (Project : Project_Id) is
      procedure For_Project (Prj : Project_Id; Dummy : in out Integer);
      --  Get all object directories of Prj

      -----------------
      -- For_Project --
      -----------------

      procedure For_Project (Prj : Project_Id; Dummy : in out Integer) is
         pragma Unreferenced (Dummy);
      begin
         --  ??? Set_Ada_Paths has a different behavior for library project
         --  files, should we have the same ?

         if Prj.Object_Directory /= No_Path_Information then
            Get_Name_String (Prj.Object_Directory.Display_Name);
            Action (Name_Buffer (1 .. Name_Len));
         end if;
      end For_Project;

      procedure Get_Object_Dirs is
        new For_Every_Project_Imported (Integer, For_Project);
      Dummy : Integer := 1;

   --  Start of processing for For_All_Object_Dirs

   begin
      Get_Object_Dirs (Project, Dummy);
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
         Current    : String_List_Id := Prj.Source_Dirs;
         The_String : String_Element;

      begin
         --  If there are Ada sources, call action with the name of every
         --  source directory.

         if Has_Ada_Sources (Project) then
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
      Get_Source_Dirs (Project, Dummy);
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
         Unit          : Unit_Index;

      begin
         Canonical_Case_File_Name (Original_Name);
         Unit := Units_Htable.Get_First (In_Tree.Units_HT);

         while Unit /= null loop
            if Unit.File_Names (Spec) /= null
              and then Unit.File_Names (Spec).File /= No_File
              and then
                (Namet.Get_Name_String
                     (Unit.File_Names (Spec).File) = Original_Name
                 or else (Unit.File_Names (Spec).Path /=
                            No_Path_Information
                          and then
                            Namet.Get_Name_String
                              (Unit.File_Names (Spec).Path.Name) =
                            Original_Name))
            then
               Project := Ultimate_Extension_Of
                          (Project => Unit.File_Names (Spec).Project);
               Path := Unit.File_Names (Spec).Path.Display_Name;

               if Current_Verbosity > Default then
                  Write_Str ("Done: Spec.");
                  Write_Eol;
               end if;

               return;

            elsif Unit.File_Names (Impl) /= null
              and then Unit.File_Names (Impl).File /= No_File
              and then
                (Namet.Get_Name_String
                   (Unit.File_Names (Impl).File) = Original_Name
                 or else (Unit.File_Names (Impl).Path /=
                            No_Path_Information
                          and then Namet.Get_Name_String
                            (Unit.File_Names (Impl).Path.Name) =
                            Original_Name))
            then
               Project := Ultimate_Extension_Of
                            (Project => Unit.File_Names (Impl).Project);
               Path := Unit.File_Names (Impl).Path.Display_Name;

               if Current_Verbosity > Default then
                  Write_Str ("Done: Body.");
                  Write_Eol;
               end if;

               return;
            end if;

            Unit := Units_Htable.Get_Next (In_Tree.Units_HT);
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
      In_Tree.Private_Part.Current_Source_Path_File := No_Path;
      In_Tree.Private_Part.Current_Object_Path_File := No_Path;
   end Initialize;

   -------------------
   -- Print_Sources --
   -------------------

   --  Could use some comments in this body ???

   procedure Print_Sources (In_Tree : Project_Tree_Ref) is
      Unit : Unit_Index;

   begin
      Write_Line ("List of Sources:");

      Unit := Units_Htable.Get_First (In_Tree.Units_HT);

      while Unit /= No_Unit_Index loop
         Write_Str  ("   ");
         Write_Line (Namet.Get_Name_String (Unit.Name));

         if Unit.File_Names (Spec).File /= No_File then
            if Unit.File_Names (Spec).Project = No_Project then
               Write_Line ("   No project");

            else
               Write_Str  ("   Project: ");
               Get_Name_String
                 (Unit.File_Names (Spec).Project.Path.Name);
               Write_Line (Name_Buffer (1 .. Name_Len));
            end if;

            Write_Str  ("      spec: ");
            Write_Line
              (Namet.Get_Name_String
               (Unit.File_Names (Spec).File));
         end if;

         if Unit.File_Names (Impl).File /= No_File then
            if Unit.File_Names (Impl).Project = No_Project then
               Write_Line ("   No project");

            else
               Write_Str  ("   Project: ");
               Get_Name_String
                 (Unit.File_Names (Impl).Project.Path.Name);
               Write_Line (Name_Buffer (1 .. Name_Len));
            end if;

            Write_Str  ("      body: ");
            Write_Line
              (Namet.Get_Name_String (Unit.File_Names (Impl).File));
         end if;

         Unit := Units_Htable.Get_Next (In_Tree.Units_HT);
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

      Lang : constant Language_Ptr :=
               Get_Language_From_Name (Main_Project, "ada");

      Unit : Unit_Index;

      Current_Name      : File_Name_Type;
      The_Original_Name : File_Name_Type;
      The_Spec_Name     : File_Name_Type;
      The_Body_Name     : File_Name_Type;

   begin
      --  ??? Same block in File_Name_Of_Library_Unit_Body
      Canonical_Case_File_Name (Original_Name);
      Name_Len := Original_Name'Length;
      Name_Buffer (1 .. Name_Len) := Original_Name;
      The_Original_Name := Name_Find;

      if Lang /= null then
         declare
            Naming : Lang_Naming_Data renames Lang.Config.Naming_Data;
            Extended_Spec_Name : String :=
                                   Name & Namet.Get_Name_String
                                            (Naming.Spec_Suffix);
            Extended_Body_Name : String :=
                                   Name & Namet.Get_Name_String
                                            (Naming.Body_Suffix);

         begin
            Canonical_Case_File_Name (Extended_Spec_Name);
            Name_Len := Extended_Spec_Name'Length;
            Name_Buffer (1 .. Name_Len) := Extended_Spec_Name;
            The_Spec_Name := Name_Find;

            Canonical_Case_File_Name (Extended_Body_Name);
            Name_Len := Extended_Body_Name'Length;
            Name_Buffer (1 .. Name_Len) := Extended_Body_Name;
            The_Body_Name := Name_Find;
         end;

      else
         The_Spec_Name := The_Original_Name;
         The_Body_Name := The_Original_Name;
      end if;

      Unit := Units_Htable.Get_First (In_Tree.Units_HT);
      while Unit /= null loop

         --  Case of a body present

         if Unit.File_Names (Impl) /= null then
            Current_Name := Unit.File_Names (Impl).File;

            --  If it has the name of the original name or the body name,
            --  we have found the project.

            if Unit.Name = Name_Id (The_Original_Name)
              or else Current_Name = The_Original_Name
              or else Current_Name = The_Body_Name
            then
               Result := Unit.File_Names (Impl).Project;
               exit;
            end if;
         end if;

         --  Check for spec

         if Unit.File_Names (Spec) /= null then
            Current_Name := Unit.File_Names (Spec).File;

            --  If name same as the original name, or the spec name, we have
            --  found the project.

            if Unit.Name = Name_Id (The_Original_Name)
              or else Current_Name = The_Original_Name
              or else Current_Name = The_Spec_Name
            then
               Result := Unit.File_Names (Spec).Project;
               exit;
            end if;
         end if;

         Unit := Units_Htable.Get_Next (In_Tree.Units_HT);
      end loop;

      --  Get the ultimate extending project

      if Result /= No_Project then
         while Result.Extended_By /= No_Project loop
            Result := Result.Extended_By;
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
      Including_Libraries : Boolean;
      Include_Path        : Boolean := True;
      Objects_Path        : Boolean := True)

   is
      Source_Paths : Source_Path_Table.Instance;
      Object_Paths : Object_Path_Table.Instance;
      --  List of source or object dirs. Only computed the first time this
      --  procedure is called (since Source_FD is then reused)

      Source_FD : File_Descriptor := Invalid_FD;
      Object_FD : File_Descriptor := Invalid_FD;
      --  The temporary files to store the paths. These are only created the
      --  first time this procedure is called, and reused from then on.

      Process_Source_Dirs : Boolean := False;
      Process_Object_Dirs : Boolean := False;

      Status : Boolean;
      --  For calls to Close

      Last        : Natural;
      Buffer      : String_Access := new String (1 .. Buffer_Initial);
      Buffer_Last : Natural := 0;

      procedure Recursive_Add (Project : Project_Id; Dummy : in out Boolean);
      --  Recursive procedure to add the source/object paths of extended/
      --  imported projects.

      -------------------
      -- Recursive_Add --
      -------------------

      procedure Recursive_Add (Project : Project_Id; Dummy : in out Boolean) is
         pragma Unreferenced (Dummy);

         Path : Path_Name_Type;

      begin
         --  ??? This is almost the equivalent of For_All_Source_Dirs

         if Process_Source_Dirs then

            --  Add to path all source directories of this project if there are
            --  Ada sources.

            if Has_Ada_Sources (Project) then
               Add_To_Source_Path (Project.Source_Dirs, In_Tree, Source_Paths);
            end if;
         end if;

         if Process_Object_Dirs then
            Path := Get_Object_Directory
              (Project,
               Including_Libraries => Including_Libraries,
               Only_If_Ada         => True);

            if Path /= No_Path then
               Add_To_Object_Path (Path, Object_Paths);
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

      if Include_Path and then Project.Include_Path_File = No_Path then
         Source_Path_Table.Init (Source_Paths);
         Process_Source_Dirs := True;
         Create_New_Path_File
           (In_Tree, Source_FD, Project.Include_Path_File);
      end if;

      --  For the object path, we make a distinction depending on
      --  Including_Libraries.

      if Objects_Path and Including_Libraries then
         if Project.Objects_Path_File_With_Libs = No_Path then
            Object_Path_Table.Init (Object_Paths);
            Process_Object_Dirs := True;
            Create_New_Path_File
              (In_Tree, Object_FD, Project.Objects_Path_File_With_Libs);
         end if;

      elsif Objects_Path then
         if Project.Objects_Path_File_Without_Libs = No_Path then
            Object_Path_Table.Init (Object_Paths);
            Process_Object_Dirs := True;
            Create_New_Path_File
              (In_Tree, Object_FD, Project.Objects_Path_File_Without_Libs);
         end if;
      end if;

      --  If there is something to do, set Seen to False for all projects,
      --  then call the recursive procedure Add for Project.

      if Process_Source_Dirs or Process_Object_Dirs then
         For_All_Projects (Project, Dummy);
      end if;

      --  Write and close any file that has been created. Source_FD is not set
      --  when this subprogram is called a second time or more, since we reuse
      --  the previous version of the file.

      if Source_FD /= Invalid_FD then
         Buffer_Last := 0;

         for Index in Source_Path_Table.First ..
                      Source_Path_Table.Last (Source_Paths)
         loop
            Get_Name_String (Source_Paths.Table (Index));
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := ASCII.LF;
            Add_To_Buffer (Name_Buffer (1 .. Name_Len), Buffer, Buffer_Last);
         end loop;

         Last := Write (Source_FD, Buffer (1)'Address, Buffer_Last);

         if Last = Buffer_Last then
            Close (Source_FD, Status);

         else
            Status := False;
         end if;

         if not Status then
            Prj.Com.Fail ("could not write temporary file");
         end if;
      end if;

      if Object_FD /= Invalid_FD then
         Buffer_Last := 0;

         for Index in Object_Path_Table.First ..
                      Object_Path_Table.Last (Object_Paths)
         loop
            Get_Name_String (Object_Paths.Table (Index));
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := ASCII.LF;
            Add_To_Buffer (Name_Buffer (1 .. Name_Len), Buffer, Buffer_Last);
         end loop;

         Last := Write (Object_FD, Buffer (1)'Address, Buffer_Last);

         if Last = Buffer_Last then
            Close (Object_FD, Status);
         else
            Status := False;
         end if;

         if not Status then
            Prj.Com.Fail ("could not write temporary file");
         end if;
      end if;

      --  Set the env vars, if they need to be changed, and set the
      --  corresponding flags.

      if Include_Path and then
         In_Tree.Private_Part.Current_Source_Path_File /=
           Project.Include_Path_File
      then
         In_Tree.Private_Part.Current_Source_Path_File :=
           Project.Include_Path_File;
         Set_Path_File_Var
           (Project_Include_Path_File,
            Get_Name_String (In_Tree.Private_Part.Current_Source_Path_File));
      end if;

      if Objects_Path then
         if Including_Libraries then
            if In_Tree.Private_Part.Current_Object_Path_File /=
              Project.Objects_Path_File_With_Libs
            then
               In_Tree.Private_Part.Current_Object_Path_File :=
                 Project.Objects_Path_File_With_Libs;
               Set_Path_File_Var
                 (Project_Objects_Path_File,
                  Get_Name_String
                    (In_Tree.Private_Part.Current_Object_Path_File));
            end if;

         else
            if In_Tree.Private_Part.Current_Object_Path_File /=
              Project.Objects_Path_File_Without_Libs
            then
               In_Tree.Private_Part.Current_Object_Path_File :=
                 Project.Objects_Path_File_Without_Libs;
               Set_Path_File_Var
                 (Project_Objects_Path_File,
                  Get_Name_String
                    (In_Tree.Private_Part.Current_Object_Path_File));
            end if;
         end if;
      end if;

      Free (Buffer);
   end Set_Ada_Paths;

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
     (Project : Project_Id) return Project_Id
   is
      Result : Project_Id;

   begin
      Result := Project;
      while Result.Extended_By /= No_Project loop
         Result := Result.Extended_By;
      end loop;

      return Result;
   end Ultimate_Extension_Of;

   ---------------------
   -- Add_Directories --
   ---------------------

   procedure Add_Directories
     (Self : in out Project_Search_Path;
      Path : String)
   is
      Tmp : String_Access;
   begin
      if Self.Path = null then
         Self.Path := new String'(Uninitialized_Prefix & Path);
      else
         Tmp := Self.Path;
         Self.Path := new String'(Tmp.all & Path_Separator & Path);
         Free (Tmp);
      end if;
   end Add_Directories;

   -----------------------------
   -- Initialize_Project_Path --
   -----------------------------

   procedure Initialize_Project_Path
     (Self        : in out Project_Search_Path;
      Target_Name : String)
   is
      Add_Default_Dir : Boolean := True;
      First           : Positive;
      Last            : Positive;
      New_Len         : Positive;
      New_Last        : Positive;

      Ada_Project_Path : constant String := "ADA_PROJECT_PATH";
      Gpr_Project_Path : constant String := "GPR_PROJECT_PATH";
      --  Name of alternate env. variable that contain path name(s) of
      --  directories where project files may reside. GPR_PROJECT_PATH has
      --  precedence over ADA_PROJECT_PATH.

      Gpr_Prj_Path : String_Access;
      Ada_Prj_Path : String_Access;
      --  The path name(s) of directories where project files may reside.
      --  May be empty.

   begin
      --  If already initialized, nothing else to do

      if Self.Path /= null
        and then Self.Path (Self.Path'First) /= '#'
      then
         return;
      end if;

      --  The current directory is always first in the search path. Since the
      --  Project_Path currently starts with '#:' as a sign that it isn't
      --  initialized, we simply replace '#' with '.'

      if Self.Path = null then
         Self.Path := new String'('.' & Path_Separator);
      else
         Self.Path (Self.Path'First) := '.';
      end if;

      --  Then the reset of the project path (if any) currently contains the
      --  directories added through Add_Search_Project_Directory

      --  If environment variables are defined and not empty, add their content

      Gpr_Prj_Path := Getenv (Gpr_Project_Path);
      Ada_Prj_Path := Getenv (Ada_Project_Path);

      if Gpr_Prj_Path.all /= "" then
         Add_Directories (Self, Gpr_Prj_Path.all);
      end if;

      Free (Gpr_Prj_Path);

      if Ada_Prj_Path.all /= "" then
         Add_Directories (Self, Ada_Prj_Path.all);
      end if;

      Free (Ada_Prj_Path);

      --  Copy to Name_Buffer, since we will need to manipulate the path

      Name_Len := Self.Path'Length;
      Name_Buffer (1 .. Name_Len) := Self.Path.all;

      --  Scan the directory path to see if "-" is one of the directories.
      --  Remove each occurrence of "-" and set Add_Default_Dir to False.
      --  Also resolve relative paths and symbolic links.

      First := 3;
      loop
         while First <= Name_Len
           and then (Name_Buffer (First) = Path_Separator)
         loop
            First := First + 1;
         end loop;

         exit when First > Name_Len;

         Last := First;

         while Last < Name_Len
           and then Name_Buffer (Last + 1) /= Path_Separator
         loop
            Last := Last + 1;
         end loop;

         --  If the directory is "-", set Add_Default_Dir to False and
         --  remove from path.

         if Name_Buffer (First .. Last) = No_Project_Default_Dir then
            Add_Default_Dir := False;

            for J in Last + 1 .. Name_Len loop
               Name_Buffer (J - No_Project_Default_Dir'Length - 1) :=
                 Name_Buffer (J);
            end loop;

            Name_Len := Name_Len - No_Project_Default_Dir'Length - 1;

            --  After removing the '-', go back one character to get the next
            --  directory correctly.

            Last := Last - 1;

         elsif not Hostparm.OpenVMS
           or else not Is_Absolute_Path (Name_Buffer (First .. Last))
         then
            --  On VMS, only expand relative path names, as absolute paths
            --  may correspond to multi-valued VMS logical names.

            declare
               New_Dir : constant String :=
                           Normalize_Pathname
                             (Name_Buffer (First .. Last),
                              Resolve_Links => Opt.Follow_Links_For_Dirs);

            begin
               --  If the absolute path was resolved and is different from
               --  the original, replace original with the resolved path.

               if New_Dir /= Name_Buffer (First .. Last)
                 and then New_Dir'Length /= 0
               then
                  New_Len := Name_Len + New_Dir'Length - (Last - First + 1);
                  New_Last := First + New_Dir'Length - 1;
                  Name_Buffer (New_Last + 1 .. New_Len) :=
                    Name_Buffer (Last + 1 .. Name_Len);
                  Name_Buffer (First .. New_Last) := New_Dir;
                  Name_Len := New_Len;
                  Last := New_Last;
               end if;
            end;
         end if;

         First := Last + 1;
      end loop;

      Free (Self.Path);

      --  Set the initial value of Current_Project_Path

      if Add_Default_Dir then
         declare
            Prefix : String_Ptr := Sdefault.Search_Dir_Prefix;

         begin
            if Prefix = null then
               Prefix := new String'(Executable_Prefix_Path);

               if Prefix.all /= "" then
                  if Target_Name /= "" then
                     Add_Str_To_Name_Buffer
                       (Path_Separator & Prefix.all &
                        "lib" & Directory_Separator & "gpr" &
                        Directory_Separator & Target_Name);
                  end if;

                  Add_Str_To_Name_Buffer
                    (Path_Separator & Prefix.all &
                     "share" & Directory_Separator & "gpr");
                  Add_Str_To_Name_Buffer
                    (Path_Separator & Prefix.all &
                     "lib" & Directory_Separator & "gnat");
               end if;

            else
               Self.Path :=
                 new String'(Name_Buffer (1 .. Name_Len) & Path_Separator &
                             Prefix.all &
                             ".." &  Directory_Separator &
                             ".." & Directory_Separator &
                             ".." & Directory_Separator & "gnat");
            end if;

            Free (Prefix);
         end;
      end if;

      if Self.Path = null then
         Self.Path := new String'(Name_Buffer (1 .. Name_Len));
      end if;
   end Initialize_Project_Path;

   --------------
   -- Get_Path --
   --------------

   procedure Get_Path
     (Self : in out Project_Search_Path;
      Path : out String_Access)
   is
   begin
      Initialize_Project_Path (Self, "");  --  ??? Target_Name unspecified
      Path := Self.Path;
   end Get_Path;

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path
     (Self : in out Project_Search_Path; Path : String) is
   begin
      Free (Self.Path);
      Self.Path := new String'(Path);
      Projects_Paths.Reset (Self.Cache);
   end Set_Path;

   ------------------
   -- Find_Project --
   ------------------

   procedure Find_Project
     (Self               : in out Project_Search_Path;
      Project_File_Name  : String;
      Directory          : String;
      Path               : out Namet.Path_Name_Type)
   is
      File : constant String := Project_File_Name;
      --  Have to do a copy, in case the parameter is Name_Buffer, which we
      --  modify below

      function Try_Path_Name (Path : String) return String_Access;
      pragma Inline (Try_Path_Name);
      --  Try the specified Path

      -------------------
      -- Try_Path_Name --
      -------------------

      function Try_Path_Name (Path : String) return String_Access is
         First    : Natural;
         Last     : Natural;
         Result   : String_Access := null;

      begin
         if Current_Verbosity = High then
            Write_Str  ("   Trying ");
            Write_Line (Path);
         end if;

         if Is_Absolute_Path (Path) then
            if Is_Regular_File (Path) then
               Result := new String'(Path);
            end if;

         else
            --  Because we don't want to resolve symbolic links, we cannot use
            --  Locate_Regular_File. So, we try each possible path
            --  successively.

            First := Self.Path'First;
            while First <= Self.Path'Last loop
               while First <= Self.Path'Last
                 and then Self.Path (First) = Path_Separator
               loop
                  First := First + 1;
               end loop;

               exit when First > Self.Path'Last;

               Last := First;
               while Last < Self.Path'Last
                 and then Self.Path (Last + 1) /= Path_Separator
               loop
                  Last := Last + 1;
               end loop;

               Name_Len := 0;

               if not Is_Absolute_Path (Self.Path (First .. Last)) then
                  Add_Str_To_Name_Buffer (Get_Current_Dir);  -- ??? System call
                  Add_Char_To_Name_Buffer (Directory_Separator);
               end if;

               Add_Str_To_Name_Buffer (Self.Path (First .. Last));
               Add_Char_To_Name_Buffer (Directory_Separator);
               Add_Str_To_Name_Buffer (Path);

               if Current_Verbosity = High then
                  Write_Str  ("   Testing file ");
                  Write_Line (Name_Buffer (1 .. Name_Len));
               end if;

               if Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
                  Result := new String'(Name_Buffer (1 .. Name_Len));
                  exit;
               end if;

               First := Last + 1;
            end loop;
         end if;

         return Result;
      end Try_Path_Name;

      --  Local Declarations

      Result    : String_Access;
      Has_Dot   : Boolean := False;
      Key       : Name_Id;

   --  Start of processing for Find_Project

   begin
      Initialize_Project_Path (Self, "");

      if Current_Verbosity = High then
         Write_Str  ("Searching for project (""");
         Write_Str  (File);
         Write_Str  (""", """);
         Write_Str  (Directory);
         Write_Line (""");");
      end if;

      --  Check the project cache

      Name_Len := File'Length;
      Name_Buffer (1 .. Name_Len) := File;
      Key := Name_Find;
      Path := Projects_Paths.Get (Self.Cache, Key);

      if Path /= No_Path then
         return;
      end if;

      --  Check if File contains an extension (a dot before a
      --  directory separator). If it is the case we do not try project file
      --  with an added extension as it is not possible to have multiple dots
      --  on a project file name.

      Check_Dot : for K in reverse File'Range loop
         if File (K) = '.' then
            Has_Dot := True;
            exit Check_Dot;
         end if;

         exit Check_Dot when File (K) = Directory_Separator
           or else File (K) = '/';
      end loop Check_Dot;

      if not Is_Absolute_Path (File) then

         --  First we try <directory>/<file_name>.<extension>

         if not Has_Dot then
            Result := Try_Path_Name
              (Directory & Directory_Separator &
               File & Project_File_Extension);
         end if;

         --  Then we try <directory>/<file_name>

         if Result = null then
            Result := Try_Path_Name (Directory & Directory_Separator & File);
         end if;
      end if;

      --  Then we try <file_name>.<extension>

      if Result = null and then not Has_Dot then
         Result := Try_Path_Name (File & Project_File_Extension);
      end if;

      --  Then we try <file_name>

      if Result = null then
         Result := Try_Path_Name (File);
      end if;

      --  If we cannot find the project file, we return an empty string

      if Result = null then
         Path := Namet.No_Path;
         return;

      else
         declare
            Final_Result : constant String :=
                             GNAT.OS_Lib.Normalize_Pathname
                               (Result.all,
                                Directory      => Directory,
                                Resolve_Links  => Opt.Follow_Links_For_Files,
                                Case_Sensitive => True);
         begin
            Free (Result);
            Name_Len := Final_Result'Length;
            Name_Buffer (1 .. Name_Len) := Final_Result;
            Path := Name_Find;
            Projects_Paths.Set (Self.Cache, Key, Path);
         end;
      end if;
   end Find_Project;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Project_Search_Path) is
   begin
      Free (Self.Path);
      Projects_Paths.Reset (Self.Cache);
   end Free;

end Prj.Env;
