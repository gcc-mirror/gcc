------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E N V                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Namet;       use Namet;
with Opt;
with Osint;       use Osint;
with Output;      use Output;
with Prj.Com;     use Prj.Com;
with Prj.Util;
with Snames;      use Snames;
with Stringt;     use Stringt;
with Table;

package body Prj.Env is

   type Naming_Id is new Nat;
   No_Naming : constant Naming_Id := 0;

   Ada_Path_Buffer : String_Access := new String (1 .. 1_000);
   --  A buffer where values for ADA_INCLUDE_PATH
   --  and ADA_OBJECTS_PATH are stored.

   Ada_Path_Length : Natural := 0;
   --  Index of the last valid character in Ada_Path_Buffer.

   package Namings is new Table.Table (
     Table_Component_Type => Naming_Data,
     Table_Index_Type     => Naming_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 5,
     Table_Increment      => 100,
     Table_Name           => "Prj.Env.Namings");

   Default_Naming : constant Naming_Id := Namings.First;

   Global_Configuration_Pragmas : Name_Id;
   Local_Configuration_Pragmas  : Name_Id;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Body_Path_Name_Of (Unit : Unit_Id) return String;
   --  Returns the path name of the body of a unit.
   --  Compute it first, if necessary.

   function Spec_Path_Name_Of (Unit : Unit_Id) return String;
   --  Returns the path name of the spec of a unit.
   --  Compute it first, if necessary.

   procedure Add_To_Path (Path : String);
   --  Add Path to global variable Ada_Path_Buffer
   --  Increment Ada_Path_Length

   ----------------------
   -- Ada_Include_Path --
   ----------------------

   function Ada_Include_Path (Project : Project_Id) return String_Access is

      procedure Add (Project : Project_Id);
      --  Add all the source directories of a project to the path,
      --  only if this project has not been visited.
      --  Call itself recursively for projects being modified,
      --  and imported projects.
      --  Add the project to the list Seen if this is the first time
      --  we call Add for this project.

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id) is
      begin
         --  If Seen is empty, then the project cannot have been
         --  visited.

         if not Projects.Table (Project).Seen then
            Projects.Table (Project).Seen := True;

            declare
               Data : Project_Data := Projects.Table (Project);
               List : Project_List := Data.Imported_Projects;

               Current : String_List_Id := Data.Source_Dirs;
               Source_Dir : String_Element;

            begin
               --  Add to path all source directories of this project

               while Current /= Nil_String loop
                  if Ada_Path_Length > 0 then
                     Add_To_Path (Path => (1 => Path_Separator));
                  end if;

                  Source_Dir := String_Elements.Table (Current);
                  String_To_Name_Buffer (Source_Dir.Value);

                  declare
                     New_Path : constant String :=
                       Name_Buffer (1 .. Name_Len);
                  begin
                     Add_To_Path (New_Path);
                  end;

                  Current := Source_Dir.Next;
               end loop;

               --  Call Add to the project being modified, if any

               if Data.Modifies /= No_Project then
                  Add (Data.Modifies);
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

      if Projects.Table (Project).Include_Path = null then
         Ada_Path_Length := 0;

         for Index in 1 .. Projects.Last loop
            Projects.Table (Index).Seen := False;
         end loop;

         Add (Project);
         Projects.Table (Project).Include_Path :=
           new String'(Ada_Path_Buffer (1 .. Ada_Path_Length));
      end if;

      return Projects.Table (Project).Include_Path;
   end Ada_Include_Path;

   ----------------------
   -- Ada_Objects_Path --
   ----------------------

   function Ada_Objects_Path
     (Project             : Project_Id;
      Including_Libraries : Boolean := True)
     return String_Access is

      procedure Add (Project : Project_Id);
      --  Add all the object directory of a project to the path,
      --  only if this project has not been visited.
      --  Call itself recursively for projects being modified,
      --  and imported projects.
      --  Add the project to the list Seen if this is the first time
      --  we call Add for this project.

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id) is
      begin

         --  If this project has not been seen yet

         if not Projects.Table (Project).Seen then
            Projects.Table (Project).Seen := True;

            declare
               Data : Project_Data := Projects.Table (Project);
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
                  if Ada_Path_Length > 0 then
                     Add_To_Path (Path => (1 => Path_Separator));
                  end if;

                  --  For a library project, att the library directory

                  if Data.Library then
                     declare
                        New_Path : constant String :=
                          Get_Name_String (Data.Library_Dir);
                     begin
                        Add_To_Path (New_Path);
                     end;
                  else

                     --  For a non library project, add the object directory
                     declare
                        New_Path : constant String :=
                          Get_Name_String (Data.Object_Directory);
                     begin
                        Add_To_Path (New_Path);
                     end;
                  end if;
               end if;

               --  Call Add to the project being modified, if any

               if Data.Modifies /= No_Project then
                  Add (Data.Modifies);
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

      if Projects.Table (Project).Objects_Path = null then
         Ada_Path_Length := 0;

         for Index in 1 .. Projects.Last loop
            Projects.Table (Index).Seen := False;
         end loop;

         Add (Project);
         Projects.Table (Project).Objects_Path :=
           new String'(Ada_Path_Buffer (1 .. Ada_Path_Length));
      end if;

      return Projects.Table (Project).Objects_Path;
   end Ada_Objects_Path;

   -----------------
   -- Add_To_Path --
   -----------------

   procedure Add_To_Path (Path : String) is
   begin
      --  If Ada_Path_Buffer is too small, double it

      if Ada_Path_Length + Path'Length > Ada_Path_Buffer'Last then
         declare
            New_Ada_Path_Buffer : constant String_Access :=
                                    new String
                                      (1 .. Ada_Path_Buffer'Last +
                                                 Ada_Path_Buffer'Last);

         begin
            New_Ada_Path_Buffer (1 .. Ada_Path_Length) :=
              Ada_Path_Buffer (1 .. Ada_Path_Length);
            Ada_Path_Buffer := New_Ada_Path_Buffer;
         end;
      end if;

      Ada_Path_Buffer
        (Ada_Path_Length + 1 .. Ada_Path_Length + Path'Length) := Path;
      Ada_Path_Length := Ada_Path_Length + Path'Length;
   end Add_To_Path;

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
               String_To_Name_Buffer
                 (String_Elements.Table (Current_Source).Value);
               Path :=
                 Locate_Regular_File
                 (Namet.Get_Name_String
                  (Data.File_Names (Body_Part).Name),
                  Name_Buffer (1 .. Name_Len));

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
     (For_Project  : Project_Id;
      Main_Project : Project_Id)
   is
      File_Name : Temp_File_Name;
      File      : File_Descriptor := Invalid_FD;

      The_Packages : Package_Id;
      Gnatmake     : Prj.Package_Id;
      Compiler     : Prj.Package_Id;

      Current_Unit : Unit_Id := Units.First;

      First_Project : Project_List := Empty_Project_List;

      Current_Project : Project_List;
      Current_Naming  : Naming_Id;

      Global_Attribute : Variable_Value := Nil_Variable_Value;
      Local_Attribute  : Variable_Value := Nil_Variable_Value;

      Global_Attribute_Present : Boolean := False;
      Local_Attribute_Present  : Boolean := False;

      procedure Check (Project : Project_Id);

      procedure Check_Temp_File;
      --  Check that a temporary file has been opened.
      --  If not, create one, and put its name in the project data,
      --  with the indication that it is a temporary file.

      procedure Copy_File (Name : String_Id);
      --  Copy a configuration pragmas file into the temp file.

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
                 (File, "pragma Source_File_Name");
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
                 (File, "pragma Source_File_Name");
               Put_Line
                 (File, "  (Body_File_Name  => ""*" &
                  Namet.Get_Name_String (Data.Naming.Current_Impl_Suffix) &
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
                 Data.Naming.Current_Impl_Suffix /= Data.Naming.Separate_Suffix
               then
                  Put_Line
                    (File, "pragma Source_File_Name");
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

            if Data.Modifies /= No_Project then
               Check (Data.Modifies);
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
            GNAT.OS_Lib.Create_Temp_File (File, Name => File_Name);
            if File = Invalid_FD then
               Osint.Fail
                 ("unable to create temporary configuration pragmas file");
            elsif Opt.Verbose_Mode then
               Write_Str ("Creating temp file """);
               Write_Str (File_Name);
               Write_Line ("""");
            end if;
         end if;
      end Check_Temp_File;

      ---------------
      -- Copy_File --
      ---------------

      procedure Copy_File (Name : in String_Id) is
         Input         : File_Descriptor;
         Buffer        : String (1 .. 1_000);
         Input_Length  : Integer;
         Output_Length : Integer;

      begin
         Check_Temp_File;
         String_To_Name_Buffer (Name);

         if Opt.Verbose_Mode then
            Write_Str ("Copying config pragmas file """);
            Write_Str (Name_Buffer (1 .. Name_Len));
            Write_Line (""" into temp file");
         end if;

         declare
            Name : constant String :=
              Name_Buffer (1 .. Name_Len)  & ASCII.NUL;
         begin
            Input := Open_Read (Name'Address, Binary);
         end;

         if Input = Invalid_FD then
            Osint.Fail
              ("cannot open configuration pragmas file " &
               Name_Buffer (1 .. Name_Len));
         end if;

         loop
            Input_Length := Read (Input, Buffer'Address, Buffer'Length);
            Output_Length := Write (File, Buffer'Address, Input_Length);

            if Output_Length /= Input_Length then
               Osint.Fail ("disk full");
            end if;

            exit when Input_Length < Buffer'Length;
         end loop;

         Close (Input);

      end Copy_File;

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

         Put (File, "pragma Source_File_Name (");
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
            Osint.Fail ("Disk full");
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
         --  Add an ASCII.LF to the string. As this gnat.adc
         --  is supposed to be used only by the compiler, we don't
         --  care about the characters for the end of line.
         --  The truth is we could have put a space, but it is
         --  more convenient to be able to read gnat.adc during
         --  development. And the development was done under UNIX.
         --  Hence the ASCII.LF.

         S0 (1 .. S'Length) := S;
         S0 (S0'Last) := ASCII.LF;
         Last := Write (File, S0'Address, S0'Length);

         if Last /= S'Length + 1 then
            Osint.Fail ("Disk full");
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

         The_Packages := Projects.Table (Main_Project).Decl.Packages;
         Gnatmake :=
           Prj.Util.Value_Of
           (Name        => Name_Builder,
            In_Packages => The_Packages);

         if Gnatmake /= No_Package then
            Global_Attribute := Prj.Util.Value_Of
              (Variable_Name => Global_Configuration_Pragmas,
               In_Variables => Packages.Table (Gnatmake).Decl.Attributes);
            Global_Attribute_Present :=
              Global_Attribute /= Nil_Variable_Value
              and then String_Length (Global_Attribute.Value) > 0;
         end if;

         The_Packages := Projects.Table (For_Project).Decl.Packages;
         Compiler :=
           Prj.Util.Value_Of
           (Name        => Name_Compiler,
            In_Packages => The_Packages);

         if Compiler /= No_Package then
            Local_Attribute := Prj.Util.Value_Of
              (Variable_Name => Local_Configuration_Pragmas,
               In_Variables => Packages.Table (Compiler).Decl.Attributes);
            Local_Attribute_Present :=
              Local_Attribute /= Nil_Variable_Value
              and then String_Length (Local_Attribute.Value) > 0;
         end if;

         if Global_Attribute_Present then

            if File /= Invalid_FD
              or else Local_Attribute_Present
            then
               Copy_File (Global_Attribute.Value);
            else
               String_To_Name_Buffer (Global_Attribute.Value);
               Projects.Table (For_Project).Config_File_Name := Name_Find;
            end if;
         end if;

         if Local_Attribute_Present then

            if File /= Invalid_FD then
               Copy_File (Local_Attribute.Value);

            else
               String_To_Name_Buffer (Local_Attribute.Value);
               Projects.Table (For_Project).Config_File_Name := Name_Find;
            end if;

         end if;

         if File /= Invalid_FD then
            GNAT.OS_Lib.Close (File);

            if Opt.Verbose_Mode then
               Write_Str ("Closing configuration file """);
               Write_Str (File_Name);
               Write_Line ("""");
            end if;

            Name_Len := File_Name'Length;
            Name_Buffer (1 .. Name_Len) := File_Name;
            Projects.Table (For_Project).Config_File_Name := Name_Find;
            Projects.Table (For_Project).Config_File_Temp := True;
         end if;

         Projects.Table (For_Project).Config_Checked := True;

      end if;

   end Create_Config_Pragmas_File;

   -------------------------
   -- Create_Mapping_File --
   -------------------------

   procedure Create_Mapping_File (Name : in out Temp_File_Name) is
      File          : File_Descriptor := Invalid_FD;
      The_Unit_Data : Unit_Data;
      Data          : File_Name_Data;

      procedure Put (S : String);
      --  Put a line in the mapping file

      procedure Put_Data (Spec : Boolean);
      --  Put the mapping of the spec or body contained in Data in the file
      --  (3 lines).

      ---------
      -- Put --
      ---------

      procedure Put (S : String) is
         Last : Natural;

      begin
         Last := Write (File, S'Address, S'Length);

         if Last /= S'Length then
            Osint.Fail ("Disk full");
         end if;
      end Put;

      --------------
      -- Put_Data --
      --------------

      procedure Put_Data (Spec : Boolean) is
      begin
         Put (Get_Name_String (The_Unit_Data.Name));

         if Spec then
            Put ("%s");
         else
            Put ("%b");
         end if;

         Put (S => (1 => ASCII.LF));
         Put (Get_Name_String (Data.Name));
         Put (S => (1 => ASCII.LF));
         Put (Get_Name_String (Data.Path));
         Put (S => (1 => ASCII.LF));
      end Put_Data;

   --  Start of processing for Create_Mapping_File

   begin
      GNAT.OS_Lib.Create_Temp_File (File, Name => Name);

      if File = Invalid_FD then
         Osint.Fail
           ("unable to create temporary mapping file");

      elsif Opt.Verbose_Mode then
         Write_Str ("Creating temp mapping file """);
         Write_Str (Name);
         Write_Line ("""");
      end if;

      --  For all units in table Units

      for Unit in 1 .. Units.Last loop
         The_Unit_Data := Units.Table (Unit);

         --  If the unit has a valid name

         if The_Unit_Data.Name /= No_Name then
            Data := The_Unit_Data.File_Names (Specification);

            --  If there is a spec, put it mapping in the file

            if Data.Name /= No_Name then
               Put_Data (Spec => True);
            end if;

            Data := The_Unit_Data.File_Names (Body_Part);

            --  If there is a body (or subunit) put its mapping in the file

            if Data.Name /= No_Name then
               Put_Data (Spec => False);
            end if;

         end if;
      end loop;

      GNAT.OS_Lib.Close (File);

   end Create_Mapping_File;

   ------------------------------------
   -- File_Name_Of_Library_Unit_Body --
   ------------------------------------

   function File_Name_Of_Library_Unit_Body
     (Name    : String;
      Project : Project_Id)
      return    String
   is
      Data          : constant Project_Data := Projects.Table (Project);
      Original_Name : String := Name;

      Extended_Spec_Name : String :=
                             Name & Namet.Get_Name_String
                                      (Data.Naming.Current_Spec_Suffix);
      Extended_Body_Name : String :=
                             Name & Namet.Get_Name_String
                                      (Data.Naming.Current_Impl_Suffix);

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

      --  For every unit

      for Current in reverse Units.First .. Units.Last loop
         Unit := Units.Table (Current);

         --  Case of unit of the same project

         if Unit.File_Names (Body_Part).Project = Project then
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

                     return Get_Name_String (Current_Name);

                  --  If it has the name of the extended body name,
                  --  return the extended body name

                  elsif Current_Name = The_Body_Name then
                     if Current_Verbosity = High then
                        Write_Line ("   OK");
                     end if;

                     return Extended_Body_Name;

                  else
                     if Current_Verbosity = High then
                        Write_Line ("   not good");
                     end if;
                  end if;
               end if;
            end;
         end if;

         --  Case of a unit of the same project

         if Units.Table (Current).File_Names (Specification).Project =
                                                                 Project
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

                  --  If name same as the original name, return original name

                  if Unit.Name = The_Original_Name
                    or else Current_Name = The_Original_Name
                  then
                     if Current_Verbosity = High then
                        Write_Line ("   OK");
                     end if;

                     return Get_Name_String (Current_Name);

                  --  If it has the same name as the extended spec name,
                  --  return the extended spec name.

                  elsif Current_Name = The_Spec_Name then
                     if Current_Verbosity = High then
                        Write_Line ("   OK");
                     end if;

                     return Extended_Spec_Name;

                  else
                     if Current_Verbosity = High then
                        Write_Line ("   not good");
                     end if;
                  end if;
               end if;
            end;
         end if;

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
      --  modified project, and all imported projects.

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

         if Data.Modifies /= No_Project then
            Add (Data.Modifies);
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
      --  modified project, and all imported projects.

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
               String_To_Name_Buffer (The_String.Value);
               Action (Name_Buffer (1 .. Name_Len));
               Current := The_String.Next;
            end loop;
         end;

         --  If we are extending a project, visit it

         if Data.Modifies /= No_Project then
            Add (Data.Modifies);
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
               Project := Unit.File_Names (Specification).Project;
               Path := Unit.File_Names (Specification).Path;

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
               Project := Unit.File_Names (Body_Part).Project;
               Path := Unit.File_Names (Body_Part).Path;

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
      Global : constant String := "global_configuration_pragmas";
      Local  : constant String :=  "local_configuration_pragmas";
   begin
      --  Put the standard GNAT naming scheme in the Namings table

      Namings.Increment_Last;
      Namings.Table (Namings.Last) := Standard_Naming_Data;
      Name_Len := Global'Length;
      Name_Buffer (1 .. Name_Len) := Global;
      Global_Configuration_Pragmas := Name_Find;
      Name_Len := Local'Length;
      Name_Buffer (1 .. Name_Len) := Local;
      Local_Configuration_Pragmas := Name_Find;
   end Initialize;

   ------------------------------------
   -- Path_Name_Of_Library_Unit_Body --
   ------------------------------------

   function Path_Name_Of_Library_Unit_Body
     (Name    : String;
      Project : Project_Id)
      return String
   is
      Data : constant Project_Data := Projects.Table (Project);
      Original_Name : String := Name;

      Extended_Spec_Name : String :=
                             Name & Namet.Get_Name_String
                                     (Data.Naming.Current_Spec_Suffix);
      Extended_Body_Name : String :=
                             Name & Namet.Get_Name_String
                                     (Data.Naming.Current_Impl_Suffix);

      First   : Unit_Id := Units.First;
      Current : Unit_Id;
      Unit    : Unit_Data;

   begin
      Canonical_Case_File_Name (Original_Name);
      Canonical_Case_File_Name (Extended_Spec_Name);
      Canonical_Case_File_Name (Extended_Spec_Name);

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
               String_To_Name_Buffer
                 (String_Elements.Table (Current_Source).Value);
               Path := Locate_Regular_File
                 (Namet.Get_Name_String
                  (Data.File_Names (Specification).Name),
                  Name_Buffer (1 .. Name_Len));

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

end Prj.Env;
