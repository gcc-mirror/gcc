------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P R J                                   --
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

with Debug;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Attr;
with Prj.Err;  use Prj.Err;
with Snames;   use Snames;
with Uintp;    use Uintp;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.HTable;

package body Prj is

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  File suffix for object files

   Initial_Buffer_Size : constant := 100;
   --  Initial size for extensible buffer used in Add_To_Buffer

   The_Empty_String : Name_Id := No_Name;

   type Cst_String_Access is access constant String;

   All_Lower_Case_Image : aliased constant String := "lowercase";
   All_Upper_Case_Image : aliased constant String := "UPPERCASE";
   Mixed_Case_Image     : aliased constant String := "MixedCase";

   The_Casing_Images : constant array (Known_Casing) of Cst_String_Access :=
                         (All_Lower_Case => All_Lower_Case_Image'Access,
                          All_Upper_Case => All_Upper_Case_Image'Access,
                          Mixed_Case     => Mixed_Case_Image'Access);

   Project_Empty : constant Project_Data :=
                     (Qualifier                      => Unspecified,
                      Externally_Built               => False,
                      Config                         => Default_Project_Config,
                      Name                           => No_Name,
                      Display_Name                   => No_Name,
                      Path                           => No_Path_Information,
                      Virtual                        => False,
                      Location                       => No_Location,
                      Mains                          => Nil_String,
                      Directory                      => No_Path_Information,
                      Library                        => False,
                      Library_Dir                    => No_Path_Information,
                      Library_Src_Dir                => No_Path_Information,
                      Library_ALI_Dir                => No_Path_Information,
                      Library_Name                   => No_Name,
                      Library_Kind                   => Static,
                      Lib_Internal_Name              => No_Name,
                      Standalone_Library             => False,
                      Lib_Interface_ALIs             => Nil_String,
                      Lib_Auto_Init                  => False,
                      Libgnarl_Needed                => Unknown,
                      Symbol_Data                    => No_Symbols,
                      Interfaces_Defined             => False,
                      Source_Dirs                    => Nil_String,
                      Source_Dir_Ranks               => No_Number_List,
                      Object_Directory               => No_Path_Information,
                      Library_TS                     => Empty_Time_Stamp,
                      Exec_Directory                 => No_Path_Information,
                      Extends                        => No_Project,
                      Extended_By                    => No_Project,
                      Languages                      => No_Language_Index,
                      Decl                           => No_Declarations,
                      Imported_Projects              => null,
                      Include_Path_File              => No_Path,
                      All_Imported_Projects          => null,
                      Ada_Include_Path               => null,
                      Ada_Objects_Path               => null,
                      Objects_Path                   => null,
                      Objects_Path_File_With_Libs    => No_Path,
                      Objects_Path_File_Without_Libs => No_Path,
                      Config_File_Name               => No_Path,
                      Config_File_Temp               => False,
                      Config_Checked                 => False,
                      Need_To_Build_Lib              => False,
                      Has_Multi_Unit_Sources         => False,
                      Depth                          => 0,
                      Unkept_Comments                => False);

   procedure Free (Project : in out Project_Id);
   --  Free memory allocated for Project

   procedure Free_List (Languages : in out Language_Ptr);
   procedure Free_List (Source : in out Source_Id);
   procedure Free_List (Languages : in out Language_List);
   --  Free memory allocated for the list of languages or sources

   procedure Free_Units (Table : in out Units_Htable.Instance);
   --  Free memory allocated for unit information in the project

   procedure Language_Changed (Iter : in out Source_Iterator);
   procedure Project_Changed (Iter : in out Source_Iterator);
   --  Called when a new project or language was selected for this iterator

   function Contains_ALI_Files (Dir : Path_Name_Type) return Boolean;
   --  Return True if there is at least one ALI file in the directory Dir

   -------------------
   -- Add_To_Buffer --
   -------------------

   procedure Add_To_Buffer
     (S    : String;
      To   : in out String_Access;
      Last : in out Natural)
   is
   begin
      if To = null then
         To := new String (1 .. Initial_Buffer_Size);
         Last := 0;
      end if;

      --  If Buffer is too small, double its size

      while Last + S'Length > To'Last loop
         declare
            New_Buffer : constant  String_Access :=
                           new String (1 .. 2 * Last);

         begin
            New_Buffer (1 .. Last) := To (1 .. Last);
            Free (To);
            To := New_Buffer;
         end;
      end loop;

      To (Last + 1 .. Last + S'Length) := S;
      Last := Last + S'Length;
   end Add_To_Buffer;

   ---------------------------
   -- Delete_Temporary_File --
   ---------------------------

   procedure Delete_Temporary_File
     (Tree : Project_Tree_Ref;
      Path : Path_Name_Type)
   is
      Dont_Care : Boolean;
      pragma Warnings (Off, Dont_Care);

   begin
      if not Debug.Debug_Flag_N then
         if Current_Verbosity = High then
            Write_Line ("Removing temp file: " & Get_Name_String (Path));
         end if;

         Delete_File (Get_Name_String (Path), Dont_Care);

         for Index in
           1 .. Temp_Files_Table.Last (Tree.Private_Part.Temp_Files)
         loop
            if Tree.Private_Part.Temp_Files.Table (Index) = Path then
               Tree.Private_Part.Temp_Files.Table (Index) := No_Path;
            end if;
         end loop;
      end if;
   end Delete_Temporary_File;

   ---------------------------
   -- Delete_All_Temp_Files --
   ---------------------------

   procedure Delete_All_Temp_Files (Tree : Project_Tree_Ref) is
      Dont_Care : Boolean;
      pragma Warnings (Off, Dont_Care);

      Path : Path_Name_Type;

   begin
      if not Debug.Debug_Flag_N then
         for Index in
           1 .. Temp_Files_Table.Last (Tree.Private_Part.Temp_Files)
         loop
            Path := Tree.Private_Part.Temp_Files.Table (Index);

            if Path /= No_Path then
               if Current_Verbosity = High then
                  Write_Line ("Removing temp file: "
                              & Get_Name_String (Path));
               end if;

               Delete_File (Get_Name_String (Path), Dont_Care);
            end if;
         end loop;

         Temp_Files_Table.Free (Tree.Private_Part.Temp_Files);
         Temp_Files_Table.Init (Tree.Private_Part.Temp_Files);
      end if;

      --  If any of the environment variables ADA_PRJ_INCLUDE_FILE or
      --  ADA_PRJ_OBJECTS_FILE has been set, then reset their value to
      --  the empty string. On VMS, this has the effect of deassigning
      --  the logical names.

      if Tree.Private_Part.Current_Source_Path_File /= No_Path then
         Setenv (Project_Include_Path_File, "");
      end if;

      if Tree.Private_Part.Current_Object_Path_File /= No_Path then
         Setenv (Project_Objects_Path_File, "");
      end if;
   end Delete_All_Temp_Files;

   ---------------------
   -- Dependency_Name --
   ---------------------

   function Dependency_Name
     (Source_File_Name : File_Name_Type;
      Dependency       : Dependency_File_Kind) return File_Name_Type
   is
   begin
      case Dependency is
         when None =>
            return No_File;

         when Makefile =>
            return Extend_Name (Source_File_Name, Makefile_Dependency_Suffix);

         when ALI_File =>
            return Extend_Name (Source_File_Name, ALI_Dependency_Suffix);
      end case;
   end Dependency_Name;

   ----------------
   -- Empty_File --
   ----------------

   function Empty_File return File_Name_Type is
   begin
      return File_Name_Type (The_Empty_String);
   end Empty_File;

   -------------------
   -- Empty_Project --
   -------------------

   function Empty_Project return Project_Data is
   begin
      Prj.Initialize (Tree => No_Project_Tree);
      return Project_Empty;
   end Empty_Project;

   ------------------
   -- Empty_String --
   ------------------

   function Empty_String return Name_Id is
   begin
      return The_Empty_String;
   end Empty_String;

   ------------
   -- Expect --
   ------------

   procedure Expect (The_Token : Token_Type; Token_Image : String) is
   begin
      if Token /= The_Token then
         --  ??? Should pass user flags here instead
         Error_Msg (Gnatmake_Flags, Token_Image & " expected", Token_Ptr);
      end if;
   end Expect;

   -----------------
   -- Extend_Name --
   -----------------

   function Extend_Name
     (File        : File_Name_Type;
      With_Suffix : String) return File_Name_Type
   is
      Last : Positive;

   begin
      Get_Name_String (File);
      Last := Name_Len + 1;

      while Name_Len /= 0 and then Name_Buffer (Name_Len) /= '.' loop
         Name_Len := Name_Len - 1;
      end loop;

      if Name_Len <= 1 then
         Name_Len := Last;
      end if;

      for J in With_Suffix'Range loop
         Name_Buffer (Name_Len) := With_Suffix (J);
         Name_Len := Name_Len + 1;
      end loop;

      Name_Len := Name_Len - 1;
      return Name_Find;

   end Extend_Name;

   ---------------------
   -- Project_Changed --
   ---------------------

   procedure Project_Changed (Iter : in out Source_Iterator) is
   begin
      Iter.Language := Iter.Project.Project.Languages;
      Language_Changed (Iter);
   end Project_Changed;

   ----------------------
   -- Language_Changed --
   ----------------------

   procedure Language_Changed (Iter : in out Source_Iterator) is
   begin
      Iter.Current  := No_Source;

      if Iter.Language_Name /= No_Name then
         while Iter.Language /= null
           and then Iter.Language.Name /= Iter.Language_Name
         loop
            Iter.Language := Iter.Language.Next;
         end loop;
      end if;

      --  If there is no matching language in this project, move to next

      if Iter.Language = No_Language_Index then
         if Iter.All_Projects then
            Iter.Project := Iter.Project.Next;

            if Iter.Project /= null then
               Project_Changed (Iter);
            end if;

         else
            Iter.Project := null;
         end if;

      else
         Iter.Current := Iter.Language.First_Source;

         if Iter.Current = No_Source then
            Iter.Language := Iter.Language.Next;
            Language_Changed (Iter);
         end if;
      end if;
   end Language_Changed;

   ---------------------
   -- For_Each_Source --
   ---------------------

   function For_Each_Source
     (In_Tree  : Project_Tree_Ref;
      Project  : Project_Id := No_Project;
      Language : Name_Id := No_Name) return Source_Iterator
   is
      Iter : Source_Iterator;
   begin
      Iter := Source_Iterator'
        (In_Tree       => In_Tree,
         Project       => In_Tree.Projects,
         All_Projects  => Project = No_Project,
         Language_Name => Language,
         Language      => No_Language_Index,
         Current       => No_Source);

      if Project /= null then
         while Iter.Project /= null
           and then Iter.Project.Project /= Project
         loop
            Iter.Project := Iter.Project.Next;
         end loop;
      end if;

      Project_Changed (Iter);

      return Iter;
   end For_Each_Source;

   -------------
   -- Element --
   -------------

   function Element (Iter : Source_Iterator) return Source_Id is
   begin
      return Iter.Current;
   end Element;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Source_Iterator) is
   begin
      Iter.Current := Iter.Current.Next_In_Lang;
      if Iter.Current = No_Source then
         Iter.Language := Iter.Language.Next;
         Language_Changed (Iter);
      end if;
   end Next;

   --------------------------------
   -- For_Every_Project_Imported --
   --------------------------------

   procedure For_Every_Project_Imported
     (By             : Project_Id;
      With_State     : in out State;
      Imported_First : Boolean := False)
   is
      use Project_Boolean_Htable;
      Seen : Project_Boolean_Htable.Instance := Project_Boolean_Htable.Nil;

      procedure Recursive_Check (Project : Project_Id);
      --  Check if a project has already been seen. If not seen, mark it as
      --  Seen, Call Action, and check all its imported projects.

      ---------------------
      -- Recursive_Check --
      ---------------------

      procedure Recursive_Check (Project : Project_Id) is
         List : Project_List;

      begin
         if not Get (Seen, Project) then
            Set (Seen, Project, True);

            if not Imported_First then
               Action (Project, With_State);
            end if;

            --  Visited all extended projects

            if Project.Extends /= No_Project then
               Recursive_Check (Project.Extends);
            end if;

            --  Visited all imported projects

            List := Project.Imported_Projects;
            while List /= null loop
               Recursive_Check (List.Project);
               List := List.Next;
            end loop;

            if Imported_First then
               Action (Project, With_State);
            end if;
         end if;
      end Recursive_Check;

   --  Start of processing for For_Every_Project_Imported

   begin
      Recursive_Check (Project => By);
      Reset (Seen);
   end For_Every_Project_Imported;

   -----------------
   -- Find_Source --
   -----------------

   function Find_Source
     (In_Tree          : Project_Tree_Ref;
      Project          : Project_Id;
      In_Imported_Only : Boolean := False;
      In_Extended_Only : Boolean := False;
      Base_Name        : File_Name_Type) return Source_Id
   is
      Result : Source_Id  := No_Source;

      procedure Look_For_Sources (Proj : Project_Id; Src : in out Source_Id);
      --  Look for Base_Name in the sources of Proj

      ----------------------
      -- Look_For_Sources --
      ----------------------

      procedure Look_For_Sources (Proj : Project_Id; Src : in out Source_Id) is
         Iterator : Source_Iterator;

      begin
         Iterator := For_Each_Source (In_Tree => In_Tree, Project => Proj);
         while Element (Iterator) /= No_Source loop
            if Element (Iterator).File = Base_Name then
               Src := Element (Iterator);
               return;
            end if;

            Next (Iterator);
         end loop;
      end Look_For_Sources;

      procedure For_Imported_Projects is new For_Every_Project_Imported
        (State => Source_Id, Action => Look_For_Sources);

      Proj : Project_Id;

   --  Start of processing for Find_Source

   begin
      if In_Extended_Only then
         Proj := Project;
         while Proj /= No_Project loop
            Look_For_Sources (Proj, Result);
            exit when Result /= No_Source;

            Proj := Proj.Extends;
         end loop;

      elsif In_Imported_Only then
         Look_For_Sources (Project, Result);

         if Result = No_Source then
            For_Imported_Projects
              (By         => Project,
               With_State => Result);
         end if;
      else
         Look_For_Sources (No_Project, Result);
      end if;

      return Result;
   end Find_Source;

   ----------
   -- Hash --
   ----------

   function Hash is new GNAT.HTable.Hash (Header_Num => Header_Num);
   --  Used in implementation of other functions Hash below

   function Hash (Name : File_Name_Type) return Header_Num is
   begin
      return Hash (Get_Name_String (Name));
   end Hash;

   function Hash (Name : Name_Id) return Header_Num is
   begin
      return Hash (Get_Name_String (Name));
   end Hash;

   function Hash (Name : Path_Name_Type) return Header_Num is
   begin
      return Hash (Get_Name_String (Name));
   end Hash;

   function Hash (Project : Project_Id) return Header_Num is
   begin
      if Project = No_Project then
         return Header_Num'First;
      else
         return Hash (Get_Name_String (Project.Name));
      end if;
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (The_Casing : Casing_Type) return String is
   begin
      return The_Casing_Images (The_Casing).all;
   end Image;

   -----------------------------
   -- Is_Standard_GNAT_Naming --
   -----------------------------

   function Is_Standard_GNAT_Naming
     (Naming : Lang_Naming_Data) return Boolean
   is
   begin
      return Get_Name_String (Naming.Spec_Suffix) = ".ads"
        and then Get_Name_String (Naming.Body_Suffix) = ".adb"
        and then Get_Name_String (Naming.Dot_Replacement) = "-";
   end Is_Standard_GNAT_Naming;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Tree : Project_Tree_Ref) is
   begin
      if The_Empty_String = No_Name then
         Uintp.Initialize;
         Name_Len := 0;
         The_Empty_String := Name_Find;

         Prj.Attr.Initialize;

         Set_Name_Table_Byte
           (Name_Project,          Token_Type'Pos (Tok_Project));
         Set_Name_Table_Byte
           (Name_Extends,          Token_Type'Pos (Tok_Extends));
         Set_Name_Table_Byte
           (Name_External,         Token_Type'Pos (Tok_External));
         Set_Name_Table_Byte
           (Name_External_As_List, Token_Type'Pos (Tok_External_As_List));
      end if;

      if Tree /= No_Project_Tree then
         Reset (Tree);
      end if;
   end Initialize;

   ------------------
   -- Is_Extending --
   ------------------

   function Is_Extending
     (Extending : Project_Id;
      Extended  : Project_Id) return Boolean
   is
      Proj : Project_Id;

   begin
      Proj := Extending;
      while Proj /= No_Project loop
         if Proj = Extended then
            return True;
         end if;

         Proj := Proj.Extends;
      end loop;

      return False;
   end Is_Extending;

   -----------------
   -- Object_Name --
   -----------------

   function Object_Name
     (Source_File_Name   : File_Name_Type;
      Object_File_Suffix : Name_Id := No_Name) return File_Name_Type
   is
   begin
      if Object_File_Suffix = No_Name then
         return Extend_Name
           (Source_File_Name, Object_Suffix);
      else
         return Extend_Name
           (Source_File_Name, Get_Name_String (Object_File_Suffix));
      end if;
   end Object_Name;

   function Object_Name
     (Source_File_Name   : File_Name_Type;
      Source_Index       : Int;
      Index_Separator    : Character;
      Object_File_Suffix : Name_Id := No_Name) return File_Name_Type
   is
      Index_Img : constant String := Source_Index'Img;
      Last      : Natural;

   begin
      Get_Name_String (Source_File_Name);

      Last := Name_Len;
      while Last > 1 and then Name_Buffer (Last) /= '.' loop
         Last := Last - 1;
      end loop;

      if Last > 1 then
         Name_Len := Last - 1;
      end if;

      Add_Char_To_Name_Buffer (Index_Separator);
      Add_Str_To_Name_Buffer (Index_Img (2 .. Index_Img'Last));

      if Object_File_Suffix = No_Name then
         Add_Str_To_Name_Buffer (Object_Suffix);
      else
         Add_Str_To_Name_Buffer (Get_Name_String (Object_File_Suffix));
      end if;

      return Name_Find;
   end Object_Name;

   ----------------------
   -- Record_Temp_File --
   ----------------------

   procedure Record_Temp_File
     (Tree : Project_Tree_Ref;
      Path : Path_Name_Type)
   is
   begin
      Temp_Files_Table.Append (Tree.Private_Part.Temp_Files, Path);
   end Record_Temp_File;

   ----------
   -- Free --
   ----------

   procedure Free (Project : in out Project_Id) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Data, Project_Id);

   begin
      if Project /= null then
         Free (Project.Ada_Include_Path);
         Free (Project.Objects_Path);
         Free (Project.Ada_Objects_Path);
         Free_List (Project.Imported_Projects, Free_Project => False);
         Free_List (Project.All_Imported_Projects, Free_Project => False);
         Free_List (Project.Languages);

         Unchecked_Free (Project);
      end if;
   end Free;

   ---------------
   -- Free_List --
   ---------------

   procedure Free_List (Languages : in out Language_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Language_List_Element, Language_List);
      Tmp : Language_List;
   begin
      while Languages /= null loop
         Tmp := Languages.Next;
         Unchecked_Free (Languages);
         Languages := Tmp;
      end loop;
   end Free_List;

   ---------------
   -- Free_List --
   ---------------

   procedure Free_List (Source : in out Source_Id) is
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Source_Data, Source_Id);

      Tmp : Source_Id;

   begin
      while Source /= No_Source loop
         Tmp := Source.Next_In_Lang;
         Free_List (Source.Alternate_Languages);

         if Source.Unit /= null
           and then Source.Kind in Spec_Or_Body
         then
            Source.Unit.File_Names (Source.Kind) := null;
         end if;

         Unchecked_Free (Source);
         Source := Tmp;
      end loop;
   end Free_List;

   ---------------
   -- Free_List --
   ---------------

   procedure Free_List
     (List         : in out Project_List;
      Free_Project : Boolean)
   is
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Project_List_Element, Project_List);

      Tmp : Project_List;

   begin
      while List /= null loop
         Tmp := List.Next;

         if Free_Project then
            Free (List.Project);
         end if;

         Unchecked_Free (List);
         List := Tmp;
      end loop;
   end Free_List;

   ---------------
   -- Free_List --
   ---------------

   procedure Free_List (Languages : in out Language_Ptr) is
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Language_Data, Language_Ptr);

      Tmp : Language_Ptr;

   begin
      while Languages /= null loop
         Tmp := Languages.Next;
         Free_List (Languages.First_Source);
         Unchecked_Free (Languages);
         Languages := Tmp;
      end loop;
   end Free_List;

   ----------------
   -- Free_Units --
   ----------------

   procedure Free_Units (Table : in out Units_Htable.Instance) is
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Unit_Data, Unit_Index);

      Unit : Unit_Index;

   begin
      Unit := Units_Htable.Get_First (Table);
      while Unit /= No_Unit_Index loop
         if Unit.File_Names (Spec) /= null then
            Unit.File_Names (Spec).Unit := No_Unit_Index;
         end if;

         if Unit.File_Names (Impl) /= null then
            Unit.File_Names (Impl).Unit := No_Unit_Index;
         end if;

         Unchecked_Free (Unit);
         Unit := Units_Htable.Get_Next (Table);
      end loop;

      Units_Htable.Reset (Table);
   end Free_Units;

   ----------
   -- Free --
   ----------

   procedure Free (Tree : in out Project_Tree_Ref) is
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Project_Tree_Data, Project_Tree_Ref);

   begin
      if Tree /= null then
         Name_List_Table.Free (Tree.Name_Lists);
         Number_List_Table.Free (Tree.Number_Lists);
         String_Element_Table.Free (Tree.String_Elements);
         Variable_Element_Table.Free (Tree.Variable_Elements);
         Array_Element_Table.Free (Tree.Array_Elements);
         Array_Table.Free (Tree.Arrays);
         Package_Table.Free (Tree.Packages);
         Source_Paths_Htable.Reset (Tree.Source_Paths_HT);
         Source_Files_Htable.Reset (Tree.Source_Files_HT);

         Free_List (Tree.Projects, Free_Project => True);
         Free_Units (Tree.Units_HT);

         --  Private part

         Temp_Files_Table.Free  (Tree.Private_Part.Temp_Files);

         Unchecked_Free (Tree);
      end if;
   end Free;

   -----------
   -- Reset --
   -----------

   procedure Reset (Tree : Project_Tree_Ref) is
   begin
      --  Visible tables

      Name_List_Table.Init          (Tree.Name_Lists);
      Number_List_Table.Init        (Tree.Number_Lists);
      String_Element_Table.Init     (Tree.String_Elements);
      Variable_Element_Table.Init   (Tree.Variable_Elements);
      Array_Element_Table.Init      (Tree.Array_Elements);
      Array_Table.Init              (Tree.Arrays);
      Package_Table.Init            (Tree.Packages);
      Source_Paths_Htable.Reset     (Tree.Source_Paths_HT);
      Source_Files_Htable.Reset     (Tree.Source_Files_HT);
      Replaced_Source_HTable.Reset  (Tree.Replaced_Sources);

      Tree.Replaced_Source_Number := 0;

      Free_List (Tree.Projects, Free_Project => True);
      Free_Units (Tree.Units_HT);

      --  Private part table

      Temp_Files_Table.Init       (Tree.Private_Part.Temp_Files);

      Tree.Private_Part.Current_Source_Path_File := No_Path;
      Tree.Private_Part.Current_Object_Path_File := No_Path;
   end Reset;

   -------------------
   -- Switches_Name --
   -------------------

   function Switches_Name
     (Source_File_Name : File_Name_Type) return File_Name_Type
   is
   begin
      return Extend_Name (Source_File_Name, Switches_Dependency_Suffix);
   end Switches_Name;

   -----------
   -- Value --
   -----------

   function Value (Image : String) return Casing_Type is
   begin
      for Casing in The_Casing_Images'Range loop
         if To_Lower (Image) = To_Lower (The_Casing_Images (Casing).all) then
            return Casing;
         end if;
      end loop;

      raise Constraint_Error;
   end Value;

   ---------------------
   -- Has_Ada_Sources --
   ---------------------

   function Has_Ada_Sources (Data : Project_Id) return Boolean is
      Lang : Language_Ptr;

   begin
      Lang := Data.Languages;
      while Lang /= No_Language_Index loop
         if Lang.Name = Name_Ada then
            return Lang.First_Source /= No_Source;
         end if;
         Lang := Lang.Next;
      end loop;

      return False;
   end Has_Ada_Sources;

   ------------------------
   -- Contains_ALI_Files --
   ------------------------

   function Contains_ALI_Files (Dir : Path_Name_Type) return Boolean is
      Dir_Name : constant String := Get_Name_String (Dir);
      Direct   : Dir_Type;
      Name     : String (1 .. 1_000);
      Last     : Natural;
      Result   : Boolean := False;

   begin
      Open (Direct, Dir_Name);

      --  For each file in the directory, check if it is an ALI file

      loop
         Read (Direct, Name, Last);
         exit when Last = 0;
         Canonical_Case_File_Name (Name (1 .. Last));
         Result := Last >= 5 and then Name (Last - 3 .. Last) = ".ali";
         exit when Result;
      end loop;

      Close (Direct);
      return Result;

   exception
      --  If there is any problem, close the directory if open and return True.
      --  The library directory will be added to the path.

      when others =>
         if Is_Open (Direct) then
            Close (Direct);
         end if;

         return True;
   end Contains_ALI_Files;

   --------------------------
   -- Get_Object_Directory --
   --------------------------

   function Get_Object_Directory
     (Project             : Project_Id;
      Including_Libraries : Boolean;
      Only_If_Ada         : Boolean := False) return Path_Name_Type
   is
   begin
      if (Project.Library and then Including_Libraries)
        or else
          (Project.Object_Directory /= No_Path_Information
            and then (not Including_Libraries or else not Project.Library))
      then
         --  For a library project, add the library ALI directory if there is
         --  no object directory or if the library ALI directory contains ALI
         --  files; otherwise add the object directory.

         if Project.Library then
            if Project.Object_Directory = No_Path_Information
              or else Contains_ALI_Files (Project.Library_ALI_Dir.Display_Name)
            then
               return Project.Library_ALI_Dir.Display_Name;
            else
               return Project.Object_Directory.Display_Name;
            end if;

            --  For a non-library project, add object directory if it is not a
            --  virtual project, and if there are Ada sources in the project or
            --  one of the projects it extends. If there are no Ada sources,
            --  adding the object directory could disrupt the order of the
            --  object dirs in the path.

         elsif not Project.Virtual then
            declare
               Add_Object_Dir : Boolean;
               Prj            : Project_Id;

            begin
               Add_Object_Dir := not Only_If_Ada;
               Prj := Project;
               while not Add_Object_Dir and then Prj /= No_Project loop
                  if Has_Ada_Sources (Prj) then
                     Add_Object_Dir := True;
                  else
                     Prj := Prj.Extends;
                  end if;
               end loop;

               if Add_Object_Dir then
                  return Project.Object_Directory.Display_Name;
               end if;
            end;
         end if;
      end if;

      return No_Path;
   end Get_Object_Directory;

   -----------------------------------
   -- Ultimate_Extending_Project_Of --
   -----------------------------------

   function Ultimate_Extending_Project_Of
     (Proj : Project_Id) return Project_Id
   is
      Prj : Project_Id;

   begin
      Prj := Proj;
      while Prj /= null and then Prj.Extended_By /= No_Project loop
         Prj := Prj.Extended_By;
      end loop;

      return Prj;
   end Ultimate_Extending_Project_Of;

   -----------------------------------
   -- Compute_All_Imported_Projects --
   -----------------------------------

   procedure Compute_All_Imported_Projects (Tree : Project_Tree_Ref) is
      Project : Project_Id;

      procedure Recursive_Add (Prj : Project_Id; Dummy : in out Boolean);
      --  Recursively add the projects imported by project Project, but not
      --  those that are extended.

      -------------------
      -- Recursive_Add --
      -------------------

      procedure Recursive_Add (Prj : Project_Id; Dummy : in out Boolean) is
         pragma Unreferenced (Dummy);
         List    : Project_List;
         Prj2    : Project_Id;

      begin
         --  A project is not importing itself

         Prj2 := Ultimate_Extending_Project_Of (Prj);

         if Project /= Prj2 then

            --  Check that the project is not already in the list. We know the
            --  one passed to Recursive_Add have never been visited before, but
            --  the one passed it are the extended projects.

            List := Project.All_Imported_Projects;
            while List /= null loop
               if List.Project = Prj2 then
                  return;
               end if;

               List := List.Next;
            end loop;

            --  Add it to the list

            Project.All_Imported_Projects :=
              new Project_List_Element'
                (Project => Prj2,
                 Next    => Project.All_Imported_Projects);
         end if;
      end Recursive_Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Recursive_Add);

      Dummy : Boolean := False;
      List  : Project_List;

   begin
      List := Tree.Projects;
      while List /= null loop
         Project := List.Project;
         Free_List (Project.All_Imported_Projects, Free_Project => False);
         For_All_Projects (Project, Dummy);
         List := List.Next;
      end loop;
   end Compute_All_Imported_Projects;

   -------------------
   -- Is_Compilable --
   -------------------

   function Is_Compilable (Source : Source_Id) return Boolean is
   begin
      return Source.Language.Config.Compiler_Driver /= No_File
        and then Length_Of_Name (Source.Language.Config.Compiler_Driver) /= 0
        and then not Source.Locally_Removed
        and then (Source.Language.Config.Kind /= File_Based
                    or else
                  Source.Kind /= Spec);
   end Is_Compilable;

   ------------------------------
   -- Object_To_Global_Archive --
   ------------------------------

   function Object_To_Global_Archive (Source : Source_Id) return Boolean is
   begin
      return Source.Language.Config.Kind = File_Based
        and then Source.Kind = Impl
        and then Source.Language.Config.Objects_Linked
        and then Is_Compilable (Source)
        and then Source.Language.Config.Object_Generated;
   end Object_To_Global_Archive;

   ----------------------------
   -- Get_Language_From_Name --
   ----------------------------

   function Get_Language_From_Name
     (Project : Project_Id;
      Name    : String) return Language_Ptr
   is
      N      : Name_Id;
      Result : Language_Ptr;

   begin
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      To_Lower (Name_Buffer (1 .. Name_Len));
      N := Name_Find;

      Result := Project.Languages;
      while Result /= No_Language_Index loop
         if Result.Name = N then
            return Result;
         end if;

         Result := Result.Next;
      end loop;

      return No_Language_Index;
   end Get_Language_From_Name;

   ----------------
   -- Other_Part --
   ----------------

   function Other_Part (Source : Source_Id) return Source_Id is
   begin
      if Source.Unit /= No_Unit_Index then
         case Source.Kind is
            when Impl =>
               return Source.Unit.File_Names (Spec);
            when Spec =>
               return Source.Unit.File_Names (Impl);
            when Sep =>
               return No_Source;
         end case;
      else
         return No_Source;
      end if;
   end Other_Part;

   ------------------
   -- Create_Flags --
   ------------------

   function Create_Flags
     (Report_Error               : Error_Handler;
      When_No_Sources            : Error_Warning;
      Require_Sources_Other_Lang : Boolean       := True;
      Allow_Duplicate_Basenames  : Boolean       := True;
      Compiler_Driver_Mandatory  : Boolean       := False;
      Error_On_Unknown_Language  : Boolean       := True;
      Require_Obj_Dirs           : Error_Warning := Error;
      Allow_Invalid_External     : Error_Warning := Error;
      Missing_Source_Files       : Error_Warning := Error)
      return Processing_Flags
   is
   begin
      return Processing_Flags'
        (Report_Error               => Report_Error,
         When_No_Sources            => When_No_Sources,
         Require_Sources_Other_Lang => Require_Sources_Other_Lang,
         Allow_Duplicate_Basenames  => Allow_Duplicate_Basenames,
         Error_On_Unknown_Language  => Error_On_Unknown_Language,
         Compiler_Driver_Mandatory  => Compiler_Driver_Mandatory,
         Require_Obj_Dirs           => Require_Obj_Dirs,
         Allow_Invalid_External     => Allow_Invalid_External,
         Missing_Source_Files       => Missing_Source_Files);
   end Create_Flags;

   ------------
   -- Length --
   ------------

   function Length
     (Table : Name_List_Table.Instance;
      List  : Name_List_Index) return Natural
   is
      Count : Natural := 0;
      Tmp   : Name_List_Index;

   begin
      Tmp := List;
      while Tmp /= No_Name_List loop
         Count := Count + 1;
         Tmp := Table.Table (Tmp).Next;
      end loop;

      return Count;
   end Length;

begin
   --  Make sure that the standard config and user project file extensions are
   --  compatible with canonical case file naming.

   Canonical_Case_File_Name (Config_Project_File_Extension);
   Canonical_Case_File_Name (Project_File_Extension);
end Prj;
