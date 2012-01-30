------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P R J                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2012, Free Software Foundation, Inc.         --
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
with Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Attr;
with Prj.Com;
with Prj.Err;  use Prj.Err;
with Snames;   use Snames;
with Uintp;    use Uintp;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;
with Ada.Unchecked_Deallocation;

with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.HTable;

package body Prj is

   type Restricted_Lang;
   type Restricted_Lang_Access is access Restricted_Lang;
   type Restricted_Lang is record
      Name : Name_Id;
      Next : Restricted_Lang_Access;
   end record;

   Restricted_Languages : Restricted_Lang_Access := null;
   --  When null, all languages are allowed, otherwise only the languages in
   --  the list are allowed.

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  File suffix for object files

   Initial_Buffer_Size : constant := 100;
   --  Initial size for extensible buffer used in Add_To_Buffer

   The_Empty_String : Name_Id := No_Name;

   Debug_Level : Integer := 0;
   --  Current indentation level for debug traces

   type Cst_String_Access is access constant String;

   All_Lower_Case_Image : aliased constant String := "lowercase";
   All_Upper_Case_Image : aliased constant String := "UPPERCASE";
   Mixed_Case_Image     : aliased constant String := "MixedCase";

   The_Casing_Images : constant array (Known_Casing) of Cst_String_Access :=
                         (All_Lower_Case => All_Lower_Case_Image'Access,
                          All_Upper_Case => All_Upper_Case_Image'Access,
                          Mixed_Case     => Mixed_Case_Image'Access);

   procedure Free (Project : in out Project_Id);
   --  Free memory allocated for Project

   procedure Free_List (Languages : in out Language_Ptr);
   procedure Free_List (Source : in out Source_Id);
   procedure Free_List (Languages : in out Language_List);
   --  Free memory allocated for the list of languages or sources

   procedure Reset_Units_In_Table (Table : in out Units_Htable.Instance);
   --  Resets all Units to No_Unit_Index Unit.File_Names (Spec).Unit &
   --  Unit.File_Names (Impl).Unit in the given table.

   procedure Free_Units (Table : in out Units_Htable.Instance);
   --  Free memory allocated for unit information in the project

   procedure Language_Changed (Iter : in out Source_Iterator);
   procedure Project_Changed (Iter : in out Source_Iterator);
   --  Called when a new project or language was selected for this iterator

   function Contains_ALI_Files (Dir : Path_Name_Type) return Boolean;
   --  Return True if there is at least one ALI file in the directory Dir

   -----------------------------
   -- Add_Restricted_Language --
   -----------------------------

   procedure Add_Restricted_Language (Name : String) is
      N : String (1 .. Name'Length) := Name;
   begin
      To_Lower (N);
      Name_Len := 0;
      Add_Str_To_Name_Buffer (N);
      Restricted_Languages :=
        new Restricted_Lang'(Name => Name_Find, Next => Restricted_Languages);
   end Add_Restricted_Language;

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

   ---------------------------------
   -- Current_Object_Path_File_Of --
   ---------------------------------

   function Current_Object_Path_File_Of
     (Shared : Shared_Project_Tree_Data_Access) return Path_Name_Type
   is
   begin
      return Shared.Private_Part.Current_Object_Path_File;
   end Current_Object_Path_File_Of;

   ---------------------------------
   -- Current_Source_Path_File_Of --
   ---------------------------------

   function Current_Source_Path_File_Of
     (Shared : Shared_Project_Tree_Data_Access)
      return Path_Name_Type is
   begin
      return Shared.Private_Part.Current_Source_Path_File;
   end Current_Source_Path_File_Of;

   ---------------------------
   -- Delete_Temporary_File --
   ---------------------------

   procedure Delete_Temporary_File
     (Shared : Shared_Project_Tree_Data_Access := null;
      Path   : Path_Name_Type)
   is
      Dont_Care : Boolean;
      pragma Warnings (Off, Dont_Care);

   begin
      if not Debug.Debug_Flag_N then
         if Current_Verbosity = High then
            Write_Line ("Removing temp file: " & Get_Name_String (Path));
         end if;

         Delete_File (Get_Name_String (Path), Dont_Care);

         if Shared /= null then
            for Index in
              1 .. Temp_Files_Table.Last (Shared.Private_Part.Temp_Files)
            loop
               if Shared.Private_Part.Temp_Files.Table (Index) = Path then
                  Shared.Private_Part.Temp_Files.Table (Index) := No_Path;
               end if;
            end loop;
         end if;
      end if;
   end Delete_Temporary_File;

   ------------------------------
   -- Delete_Temp_Config_Files --
   ------------------------------

   procedure Delete_Temp_Config_Files (Project_Tree : Project_Tree_Ref) is
      Success : Boolean;
      pragma Warnings (Off, Success);

      Proj : Project_List;

   begin
      if not Debug.Debug_Flag_N then
         if Project_Tree /= null then
            Proj := Project_Tree.Projects;
            while Proj /= null loop
               if Proj.Project.Config_File_Temp then
                  Delete_Temporary_File
                    (Project_Tree.Shared, Proj.Project.Config_File_Name);

                  --  Make sure that we don't have a config file for this
                  --  project, in case there are several mains. In this case,
                  --  we will recreate another config file: we cannot reuse the
                  --  one that we just deleted!

                  Proj.Project.Config_Checked   := False;
                  Proj.Project.Config_File_Name := No_Path;
                  Proj.Project.Config_File_Temp := False;
               end if;

               Proj := Proj.Next;
            end loop;
         end if;
      end if;
   end Delete_Temp_Config_Files;

   ---------------------------
   -- Delete_All_Temp_Files --
   ---------------------------

   procedure Delete_All_Temp_Files
     (Shared : Shared_Project_Tree_Data_Access)
   is
      Dont_Care : Boolean;
      pragma Warnings (Off, Dont_Care);

      Path : Path_Name_Type;

   begin
      if not Debug.Debug_Flag_N then
         for Index in
           1 .. Temp_Files_Table.Last (Shared.Private_Part.Temp_Files)
         loop
            Path := Shared.Private_Part.Temp_Files.Table (Index);

            if Path /= No_Path then
               if Current_Verbosity = High then
                  Write_Line ("Removing temp file: "
                              & Get_Name_String (Path));
               end if;

               Delete_File (Get_Name_String (Path), Dont_Care);
            end if;
         end loop;

         Temp_Files_Table.Free (Shared.Private_Part.Temp_Files);
         Temp_Files_Table.Init (Shared.Private_Part.Temp_Files);
      end if;

      --  If any of the environment variables ADA_PRJ_INCLUDE_FILE or
      --  ADA_PRJ_OBJECTS_FILE has been set, then reset their value to
      --  the empty string. On VMS, this has the effect of deassigning
      --  the logical names.

      if Shared.Private_Part.Current_Source_Path_File /= No_Path then
         Setenv (Project_Include_Path_File, "");
      end if;

      if Shared.Private_Part.Current_Object_Path_File /= No_Path then
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

   function Empty_Project
     (Qualifier : Project_Qualifier) return Project_Data
   is
   begin
      Prj.Initialize (Tree => No_Project_Tree);

      declare
         Data : Project_Data (Qualifier => Qualifier);

      begin
         --  Only the fields for which no default value could be provided in
         --  prj.ads are initialized below.

         Data.Config := Default_Project_Config;
         return Data;
      end;
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

   -------------------------
   -- Is_Allowed_Language --
   -------------------------

   function Is_Allowed_Language (Name : Name_Id) return Boolean is
      R    : Restricted_Lang_Access := Restricted_Languages;
      Lang : constant String := Get_Name_String (Name);

   begin
      if R = null then
         return True;

      else
         while R /= null loop
            if Get_Name_String (R.Name) = Lang then
               return True;
            end if;

            R := R.Next;
         end loop;

         return False;
      end if;
   end Is_Allowed_Language;

   ---------------------
   -- Project_Changed --
   ---------------------

   procedure Project_Changed (Iter : in out Source_Iterator) is
   begin
      if Iter.Project /= null then
         Iter.Language := Iter.Project.Project.Languages;
         Language_Changed (Iter);
      end if;
   end Project_Changed;

   ----------------------
   -- Language_Changed --
   ----------------------

   procedure Language_Changed (Iter : in out Source_Iterator) is
   begin
      Iter.Current := No_Source;

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
            Project_Changed (Iter);
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

   procedure For_Every_Project_Imported_Context
     (By                 : Project_Id;
      Tree               : Project_Tree_Ref;
      With_State         : in out State;
      Include_Aggregated : Boolean := True;
      Imported_First     : Boolean := False)
   is
      use Project_Boolean_Htable;

      procedure Recursive_Check_Context
        (Project               : Project_Id;
         Tree                  : Project_Tree_Ref;
         In_Aggregate_Lib      : Boolean;
         From_Encapsulated_Lib : Boolean);
      --  Recursively handle the project tree creating a new context for
      --  keeping track about already handled projects.

      -----------------------------
      -- Recursive_Check_Context --
      -----------------------------

      procedure Recursive_Check_Context
        (Project               : Project_Id;
         Tree                  : Project_Tree_Ref;
         In_Aggregate_Lib      : Boolean;
         From_Encapsulated_Lib : Boolean)
      is
         package Name_Id_Set is
           new Ada.Containers.Ordered_Sets (Element_Type => Name_Id);

         Seen_Name : Name_Id_Set.Set;
         --  This set is needed to ensure that we do not haandle the same
         --  project twice in the context of aggregate libraries.

         procedure Recursive_Check
           (Project               : Project_Id;
            Tree                  : Project_Tree_Ref;
            In_Aggregate_Lib      : Boolean;
            From_Encapsulated_Lib : Boolean);
         --  Check if project has already been seen. If not, mark it as Seen,
         --  Call Action, and check all its imported and aggregated projects.

         ---------------------
         -- Recursive_Check --
         ---------------------

         procedure Recursive_Check
           (Project               : Project_Id;
            Tree                  : Project_Tree_Ref;
            In_Aggregate_Lib      : Boolean;
            From_Encapsulated_Lib : Boolean)
         is
            List : Project_List;
            T    : Project_Tree_Ref;

         begin
            if not Seen_Name.Contains (Project.Name) then

               --  Even if a project is aggregated multiple times in an
               --  aggregated library, we will only return it once.

               Seen_Name.Include (Project.Name);

               if not Imported_First then
                  Action
                    (Project,
                     Tree,
                     Project_Context'(In_Aggregate_Lib, From_Encapsulated_Lib),
                     With_State);
               end if;

               --  Visit all extended projects

               if Project.Extends /= No_Project then
                  Recursive_Check
                    (Project.Extends, Tree,
                     In_Aggregate_Lib, From_Encapsulated_Lib);
               end if;

               --  Visit all imported projects

               List := Project.Imported_Projects;
               while List /= null loop
                  Recursive_Check
                    (List.Project, Tree,
                     In_Aggregate_Lib,
                     From_Encapsulated_Lib
                       or (Project.Standalone_Library = Encapsulated));
                  List := List.Next;
               end loop;

               --  Visit all aggregated projects

               if Include_Aggregated
                 and then Project.Qualifier in Aggregate_Project
               then
                  declare
                     Agg : Aggregated_Project_List;

                  begin
                     Agg := Project.Aggregated_Projects;
                     while Agg /= null loop
                        pragma Assert (Agg.Project /= No_Project);

                        --  For aggregated libraries, the tree must be the one
                        --  of the aggregate library.

                        if Project.Qualifier = Aggregate_Library then
                           T := Tree;
                           Recursive_Check
                             (Agg.Project, T,
                              True,
                              From_Encapsulated_Lib or
                                Project.Standalone_Library = Encapsulated);

                        else
                           T := Agg.Tree;

                           --  Use a new context as we want to returns the same
                           --  project in different project tree for aggregated
                           --  projects.

                           Recursive_Check_Context
                             (Agg.Project, T, False, False);
                        end if;

                        Agg := Agg.Next;
                     end loop;
                  end;
               end if;

               if Imported_First then
                  Action
                    (Project,
                     Tree,
                     Project_Context'(In_Aggregate_Lib, From_Encapsulated_Lib),
                     With_State);
               end if;
            end if;
         end Recursive_Check;

      --  Start of processing for Recursive_Check_Context

      begin
         Recursive_Check
           (Project, Tree, In_Aggregate_Lib, From_Encapsulated_Lib);
      end Recursive_Check_Context;

   --  Start of processing for For_Every_Project_Imported

   begin
      Recursive_Check_Context
        (Project               => By,
         Tree                  => Tree,
         In_Aggregate_Lib      => False,
         From_Encapsulated_Lib => False);
   end For_Every_Project_Imported_Context;

   procedure For_Every_Project_Imported
     (By                 : Project_Id;
      Tree               : Project_Tree_Ref;
      With_State         : in out State;
      Include_Aggregated : Boolean := True;
      Imported_First     : Boolean := False)
   is
      procedure Internal
        (Project    : Project_Id;
         Tree       : Project_Tree_Ref;
         Context    : Project_Context;
         With_State : in out State);
      --  Action wrapper for handling the context

      --------------
      -- Internal --
      --------------

      procedure Internal
        (Project    : Project_Id;
         Tree       : Project_Tree_Ref;
         Context    : Project_Context;
         With_State : in out State)
      is
         pragma Unreferenced (Context);
      begin
         Action (Project, Tree, With_State);
      end Internal;

      procedure For_Projects is
        new For_Every_Project_Imported_Context (State, Internal);

   begin
      For_Projects (By, Tree, With_State, Include_Aggregated, Imported_First);
   end For_Every_Project_Imported;

   -----------------
   -- Find_Source --
   -----------------

   function Find_Source
     (In_Tree          : Project_Tree_Ref;
      Project          : Project_Id;
      In_Imported_Only : Boolean := False;
      In_Extended_Only : Boolean := False;
      Base_Name        : File_Name_Type;
      Index            : Int := 0) return Source_Id
   is
      Result : Source_Id  := No_Source;

      procedure Look_For_Sources
        (Proj : Project_Id;
         Tree : Project_Tree_Ref;
         Src  : in out Source_Id);
      --  Look for Base_Name in the sources of Proj

      ----------------------
      -- Look_For_Sources --
      ----------------------

      procedure Look_For_Sources
        (Proj : Project_Id;
         Tree : Project_Tree_Ref;
         Src  : in out Source_Id)
      is
         Iterator : Source_Iterator;

      begin
         Iterator := For_Each_Source (In_Tree => Tree, Project => Proj);
         while Element (Iterator) /= No_Source loop
            if Element (Iterator).File = Base_Name
              and then (Index = 0 or else Element (Iterator).Index = Index)
            then
               Src := Element (Iterator);

               --  If the source has been excluded, continue looking. We will
               --  get the excluded source only if there is no other source
               --  with the same base name that is not locally removed.

               if not Element (Iterator).Locally_Removed then
                  return;
               end if;
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
            Look_For_Sources (Proj, In_Tree, Result);
            exit when Result /= No_Source;

            Proj := Proj.Extends;
         end loop;

      elsif In_Imported_Only then
         Look_For_Sources (Project, In_Tree, Result);

         if Result = No_Source then
            For_Imported_Projects
              (By                 => Project,
               Tree               => In_Tree,
               Include_Aggregated => False,
               With_State         => Result);
         end if;

      else
         Look_For_Sources (No_Project, In_Tree, Result);
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

         --  Make sure that new reserved words after Ada 95 may be used as
         --  identifiers.

         Opt.Ada_Version := Opt.Ada_95;

         Set_Name_Table_Byte (Name_Project,  Token_Type'Pos (Tok_Project));
         Set_Name_Table_Byte (Name_Extends,  Token_Type'Pos (Tok_Extends));
         Set_Name_Table_Byte (Name_External, Token_Type'Pos (Tok_External));
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
     (Shared : Shared_Project_Tree_Data_Access;
      Path   : Path_Name_Type)
   is
   begin
      Temp_Files_Table.Append (Shared.Private_Part.Temp_Files, Path);
   end Record_Temp_File;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Aggregated_Project_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Aggregated_Project, Aggregated_Project_List);
      Tmp : Aggregated_Project_List;
   begin
      while List /= null loop
         Tmp := List.Next;

         Free (List.Tree);

         Unchecked_Free (List);
         List := Tmp;
      end loop;
   end Free;

   ----------------------------
   -- Add_Aggregated_Project --
   ----------------------------

   procedure Add_Aggregated_Project
     (Project : Project_Id; Path : Path_Name_Type) is
   begin
      Project.Aggregated_Projects := new Aggregated_Project'
        (Path    => Path,
         Project => No_Project,
         Tree    => null,
         Next    => Project.Aggregated_Projects);
   end Add_Aggregated_Project;

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

         case Project.Qualifier is
            when Aggregate | Aggregate_Library =>
               Free (Project.Aggregated_Projects);

            when others =>
               null;
         end case;

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

   --------------------------
   -- Reset_Units_In_Table --
   --------------------------

   procedure Reset_Units_In_Table (Table : in out Units_Htable.Instance) is
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

         Unit := Units_Htable.Get_Next (Table);
      end loop;
   end Reset_Units_In_Table;

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

         --  We cannot reset Unit.File_Names (Impl or Spec).Unit here as
         --  Source_Data buffer is freed by the following instruction
         --  Free_List (Tree.Projects, Free_Project => True);

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
        Ada.Unchecked_Deallocation
          (Project_Tree_Data, Project_Tree_Ref);

      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation
          (Project_Tree_Appdata'Class, Project_Tree_Appdata_Access);

   begin
      if Tree /= null then
         if Tree.Is_Root_Tree then
            Name_List_Table.Free        (Tree.Shared.Name_Lists);
            Number_List_Table.Free      (Tree.Shared.Number_Lists);
            String_Element_Table.Free   (Tree.Shared.String_Elements);
            Variable_Element_Table.Free (Tree.Shared.Variable_Elements);
            Array_Element_Table.Free    (Tree.Shared.Array_Elements);
            Array_Table.Free            (Tree.Shared.Arrays);
            Package_Table.Free          (Tree.Shared.Packages);
            Temp_Files_Table.Free       (Tree.Shared.Private_Part.Temp_Files);
         end if;

         if Tree.Appdata /= null then
            Free (Tree.Appdata.all);
            Unchecked_Free (Tree.Appdata);
         end if;

         Source_Paths_Htable.Reset (Tree.Source_Paths_HT);
         Source_Files_Htable.Reset (Tree.Source_Files_HT);

         Reset_Units_In_Table (Tree.Units_HT);
         Free_List (Tree.Projects, Free_Project => True);
         Free_Units (Tree.Units_HT);

         Unchecked_Free (Tree);
      end if;
   end Free;

   -----------
   -- Reset --
   -----------

   procedure Reset (Tree : Project_Tree_Ref) is
   begin
      --  Visible tables

      if Tree.Is_Root_Tree then

         --  We cannot use 'Access here:
         --    "illegal attribute for discriminant-dependent component"
         --  However, we know this is valid since Shared and Shared_Data have
         --  the same lifetime and will always exist concurrently.

         Tree.Shared := Tree.Shared_Data'Unrestricted_Access;
         Name_List_Table.Init        (Tree.Shared.Name_Lists);
         Number_List_Table.Init      (Tree.Shared.Number_Lists);
         String_Element_Table.Init   (Tree.Shared.String_Elements);
         Variable_Element_Table.Init (Tree.Shared.Variable_Elements);
         Array_Element_Table.Init    (Tree.Shared.Array_Elements);
         Array_Table.Init            (Tree.Shared.Arrays);
         Package_Table.Init          (Tree.Shared.Packages);

         --  Private part table

         Temp_Files_Table.Init (Tree.Shared.Private_Part.Temp_Files);

         Tree.Shared.Private_Part.Current_Source_Path_File := No_Path;
         Tree.Shared.Private_Part.Current_Object_Path_File := No_Path;
      end if;

      Source_Paths_Htable.Reset    (Tree.Source_Paths_HT);
      Source_Files_Htable.Reset    (Tree.Source_Files_HT);
      Replaced_Source_HTable.Reset (Tree.Replaced_Sources);

      Tree.Replaced_Source_Number := 0;

      Reset_Units_In_Table (Tree.Units_HT);
      Free_List (Tree.Projects, Free_Project => True);
      Free_Units (Tree.Units_HT);
   end Reset;

   -------------------------------------
   -- Set_Current_Object_Path_File_Of --
   -------------------------------------

   procedure Set_Current_Object_Path_File_Of
     (Shared : Shared_Project_Tree_Data_Access;
      To     : Path_Name_Type)
   is
   begin
      Shared.Private_Part.Current_Object_Path_File := To;
   end Set_Current_Object_Path_File_Of;

   -------------------------------------
   -- Set_Current_Source_Path_File_Of --
   -------------------------------------

   procedure Set_Current_Source_Path_File_Of
     (Shared : Shared_Project_Tree_Data_Access;
      To     : Path_Name_Type)
   is
   begin
      Shared.Private_Part.Current_Source_Path_File := To;
   end Set_Current_Source_Path_File_Of;

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

   procedure Compute_All_Imported_Projects
     (Root_Project : Project_Id;
      Tree         : Project_Tree_Ref)
   is
      procedure Analyze_Tree
        (Local_Root : Project_Id;
         Local_Tree : Project_Tree_Ref);
      --  Process Project and all its aggregated project to analyze their own
      --  imported projects.

      ------------------
      -- Analyze_Tree --
      ------------------

      procedure Analyze_Tree
        (Local_Root : Project_Id;
         Local_Tree : Project_Tree_Ref)
      is
         pragma Unreferenced (Local_Root);

         Project : Project_Id;

         procedure Recursive_Add
           (Prj   : Project_Id;
            Tree  : Project_Tree_Ref;
            Dummy : in out Boolean);
         --  Recursively add the projects imported by project Project, but not
         --  those that are extended.

         -------------------
         -- Recursive_Add --
         -------------------

         procedure Recursive_Add
           (Prj   : Project_Id;
            Tree  : Project_Tree_Ref;
            Dummy : in out Boolean)
         is
            pragma Unreferenced (Dummy, Tree);

            List : Project_List;
            Prj2 : Project_Id;

         begin
            --  A project is not importing itself

            Prj2 := Ultimate_Extending_Project_Of (Prj);

            if Project /= Prj2 then

               --  Check that the project is not already in the list. We know
               --  the one passed to Recursive_Add have never been visited
               --  before, but the one passed it are the extended projects.

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
         List := Local_Tree.Projects;
         while List /= null loop
            Project := List.Project;
            Free_List
              (Project.All_Imported_Projects, Free_Project => False);
            For_All_Projects
              (Project, Local_Tree, Dummy, Include_Aggregated => False);
            List := List.Next;
         end loop;
      end Analyze_Tree;

      procedure For_Aggregates is
        new For_Project_And_Aggregated (Analyze_Tree);

   --  Start of processing for Compute_All_Imported_Projects

   begin
      For_Aggregates (Root_Project, Tree);
   end Compute_All_Imported_Projects;

   -------------------
   -- Is_Compilable --
   -------------------

   function Is_Compilable (Source : Source_Id) return Boolean is
   begin
      case Source.Compilable is
         when Unknown =>
            if Source.Language.Config.Compiler_Driver /= No_File
              and then
                Length_Of_Name (Source.Language.Config.Compiler_Driver) /= 0
              and then not Source.Locally_Removed
              and then (Source.Language.Config.Kind /= File_Based
                         or else Source.Kind /= Spec)
            then
               --  Do not modify Source.Compilable before the source record
               --  has been initialized.

               if Source.Source_TS /= Empty_Time_Stamp then
                  Source.Compilable := Yes;
               end if;

               return True;

            else
               if Source.Source_TS /= Empty_Time_Stamp then
                  Source.Compilable := No;
               end if;

               return False;
            end if;

         when Yes =>
            return True;

         when No =>
            return False;
      end case;
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
      Missing_Source_Files       : Error_Warning := Error;
      Ignore_Missing_With        : Boolean       := False)
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
         Missing_Source_Files       => Missing_Source_Files,
         Ignore_Missing_With        => Ignore_Missing_With);
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

   ------------------
   -- Debug_Output --
   ------------------

   procedure Debug_Output (Str : String) is
   begin
      if Current_Verbosity > Default then
         Set_Standard_Error;
         Write_Line ((1 .. Debug_Level * 2 => ' ') & Str);
         Set_Standard_Output;
      end if;
   end Debug_Output;

   ------------------
   -- Debug_Indent --
   ------------------

   procedure Debug_Indent is
   begin
      if Current_Verbosity = High then
         Set_Standard_Error;
         Write_Str ((1 .. Debug_Level * 2 => ' '));
         Set_Standard_Output;
      end if;
   end Debug_Indent;

   ------------------
   -- Debug_Output --
   ------------------

   procedure Debug_Output (Str : String; Str2 : Name_Id) is
   begin
      if Current_Verbosity = High then
         Debug_Indent;
         Set_Standard_Error;
         Write_Str (Str);

         if Str2 = No_Name then
            Write_Line (" <no_name>");
         else
            Write_Line (" """ & Get_Name_String (Str2) & '"');
         end if;

         Set_Standard_Output;
      end if;
   end Debug_Output;

   ---------------------------
   -- Debug_Increase_Indent --
   ---------------------------

   procedure Debug_Increase_Indent
     (Str : String := ""; Str2 : Name_Id := No_Name)
   is
   begin
      if Str2 /= No_Name then
         Debug_Output (Str, Str2);
      else
         Debug_Output (Str);
      end if;
      Debug_Level := Debug_Level + 1;
   end Debug_Increase_Indent;

   ---------------------------
   -- Debug_Decrease_Indent --
   ---------------------------

   procedure Debug_Decrease_Indent (Str : String := "") is
   begin
      if Debug_Level > 0 then
         Debug_Level := Debug_Level - 1;
      end if;

      if Str /= "" then
         Debug_Output (Str);
      end if;
   end Debug_Decrease_Indent;

   ----------------
   -- Debug_Name --
   ----------------

   function Debug_Name (Tree : Project_Tree_Ref) return Name_Id is
      P : Project_List;

   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("Tree [");

      P := Tree.Projects;
      while P /= null loop
         if P /= Tree.Projects then
            Add_Char_To_Name_Buffer (',');
         end if;

         Add_Str_To_Name_Buffer (Get_Name_String (P.Project.Name));

         P := P.Next;
      end loop;

      Add_Char_To_Name_Buffer (']');

      return Name_Find;
   end Debug_Name;

   ----------
   -- Free --
   ----------

   procedure Free (Tree : in out Project_Tree_Appdata) is
      pragma Unreferenced (Tree);
   begin
      null;
   end Free;

   --------------------------------
   -- For_Project_And_Aggregated --
   --------------------------------

   procedure For_Project_And_Aggregated
     (Root_Project : Project_Id;
      Root_Tree    : Project_Tree_Ref)
   is
      Agg : Aggregated_Project_List;

   begin
      Action (Root_Project, Root_Tree);

      if Root_Project.Qualifier in Aggregate_Project then
         Agg := Root_Project.Aggregated_Projects;
         while Agg /= null loop
            For_Project_And_Aggregated (Agg.Project, Agg.Tree);
            Agg := Agg.Next;
         end loop;
      end if;
   end For_Project_And_Aggregated;

--  Package initialization for Prj

begin
   --  Make sure that the standard config and user project file extensions are
   --  compatible with canonical case file naming.

   Canonical_Case_File_Name (Config_Project_File_Extension);
   Canonical_Case_File_Name (Project_File_Extension);
end Prj;
