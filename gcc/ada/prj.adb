------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P R J                                   --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with Debug;
with Osint;    use Osint;
with Prj.Attr;
with Prj.Err;  use Prj.Err;
with Snames;   use Snames;
with Table;
with Uintp;    use Uintp;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with System.Case_Util; use System.Case_Util;
with System.HTable;

package body Prj is

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  File suffix for object files

   Initial_Buffer_Size : constant := 100;
   --  Initial size for extensible buffer used in Add_To_Buffer

   Current_Mode : Mode := Ada_Only;

   The_Empty_String : Name_Id;

   Default_Ada_Spec_Suffix_Id : File_Name_Type;
   Default_Ada_Body_Suffix_Id : File_Name_Type;
   Slash_Id                   : Path_Name_Type;
   --  Initialized in Prj.Initialize, then never modified

   subtype Known_Casing is Casing_Type range All_Upper_Case .. Mixed_Case;

   The_Casing_Images : constant array (Known_Casing) of String_Access :=
     (All_Lower_Case => new String'("lowercase"),
      All_Upper_Case => new String'("UPPERCASE"),
      Mixed_Case     => new String'("MixedCase"));

   Initialized : Boolean := False;

   Standard_Dot_Replacement : constant File_Name_Type :=
                                File_Name_Type
                                  (First_Name_Id + Character'Pos ('-'));

   Std_Naming_Data : constant Naming_Data :=
                       (Dot_Replacement           => Standard_Dot_Replacement,
                        Casing                    => All_Lower_Case,
                        Spec_Suffix               => No_Array_Element,
                        Body_Suffix               => No_Array_Element,
                        Separate_Suffix           => No_File,
                        Specs                     => No_Array_Element,
                        Bodies                    => No_Array_Element,
                        Specification_Exceptions  => No_Array_Element,
                        Implementation_Exceptions => No_Array_Element);

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
                      Include_Path                   => null,
                      Include_Data_Set               => False,
                      Source_Dirs                    => Nil_String,
                      Known_Order_Of_Source_Dirs     => True,
                      Object_Directory               => No_Path_Information,
                      Library_TS                     => Empty_Time_Stamp,
                      Exec_Directory                 => No_Path_Information,
                      Extends                        => No_Project,
                      Extended_By                    => No_Project,
                      Naming                         => Std_Naming_Data,
                      Languages      => No_Language_Index,
                      Decl                           => No_Declarations,
                      Imported_Projects              => null,
                      All_Imported_Projects          => null,
                      Ada_Include_Path               => null,
                      Ada_Objects_Path               => null,
                      Objects_Path                   => null,
                      Include_Path_File              => No_Path,
                      Objects_Path_File_With_Libs    => No_Path,
                      Objects_Path_File_Without_Libs => No_Path,
                      Config_File_Name               => No_Path,
                      Config_File_Temp               => False,
                      Config_Checked                 => False,
                      Need_To_Build_Lib              => False,
                      Depth                          => 0,
                      Unkept_Comments                => False);

   package Temp_Files is new Table.Table
     (Table_Component_Type => Path_Name_Type,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Makegpr.Temp_Files");
   --  Table to store the path name of all the created temporary files, so that
   --  they can be deleted at the end, or when the program is interrupted.

   procedure Free (Project : in out Project_Id);
   --  Free memory allocated for Project

   procedure Free_List (Languages : in out Language_Ptr);
   procedure Free_List (Source : in out Source_Id);
   procedure Free_List (Languages : in out Language_List);
   --  Free memory allocated for the list of languages or sources

   procedure Language_Changed (Iter : in out Source_Iterator);
   procedure Project_Changed (Iter : in out Source_Iterator);
   --  Called when a new project or language was selected for this iterator.

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

   -----------------------
   -- Body_Suffix_Id_Of --
   -----------------------

   function Body_Suffix_Id_Of
     (In_Tree     : Project_Tree_Ref;
      Language_Id : Name_Id;
      Naming      : Naming_Data) return File_Name_Type
   is
      Element_Id : Array_Element_Id;
      Element    : Array_Element;

   begin
      --  ??? This seems to be only for Ada_Only mode...
      Element_Id := Naming.Body_Suffix;
      while Element_Id /= No_Array_Element loop
         Element := In_Tree.Array_Elements.Table (Element_Id);

         if Element.Index = Language_Id then
            return File_Name_Type (Element.Value.Value);
         end if;

         Element_Id := Element.Next;
      end loop;

      return No_File;
   end Body_Suffix_Id_Of;

   --------------------
   -- Body_Suffix_Of --
   --------------------

   function Body_Suffix_Of
     (In_Tree  : Project_Tree_Ref;
      Language : String;
      Naming   : Naming_Data) return String
   is
      Language_Id : Name_Id;
      Element_Id  : Array_Element_Id;
      Element     : Array_Element;

   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Language);
      To_Lower (Name_Buffer (1 .. Name_Len));
      Language_Id := Name_Find;

      Element_Id := Naming.Body_Suffix;
      while Element_Id /= No_Array_Element loop
         Element := In_Tree.Array_Elements.Table (Element_Id);

         if Element.Index = Language_Id then
            return Get_Name_String (Element.Value.Value);
         end if;

         Element_Id := Element.Next;
      end loop;

      return "";
   end Body_Suffix_Of;

   -----------------------------
   -- Default_Ada_Body_Suffix --
   -----------------------------

   function Default_Ada_Body_Suffix return File_Name_Type is
   begin
      return Default_Ada_Body_Suffix_Id;
   end Default_Ada_Body_Suffix;

   -----------------------------
   -- Default_Ada_Spec_Suffix --
   -----------------------------

   function Default_Ada_Spec_Suffix return File_Name_Type is
   begin
      return Default_Ada_Spec_Suffix_Id;
   end Default_Ada_Spec_Suffix;

   ---------------------------
   -- Delete_All_Temp_Files --
   ---------------------------

   procedure Delete_All_Temp_Files is
      Dont_Care : Boolean;
      pragma Warnings (Off, Dont_Care);
   begin
      if not Debug.Debug_Flag_N then
         for Index in 1 .. Temp_Files.Last loop
            Delete_File
              (Get_Name_String (Temp_Files.Table (Index)), Dont_Care);
         end loop;
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
            return
              File_Name_Type
                (Extend_Name
                   (Source_File_Name, Makefile_Dependency_Suffix));

         when ALI_File =>
            return
              File_Name_Type
                (Extend_Name
                   (Source_File_Name, ALI_Dependency_Suffix));
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

   function Empty_Project (Tree : Project_Tree_Ref) return Project_Data is
      Value : Project_Data;

   begin
      Prj.Initialize (Tree => No_Project_Tree);
      Value := Project_Empty;
      Value.Naming := Tree.Private_Part.Default_Naming;

      return Value;
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
         Error_Msg (Token_Image & " expected", Token_Ptr);
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

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode return Mode is
   begin
      return Current_Mode;
   end Get_Mode;

   ----------
   -- Hash --
   ----------

   function Hash is new System.HTable.Hash (Header_Num => Header_Num);
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

   function Image (Casing : Casing_Type) return String is
   begin
      return The_Casing_Images (Casing).all;
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Tree : Project_Tree_Ref) is
   begin
      if not Initialized then
         Initialized := True;
         Uintp.Initialize;
         Name_Len := 0;
         The_Empty_String := Name_Find;
         Empty_Name := The_Empty_String;
         Empty_File_Name := File_Name_Type (The_Empty_String);
         Name_Len := 4;
         Name_Buffer (1 .. 4) := ".ads";
         Default_Ada_Spec_Suffix_Id := Name_Find;
         Name_Len := 4;
         Name_Buffer (1 .. 4) := ".adb";
         Default_Ada_Body_Suffix_Id := Name_Find;
         Name_Len := 1;
         Name_Buffer (1) := '/';
         Slash_Id := Name_Find;

         Prj.Attr.Initialize;
         Set_Name_Table_Byte (Name_Project,  Token_Type'Pos (Tok_Project));
         Set_Name_Table_Byte (Name_Extends,  Token_Type'Pos (Tok_Extends));
         Set_Name_Table_Byte (Name_External, Token_Type'Pos (Tok_External));
      end if;

      if Tree /= No_Project_Tree then
         Reset (Tree);
      end if;
   end Initialize;

   -------------------
   -- Is_A_Language --
   -------------------

   function Is_A_Language
     (Project       : Project_Id;
      Language_Name : Name_Id) return Boolean
   is
      Lang_Ind : Language_Ptr;

   begin
      Lang_Ind := Project.Languages;
      while Lang_Ind /= No_Language_Index loop
         if Lang_Ind.Name = Language_Name then
            return True;
         end if;

         Lang_Ind := Lang_Ind.Next;
      end loop;

      return False;
   end Is_A_Language;

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

   ----------------------
   -- Record_Temp_File --
   ----------------------

   procedure Record_Temp_File (Path : Path_Name_Type) is
   begin
      Temp_Files.Increment_Last;
      Temp_Files.Table (Temp_Files.Last) := Path;
   end Record_Temp_File;

   ------------------------------------
   -- Register_Default_Naming_Scheme --
   ------------------------------------

   procedure Register_Default_Naming_Scheme
     (Language            : Name_Id;
      Default_Spec_Suffix : File_Name_Type;
      Default_Body_Suffix : File_Name_Type;
      In_Tree             : Project_Tree_Ref)
   is
      Lang    : Name_Id;
      Suffix  : Array_Element_Id;
      Found   : Boolean := False;
      Element : Array_Element;

   begin
      --  Get the language name in small letters

      Get_Name_String (Language);
      Name_Buffer (1 .. Name_Len) := To_Lower (Name_Buffer (1 .. Name_Len));
      Lang := Name_Find;

      --  Look for an element of the spec suffix array indexed by the language
      --  name. If one is found, put the default value.

      Suffix := In_Tree.Private_Part.Default_Naming.Spec_Suffix;
      Found := False;
      while Suffix /= No_Array_Element and then not Found loop
         Element := In_Tree.Array_Elements.Table (Suffix);

         if Element.Index = Lang then
            Found := True;
            Element.Value.Value := Name_Id (Default_Spec_Suffix);
            In_Tree.Array_Elements.Table (Suffix) := Element;

         else
            Suffix := Element.Next;
         end if;
      end loop;

      --  If none can be found, create a new one

      if not Found then
         Element :=
           (Index     => Lang,
            Src_Index => 0,
            Index_Case_Sensitive => False,
            Value => (Project  => No_Project,
                      Kind     => Single,
                      Location => No_Location,
                      Default  => False,
                      Value    => Name_Id (Default_Spec_Suffix),
                      Index    => 0),
            Next  => In_Tree.Private_Part.Default_Naming.Spec_Suffix);
         Array_Element_Table.Increment_Last (In_Tree.Array_Elements);
         In_Tree.Array_Elements.Table
           (Array_Element_Table.Last (In_Tree.Array_Elements)) :=
            Element;
         In_Tree.Private_Part.Default_Naming.Spec_Suffix :=
           Array_Element_Table.Last (In_Tree.Array_Elements);
      end if;

      --  Look for an element of the body suffix array indexed by the language
      --  name. If one is found, put the default value.

      Suffix := In_Tree.Private_Part.Default_Naming.Body_Suffix;
      Found := False;
      while Suffix /= No_Array_Element and then not Found loop
         Element := In_Tree.Array_Elements.Table (Suffix);

         if Element.Index = Lang then
            Found := True;
            Element.Value.Value := Name_Id (Default_Body_Suffix);
            In_Tree.Array_Elements.Table (Suffix) := Element;

         else
            Suffix := Element.Next;
         end if;
      end loop;

      --  If none can be found, create a new one

      if not Found then
         Element :=
           (Index     => Lang,
            Src_Index => 0,
            Index_Case_Sensitive => False,
            Value => (Project  => No_Project,
                      Kind     => Single,
                      Location => No_Location,
                      Default  => False,
                      Value    => Name_Id (Default_Body_Suffix),
                      Index    => 0),
            Next  => In_Tree.Private_Part.Default_Naming.Body_Suffix);
         Array_Element_Table.Increment_Last
           (In_Tree.Array_Elements);
         In_Tree.Array_Elements.Table
           (Array_Element_Table.Last (In_Tree.Array_Elements))
             := Element;
         In_Tree.Private_Part.Default_Naming.Body_Suffix :=
           Array_Element_Table.Last (In_Tree.Array_Elements);
      end if;
   end Register_Default_Naming_Scheme;

   ----------
   -- Free --
   ----------

   procedure Free (Project : in out Project_Id) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Data, Project_Id);

   begin
      if Project /= null then
         Free (Project.Include_Path);
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
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Source_Data, Source_Id);
      Tmp : Source_Id;
   begin
      while Source /= No_Source loop
         Tmp := Source.Next_In_Lang;
         Free_List (Source.Alternate_Languages);
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
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_List_Element, Project_List);
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
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Language_Data, Language_Ptr);
      Tmp : Language_Ptr;
   begin
      while Languages /= null loop
         Tmp := Languages.Next;
         Free_List (Languages.First_Source);
         Unchecked_Free (Languages);
         Languages := Tmp;
      end loop;
   end Free_List;

   ----------
   -- Free --
   ----------

   procedure Free (Tree : in out Project_Tree_Ref) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Tree_Data, Project_Tree_Ref);

   begin
      if Tree /= null then
         Name_List_Table.Free (Tree.Name_Lists);
         String_Element_Table.Free (Tree.String_Elements);
         Variable_Element_Table.Free (Tree.Variable_Elements);
         Array_Element_Table.Free (Tree.Array_Elements);
         Array_Table.Free (Tree.Arrays);
         Package_Table.Free (Tree.Packages);
         Unit_Table.Free (Tree.Units);
         Units_Htable.Reset (Tree.Units_HT);
         Source_Paths_Htable.Reset (Tree.Source_Paths_HT);
         Unit_Sources_Htable.Reset (Tree.Unit_Sources_HT);

         Free_List (Tree.Projects, Free_Project => True);

         --  Private part

         Naming_Table.Free      (Tree.Private_Part.Namings);
         Path_File_Table.Free   (Tree.Private_Part.Path_Files);
         Source_Path_Table.Free (Tree.Private_Part.Source_Paths);
         Object_Path_Table.Free (Tree.Private_Part.Object_Paths);

         Free (Tree.Private_Part.Ada_Path_Buffer);

         --  Naming data (nothing to free ???)

         null;

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
      String_Element_Table.Init     (Tree.String_Elements);
      Variable_Element_Table.Init   (Tree.Variable_Elements);
      Array_Element_Table.Init      (Tree.Array_Elements);
      Array_Table.Init              (Tree.Arrays);
      Package_Table.Init            (Tree.Packages);
      Unit_Table.Init               (Tree.Units);
      Units_Htable.Reset            (Tree.Units_HT);
      Source_Paths_Htable.Reset     (Tree.Source_Paths_HT);
      Unit_Sources_Htable.Reset     (Tree.Unit_Sources_HT);

      Free_List (Tree.Projects, Free_Project => True);

      --  Private part table

      Naming_Table.Init             (Tree.Private_Part.Namings);
      Naming_Table.Increment_Last   (Tree.Private_Part.Namings);
      Tree.Private_Part.Namings.Table
        (Naming_Table.Last (Tree.Private_Part.Namings)) := Std_Naming_Data;
      Path_File_Table.Init        (Tree.Private_Part.Path_Files);
      Source_Path_Table.Init      (Tree.Private_Part.Source_Paths);
      Object_Path_Table.Init      (Tree.Private_Part.Object_Paths);
      Tree.Private_Part.Default_Naming := Std_Naming_Data;

      if Current_Mode = Ada_Only then
         Register_Default_Naming_Scheme
           (Language            => Name_Ada,
            Default_Spec_Suffix => Default_Ada_Spec_Suffix,
            Default_Body_Suffix => Default_Ada_Body_Suffix,
            In_Tree             => Tree);
         Tree.Private_Part.Default_Naming.Separate_Suffix :=
           Default_Ada_Body_Suffix;

         Tree.Private_Part.Current_Source_Path_File := No_Path;
         Tree.Private_Part.Current_Object_Path_File := No_Path;
         Tree.Private_Part.Ada_Path_Length := 0;
         Tree.Private_Part.Ada_Prj_Include_File_Set := False;
         Tree.Private_Part.Ada_Prj_Objects_File_Set := False;
         Tree.Private_Part.Fill_Mapping_File := True;
      end if;
   end Reset;

   ------------------------
   -- Same_Naming_Scheme --
   ------------------------

   function Same_Naming_Scheme
     (Left, Right : Naming_Data) return Boolean
   is
   begin
      return Left.Dot_Replacement = Right.Dot_Replacement
        and then Left.Casing = Right.Casing
        and then Left.Separate_Suffix = Right.Separate_Suffix;
   end Same_Naming_Scheme;

   ---------------------
   -- Set_Body_Suffix --
   ---------------------

   procedure Set_Body_Suffix
     (In_Tree  : Project_Tree_Ref;
      Language : String;
      Naming   : in out Naming_Data;
      Suffix   : File_Name_Type)
   is
      Language_Id : Name_Id;
      Element     : Array_Element;

   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Language);
      To_Lower (Name_Buffer (1 .. Name_Len));
      Language_Id := Name_Find;

      Element :=
        (Index                => Language_Id,
         Src_Index            => 0,
         Index_Case_Sensitive => False,
         Value                =>
           (Kind     => Single,
            Project  => No_Project,
            Location => No_Location,
            Default  => False,
            Value    => Name_Id (Suffix),
            Index    => 0),
         Next                 => Naming.Body_Suffix);

      Array_Element_Table.Increment_Last (In_Tree.Array_Elements);
      Naming.Body_Suffix :=
         Array_Element_Table.Last (In_Tree.Array_Elements);
      In_Tree.Array_Elements.Table (Naming.Body_Suffix) := Element;
   end Set_Body_Suffix;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (New_Mode : Mode) is
   begin
      Current_Mode := New_Mode;
      case New_Mode is
         when Ada_Only =>
            Default_Language_Is_Ada := True;
            Must_Check_Configuration := False;
         when Multi_Language =>
            Default_Language_Is_Ada := False;
            Must_Check_Configuration := True;
      end case;
   end Set_Mode;

   ---------------------
   -- Set_Spec_Suffix --
   ---------------------

   procedure Set_Spec_Suffix
     (In_Tree  : Project_Tree_Ref;
      Language : String;
      Naming   : in out Naming_Data;
      Suffix   : File_Name_Type)
   is
      Language_Id : Name_Id;
      Element     : Array_Element;

   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Language);
      To_Lower (Name_Buffer (1 .. Name_Len));
      Language_Id := Name_Find;

      Element :=
        (Index                => Language_Id,
         Src_Index            => 0,
         Index_Case_Sensitive => False,
         Value                =>
           (Kind     => Single,
            Project  => No_Project,
            Location => No_Location,
            Default  => False,
            Value    => Name_Id (Suffix),
            Index    => 0),
         Next                 => Naming.Spec_Suffix);

      Array_Element_Table.Increment_Last (In_Tree.Array_Elements);
      Naming.Spec_Suffix :=
        Array_Element_Table.Last (In_Tree.Array_Elements);
      In_Tree.Array_Elements.Table (Naming.Spec_Suffix) := Element;
   end Set_Spec_Suffix;

   -----------
   -- Slash --
   -----------

   function Slash return Path_Name_Type is
   begin
      return Slash_Id;
   end Slash;

   -----------------------
   -- Spec_Suffix_Id_Of --
   -----------------------

   function Spec_Suffix_Id_Of
     (In_Tree     : Project_Tree_Ref;
      Language_Id : Name_Id;
      Naming      : Naming_Data) return File_Name_Type
   is
      Element_Id : Array_Element_Id;
      Element    : Array_Element;

   begin
      Element_Id := Naming.Spec_Suffix;
      while Element_Id /= No_Array_Element loop
         Element := In_Tree.Array_Elements.Table (Element_Id);

         if Element.Index = Language_Id then
            return File_Name_Type (Element.Value.Value);
         end if;

         Element_Id := Element.Next;
      end loop;

      return No_File;
   end Spec_Suffix_Id_Of;

   --------------------
   -- Spec_Suffix_Of --
   --------------------

   function Spec_Suffix_Of
     (In_Tree  : Project_Tree_Ref;
      Language : String;
      Naming   : Naming_Data) return String
   is
      Language_Id : Name_Id;
      Element_Id  : Array_Element_Id;
      Element     : Array_Element;

   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Language);
      To_Lower (Name_Buffer (1 .. Name_Len));
      Language_Id := Name_Find;

      Element_Id := Naming.Spec_Suffix;
      while Element_Id /= No_Array_Element loop
         Element := In_Tree.Array_Elements.Table (Element_Id);

         if Element.Index = Language_Id then
            return Get_Name_String (Element.Value.Value);
         end if;

         Element_Id := Element.Next;
      end loop;

      return "";
   end Spec_Suffix_Of;

   --------------------------
   -- Standard_Naming_Data --
   --------------------------

   function Standard_Naming_Data
     (Tree : Project_Tree_Ref := No_Project_Tree) return Naming_Data
   is
   begin
      if Tree = No_Project_Tree then
         Prj.Initialize (Tree => No_Project_Tree);
         return Std_Naming_Data;
      else
         return Tree.Private_Part.Default_Naming;
      end if;
   end Standard_Naming_Data;

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

   -------------------------
   -- Has_Foreign_Sources --
   -------------------------

   function Has_Foreign_Sources (Data : Project_Id) return Boolean is
      Lang : Language_Ptr;

   begin
      Lang := Data.Languages;
      while Lang /= No_Language_Index loop
         if Lang.Name /= Name_Ada
           and then
             (Current_Mode = Ada_Only or else Lang.First_Source /= No_Source)
         then
            return True;
         end if;

         Lang := Lang.Next;
      end loop;

      return False;
   end Has_Foreign_Sources;

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
      if (Project.Library and Including_Libraries)
        or else
          (Project.Object_Directory /= No_Path_Information
            and then (not Including_Libraries or else not Project.Library))
      then
         --  For a library project, add the library ALI directory if there is
         --  no object directory or if the library ALI directory contains ALI
         --  files; otherwise add the object directory.

         if Project.Library then
            if Project.Object_Directory = No_Path_Information
              or else Contains_ALI_Files (Project.Library_ALI_Dir.Name)
            then
               return Project.Library_ALI_Dir.Name;
            else
               return Project.Object_Directory.Name;
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
                  return Project.Object_Directory.Name;
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

   procedure Compute_All_Imported_Projects (Project : Project_Id) is
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

         if Project /= Prj then
            Prj2 := Ultimate_Extending_Project_Of (Prj);

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

   begin
      Free_List (Project.All_Imported_Projects, Free_Project => False);
      For_All_Projects (Project, Dummy);
   end Compute_All_Imported_Projects;

begin
   --  Make sure that the standard config and user project file extensions are
   --  compatible with canonical case file naming.

   Canonical_Case_File_Name (Config_Project_File_Extension);
   Canonical_Case_File_Name (Project_File_Extension);
end Prj;
