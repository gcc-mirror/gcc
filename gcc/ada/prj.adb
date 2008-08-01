------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P R J                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2008, Free Software Foundation, Inc.         --
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

with Debug;
with Output;   use Output;
with Osint;    use Osint;
with Prj.Attr;
with Prj.Env;
with Prj.Err;  use Prj.Err;
with Snames;   use Snames;
with Table;
with Uintp;    use Uintp;

with System.Case_Util; use System.Case_Util;
with System.HTable;

package body Prj is

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  File suffix for object files

   Initial_Buffer_Size : constant := 100;
   --  Initial size for extensible buffer used in Add_To_Buffer

   Current_Mode : Mode := Ada_Only;

   Configuration_Mode : Boolean := False;

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
                        Dot_Repl_Loc              => No_Location,
                        Casing                    => All_Lower_Case,
                        Spec_Suffix               => No_Array_Element,
                        Ada_Spec_Suffix_Loc       => No_Location,
                        Body_Suffix               => No_Array_Element,
                        Ada_Body_Suffix_Loc       => No_Location,
                        Separate_Suffix           => No_File,
                        Sep_Suffix_Loc            => No_Location,
                        Specs                     => No_Array_Element,
                        Bodies                    => No_Array_Element,
                        Specification_Exceptions  => No_Array_Element,
                        Implementation_Exceptions => No_Array_Element);

   Project_Empty : constant Project_Data :=
                     (Qualifier                      => Unspecified,
                      Externally_Built               => False,
                      Config                         => Default_Project_Config,
                      Languages                      => No_Name_List,
                      First_Referred_By              => No_Project,
                      Name                           => No_Name,
                      Display_Name                   => No_Name,
                      Path                           => No_Path_Information,
                      Virtual                        => False,
                      Location                       => No_Location,
                      Mains                          => Nil_String,
                      Directory                      => No_Path_Information,
                      Dir_Path                       => null,
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
                      Ada_Sources_Present            => True,
                      Other_Sources_Present          => True,
                      Ada_Sources                    => Nil_String,
                      First_Source                   => No_Source,
                      Last_Source                    => No_Source,
                      Interfaces_Defined             => False,
                      Unit_Based_Language_Name       => No_Name,
                      Unit_Based_Language_Index      => No_Language_Index,
                      Imported_Directories_Switches  => null,
                      Include_Path                   => null,
                      Include_Data_Set               => False,
                      Include_Language               => No_Language_Index,
                      Source_Dirs                    => Nil_String,
                      Known_Order_Of_Source_Dirs     => True,
                      Object_Directory               => No_Path_Information,
                      Library_TS                     => Empty_Time_Stamp,
                      Exec_Directory                 => No_Path_Information,
                      Extends                        => No_Project,
                      Extended_By                    => No_Project,
                      Naming                         => Std_Naming_Data,
                      First_Language_Processing      => No_Language_Index,
                      Decl                           => No_Declarations,
                      Imported_Projects              => Empty_Project_List,
                      All_Imported_Projects          => Empty_Project_List,
                      Ada_Include_Path               => null,
                      Ada_Objects_Path               => null,
                      Objects_Path                   => null,
                      Include_Path_File              => No_Path,
                      Objects_Path_File_With_Libs    => No_Path,
                      Objects_Path_File_Without_Libs => No_Path,
                      Config_File_Name               => No_Path,
                      Config_File_Temp               => False,
                      Config_Checked                 => False,
                      Checked                        => False,
                      Seen                           => False,
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
     (In_Tree  : Project_Tree_Ref;
      Language : String;
      Naming   : Naming_Data) return File_Name_Type
   is
      Language_Id : Name_Id;

   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Language);
      To_Lower (Name_Buffer (1 .. Name_Len));
      Language_Id := Name_Find;

      return
        Body_Suffix_Id_Of
          (In_Tree     => In_Tree,
           Language_Id => Language_Id,
           Naming      => Naming);
   end Body_Suffix_Id_Of;

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
      Suffix     : File_Name_Type := No_File;
      Lang       : Language_Index;

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

      if Current_Mode = Multi_Language then
         Lang := In_Tree.First_Language;
         while Lang /= No_Language_Index loop
            if In_Tree.Languages_Data.Table (Lang).Name = Language_Id then
               Suffix :=
                 In_Tree.Languages_Data.Table
                   (Lang).Config.Naming_Data.Body_Suffix;
               exit;
            end if;

            Lang := In_Tree.Languages_Data.Table (Lang).Next;
         end loop;
      end if;

      return Suffix;
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
      Suffix      : File_Name_Type := No_File;
      Lang        : Language_Index;

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

      if Current_Mode = Multi_Language then
         Lang := In_Tree.First_Language;
         while Lang /= No_Language_Index loop
            if In_Tree.Languages_Data.Table (Lang).Name = Language_Id then
               Suffix :=
                 File_Name_Type
                   (In_Tree.Languages_Data.Table
                        (Lang).Config.Naming_Data.Body_Suffix);
               exit;
            end if;

            Lang := In_Tree.Languages_Data.Table (Lang).Next;
         end loop;

         if Suffix /= No_File then
            return Get_Name_String (Suffix);
         end if;
      end if;

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

   ---------------------------
   -- Display_Language_Name --
   ---------------------------

   procedure Display_Language_Name
     (In_Tree  : Project_Tree_Ref;
      Language : Language_Index)
   is
   begin
      Get_Name_String (In_Tree.Languages_Data.Table (Language).Display_Name);
      Write_Str (Name_Buffer (1 .. Name_Len));
   end Display_Language_Name;

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

   --------------------------------
   -- For_Every_Project_Imported --
   --------------------------------

   procedure For_Every_Project_Imported
     (By         : Project_Id;
      In_Tree    : Project_Tree_Ref;
      With_State : in out State)
   is

      procedure Recursive_Check (Project : Project_Id);
      --  Check if a project has already been seen. If not seen, mark it as
      --  Seen, Call Action, and check all its imported projects.

      ---------------------
      -- Recursive_Check --
      ---------------------

      procedure Recursive_Check (Project : Project_Id) is
         List : Project_List;
      begin
         if not In_Tree.Projects.Table (Project).Seen then
            In_Tree.Projects.Table (Project).Seen := True;
            Action (Project, With_State);

            List := In_Tree.Projects.Table (Project).Imported_Projects;
            while List /= Empty_Project_List loop
               Recursive_Check (In_Tree.Project_Lists.Table (List).Project);
               List := In_Tree.Project_Lists.Table (List).Next;
            end loop;
         end if;
      end Recursive_Check;

   --  Start of processing for For_Every_Project_Imported

   begin
      for Project in Project_Table.First ..
                     Project_Table.Last (In_Tree.Projects)
      loop
         In_Tree.Projects.Table (Project).Seen := False;
      end loop;

      Recursive_Check (Project => By);
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
      return Header_Num (Project mod Max_Header_Num);
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (Casing : Casing_Type) return String is
   begin
      return The_Casing_Images (Casing).all;
   end Image;

   ----------------------
   -- In_Configuration --
   ----------------------

   function In_Configuration return Boolean is
   begin
      return Configuration_Mode;
   end In_Configuration;

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

         Prj.Env.Initialize;
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
     (Tree          : Project_Tree_Ref;
      Data          : Project_Data;
      Language_Name : Name_Id) return Boolean
   is
   begin
      if Get_Mode = Ada_Only then
         declare
            List : Name_List_Index := Data.Languages;
         begin
            while List /= No_Name_List loop
               if Tree.Name_Lists.Table (List).Name = Language_Name then
                  return True;
               else
                  List := Tree.Name_Lists.Table (List).Next;
               end if;
            end loop;
         end;

      else
         declare
            Lang_Ind  : Language_Index := Data.First_Language_Processing;
            Lang_Data : Language_Data;

         begin
            while Lang_Ind /= No_Language_Index loop
               Lang_Data := Tree.Languages_Data.Table (Lang_Ind);

               if Lang_Data.Name = Language_Name then
                  return True;
               end if;

               Lang_Ind := Lang_Data.Next;
            end loop;
         end;
      end if;

      return False;
   end Is_A_Language;

   ------------------
   -- Is_Extending --
   ------------------

   function Is_Extending
     (Extending : Project_Id;
      Extended  : Project_Id;
      In_Tree   : Project_Tree_Ref) return Boolean
   is
      Proj : Project_Id;

   begin
      Proj := Extending;
      while Proj /= No_Project loop
         if Proj = Extended then
            return True;
         end if;

         Proj := In_Tree.Projects.Table (Proj).Extends;
      end loop;

      return False;
   end Is_Extending;

   -----------------------
   -- Objects_Exist_For --
   -----------------------

   function Objects_Exist_For
     (Language : String;
      In_Tree  : Project_Tree_Ref) return Boolean
   is
      Language_Id : Name_Id;
      Lang        : Language_Index;

   begin
      if Current_Mode = Multi_Language then
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Language);
         To_Lower (Name_Buffer (1 .. Name_Len));
         Language_Id := Name_Find;

         Lang := In_Tree.First_Language;
         while Lang /= No_Language_Index loop
            if In_Tree.Languages_Data.Table (Lang).Name = Language_Id then
               return
                 In_Tree.Languages_Data.Table
                   (Lang).Config.Object_Generated;
            end if;

            Lang := In_Tree.Languages_Data.Table (Lang).Next;
         end loop;
      end if;

      return True;
   end Objects_Exist_For;

   -----------------
   -- Object_Name --
   -----------------

   function Object_Name
     (Source_File_Name : File_Name_Type)
      return File_Name_Type
   is
   begin
      return Extend_Name (Source_File_Name, Object_Suffix);
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
      Lang : Name_Id;
      Suffix : Array_Element_Id;
      Found : Boolean := False;
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

   -----------
   -- Reset --
   -----------

   procedure Reset (Tree : Project_Tree_Ref) is

      --  Def_Lang : constant Name_Node :=
      --             (Name => Name_Ada,
      --              Next => No_Name_List);
      --  Why is the above commented out ???

   begin
      Prj.Env.Initialize;

      --  Visible tables

      Language_Data_Table.Init      (Tree.Languages_Data);
      Name_List_Table.Init          (Tree.Name_Lists);
      String_Element_Table.Init     (Tree.String_Elements);
      Variable_Element_Table.Init   (Tree.Variable_Elements);
      Array_Element_Table.Init      (Tree.Array_Elements);
      Array_Table.Init              (Tree.Arrays);
      Package_Table.Init            (Tree.Packages);
      Project_List_Table.Init       (Tree.Project_Lists);
      Project_Table.Init            (Tree.Projects);
      Source_Data_Table.Init        (Tree.Sources);
      Alternate_Language_Table.Init (Tree.Alt_Langs);
      Unit_Table.Init               (Tree.Units);
      Units_Htable.Reset            (Tree.Units_HT);
      Files_Htable.Reset            (Tree.Files_HT);
      Source_Paths_Htable.Reset     (Tree.Source_Paths_HT);

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

   --------------------------
   -- Set_In_Configuration --
   --------------------------

   procedure Set_In_Configuration (Value : Boolean) is
   begin
      Configuration_Mode := Value;
   end Set_In_Configuration;

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
     (In_Tree  : Project_Tree_Ref;
      Language : String;
      Naming   : Naming_Data) return File_Name_Type
   is
      Language_Id : Name_Id;

   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Language);
      To_Lower (Name_Buffer (1 .. Name_Len));
      Language_Id := Name_Find;

      return
        Spec_Suffix_Id_Of
          (In_Tree     => In_Tree,
           Language_Id => Language_Id,
           Naming      => Naming);
   end Spec_Suffix_Id_Of;

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
      Suffix     : File_Name_Type := No_File;
      Lang       : Language_Index;

   begin
      Element_Id := Naming.Spec_Suffix;
      while Element_Id /= No_Array_Element loop
         Element := In_Tree.Array_Elements.Table (Element_Id);

         if Element.Index = Language_Id then
            return File_Name_Type (Element.Value.Value);
         end if;

         Element_Id := Element.Next;
      end loop;

      if Current_Mode = Multi_Language then
         Lang := In_Tree.First_Language;
         while Lang /= No_Language_Index loop
            if In_Tree.Languages_Data.Table (Lang).Name = Language_Id then
               Suffix :=
                 In_Tree.Languages_Data.Table
                   (Lang).Config.Naming_Data.Spec_Suffix;
               exit;
            end if;

            Lang := In_Tree.Languages_Data.Table (Lang).Next;
         end loop;
      end if;

      return Suffix;
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
      Suffix      : File_Name_Type := No_File;
      Lang        : Language_Index;

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

      if Current_Mode = Multi_Language then
         Lang := In_Tree.First_Language;
         while Lang /= No_Language_Index loop
            if In_Tree.Languages_Data.Table (Lang).Name = Language_Id then
               Suffix :=
                 File_Name_Type
                   (In_Tree.Languages_Data.Table
                      (Lang).Config.Naming_Data.Spec_Suffix);
               exit;
            end if;

            Lang := In_Tree.Languages_Data.Table (Lang).Next;
         end loop;

         if Suffix /= No_File then
            return Get_Name_String (Suffix);
         end if;
      end if;

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

begin
   --  Make sure that the standard config and user project file extensions are
   --  compatible with canonical case file naming.

   Canonical_Case_File_Name (Config_Project_File_Extension);
   Canonical_Case_File_Name (Project_File_Extension);
end Prj;
