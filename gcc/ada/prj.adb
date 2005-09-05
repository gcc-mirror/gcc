------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P R J                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2001-2005 Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Namet;    use Namet;
with Output;   use Output;
with Osint;    use Osint;
with Prj.Attr;
with Prj.Env;
with Prj.Err;  use Prj.Err;
with Snames;   use Snames;
with Uintp;    use Uintp;

with GNAT.Case_Util; use GNAT.Case_Util;

package body Prj is

   Initial_Buffer_Size : constant := 100;
   --  Initial size for extensible buffer used in Add_To_Buffer

   The_Empty_String : Name_Id;

   Name_C_Plus_Plus : Name_Id;

   Default_Ada_Spec_Suffix_Id : Name_Id;
   Default_Ada_Body_Suffix_Id : Name_Id;
   Slash_Id                   : Name_Id;
   --  Initialized in Prj.Initialized, then never modified

   subtype Known_Casing is Casing_Type range All_Upper_Case .. Mixed_Case;

   The_Casing_Images : constant array (Known_Casing) of String_Access :=
     (All_Lower_Case => new String'("lowercase"),
      All_Upper_Case => new String'("UPPERCASE"),
      Mixed_Case     => new String'("MixedCase"));

   Initialized : Boolean := False;

   Standard_Dot_Replacement      : constant Name_Id :=
     First_Name_Id + Character'Pos ('-');

   Std_Naming_Data : Naming_Data :=
     (Dot_Replacement           => Standard_Dot_Replacement,
      Dot_Repl_Loc              => No_Location,
      Casing                    => All_Lower_Case,
      Spec_Suffix               => No_Array_Element,
      Ada_Spec_Suffix           => No_Name,
      Spec_Suffix_Loc           => No_Location,
      Impl_Suffixes             => No_Impl_Suffixes,
      Supp_Suffixes             => No_Supp_Language_Index,
      Body_Suffix               => No_Array_Element,
      Ada_Body_Suffix           => No_Name,
      Body_Suffix_Loc           => No_Location,
      Separate_Suffix           => No_Name,
      Sep_Suffix_Loc            => No_Location,
      Specs                     => No_Array_Element,
      Bodies                    => No_Array_Element,
      Specification_Exceptions  => No_Array_Element,
      Implementation_Exceptions => No_Array_Element);

   Project_Empty : Project_Data :=
     (Externally_Built               => False,
      Languages                      => No_Languages,
      Supp_Languages                 => No_Supp_Language_Index,
      First_Referred_By              => No_Project,
      Name                           => No_Name,
      Display_Name                   => No_Name,
      Path_Name                      => No_Name,
      Display_Path_Name              => No_Name,
      Virtual                        => False,
      Location                       => No_Location,
      Mains                          => Nil_String,
      Directory                      => No_Name,
      Display_Directory              => No_Name,
      Dir_Path                       => null,
      Library                        => False,
      Library_Dir                    => No_Name,
      Display_Library_Dir            => No_Name,
      Library_Src_Dir                => No_Name,
      Display_Library_Src_Dir        => No_Name,
      Library_Name                   => No_Name,
      Library_Kind                   => Static,
      Lib_Internal_Name              => No_Name,
      Standalone_Library             => False,
      Lib_Interface_ALIs             => Nil_String,
      Lib_Auto_Init                  => False,
      Symbol_Data                    => No_Symbols,
      Ada_Sources_Present            => True,
      Other_Sources_Present          => True,
      Sources                        => Nil_String,
      First_Other_Source             => No_Other_Source,
      Last_Other_Source              => No_Other_Source,
      Imported_Directories_Switches  => null,
      Include_Path                   => null,
      Include_Data_Set               => False,
      Source_Dirs                    => Nil_String,
      Known_Order_Of_Source_Dirs     => True,
      Object_Directory               => No_Name,
      Display_Object_Dir             => No_Name,
      Exec_Directory                 => No_Name,
      Display_Exec_Dir               => No_Name,
      Extends                        => No_Project,
      Extended_By                    => No_Project,
      Naming                         => Std_Naming_Data,
      First_Language_Processing      => Default_First_Language_Processing_Data,
      Supp_Language_Processing       => No_Supp_Language_Index,
      Default_Linker                 => No_Name,
      Default_Linker_Path            => No_Name,
      Decl                           => No_Declarations,
      Imported_Projects              => Empty_Project_List,
      Ada_Include_Path               => null,
      Ada_Objects_Path               => null,
      Include_Path_File              => No_Name,
      Objects_Path_File_With_Libs    => No_Name,
      Objects_Path_File_Without_Libs => No_Name,
      Config_File_Name               => No_Name,
      Config_File_Temp               => False,
      Config_Checked                 => False,
      Language_Independent_Checked   => False,
      Checked                        => False,
      Seen                           => False,
      Need_To_Build_Lib              => False,
      Depth                          => 0,
      Unkept_Comments                => False);

   -----------------------
   -- Add_Language_Name --
   -----------------------

   procedure Add_Language_Name (Name : Name_Id) is
   begin
      Last_Language_Index := Last_Language_Index + 1;
      Language_Indexes.Set (Name, Last_Language_Index);
      Language_Names.Increment_Last;
      Language_Names.Table (Last_Language_Index) := Name;
   end Add_Language_Name;

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

   -----------------------------
   -- Default_Ada_Body_Suffix --
   -----------------------------

   function Default_Ada_Body_Suffix return Name_Id is
   begin
      return Default_Ada_Body_Suffix_Id;
   end Default_Ada_Body_Suffix;

   -----------------------------
   -- Default_Ada_Spec_Suffix --
   -----------------------------

   function Default_Ada_Spec_Suffix return Name_Id is
   begin
      return Default_Ada_Spec_Suffix_Id;
   end Default_Ada_Spec_Suffix;

   ---------------------------
   -- Display_Language_Name --
   ---------------------------

   procedure Display_Language_Name (Language : Language_Index) is
   begin
      Get_Name_String (Language_Names.Table (Language));
      To_Upper (Name_Buffer (1 .. 1));
      Write_Str (Name_Buffer (1 .. Name_Len));
   end Display_Language_Name;

   -------------------
   -- Empty_Project --
   -------------------

   function Empty_Project (Tree : Project_Tree_Ref)  return Project_Data is
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

            List :=
              In_Tree.Projects.Table (Project).Imported_Projects;
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

   ----------
   -- Hash --
   ----------

   function Hash (Name : Name_Id) return Header_Num is
   begin
      return Hash (Get_Name_String (Name));
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
         Name_Len := 4;
         Name_Buffer (1 .. 4) := ".ads";
         Default_Ada_Spec_Suffix_Id := Name_Find;
         Name_Len := 4;
         Name_Buffer (1 .. 4) := ".adb";
         Default_Ada_Body_Suffix_Id := Name_Find;
         Name_Len := 1;
         Name_Buffer (1) := '/';
         Slash_Id := Name_Find;
         Name_Len := 3;
         Name_Buffer (1 .. 3) := "c++";
         Name_C_Plus_Plus := Name_Find;

         Std_Naming_Data.Ada_Spec_Suffix := Default_Ada_Spec_Suffix;
         Std_Naming_Data.Ada_Body_Suffix := Default_Ada_Body_Suffix;
         Std_Naming_Data.Separate_Suffix := Default_Ada_Body_Suffix;
         Project_Empty.Naming := Std_Naming_Data;
         Prj.Env.Initialize;
         Prj.Attr.Initialize;
         Set_Name_Table_Byte (Name_Project,  Token_Type'Pos (Tok_Project));
         Set_Name_Table_Byte (Name_Extends,  Token_Type'Pos (Tok_Extends));
         Set_Name_Table_Byte (Name_External, Token_Type'Pos (Tok_External));

         Language_Indexes.Reset;
         Last_Language_Index := No_Language_Index;
         Language_Names.Init;
         Add_Language_Name (Name_Ada);
         Add_Language_Name (Name_C);
         Add_Language_Name (Name_C_Plus_Plus);
      end if;

      if Tree /= No_Project_Tree then
         Reset (Tree);
      end if;
   end Initialize;

   ----------------
   -- Is_Present --
   ----------------

   function Is_Present
     (Language   : Language_Index;
      In_Project : Project_Data;
      In_Tree    : Project_Tree_Ref) return Boolean
   is
   begin
      case Language is
         when No_Language_Index =>
            return False;

         when First_Language_Indexes =>
            return In_Project.Languages (Language);

         when others =>
            declare
               Supp : Supp_Language;
               Supp_Index : Supp_Language_Index := In_Project.Supp_Languages;

            begin
               while Supp_Index /= No_Supp_Language_Index loop
                  Supp := In_Tree.Present_Languages.Table (Supp_Index);

                  if Supp.Index = Language then
                     return Supp.Present;
                  end if;

                  Supp_Index := Supp.Next;
               end loop;

               return False;
            end;
      end case;
   end Is_Present;

   ---------------------------------
   -- Language_Processing_Data_Of --
   ---------------------------------

   function Language_Processing_Data_Of
     (Language   : Language_Index;
      In_Project : Project_Data;
      In_Tree    : Project_Tree_Ref) return Language_Processing_Data
   is
   begin
      case Language is
         when No_Language_Index =>
            return Default_Language_Processing_Data;

         when First_Language_Indexes =>
            return In_Project.First_Language_Processing (Language);

         when others =>
            declare
               Supp : Supp_Language_Data;
               Supp_Index : Supp_Language_Index :=
                 In_Project.Supp_Language_Processing;

            begin
               while Supp_Index /= No_Supp_Language_Index loop
                  Supp := In_Tree.Supp_Languages.Table (Supp_Index);

                  if Supp.Index = Language then
                     return Supp.Data;
                  end if;

                  Supp_Index := Supp.Next;
               end loop;

               return Default_Language_Processing_Data;
            end;
      end case;
   end Language_Processing_Data_Of;

   ------------------------------------
   -- Register_Default_Naming_Scheme --
   ------------------------------------

   procedure Register_Default_Naming_Scheme
     (Language            : Name_Id;
      Default_Spec_Suffix : Name_Id;
      Default_Body_Suffix : Name_Id;
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

      Suffix := In_Tree.Private_Part.Default_Naming.Spec_Suffix;
      Found := False;

      --  Look for an element of the spec sufix array indexed by the language
      --  name. If one is found, put the default value.

      while Suffix /= No_Array_Element and then not Found loop
         Element := In_Tree.Array_Elements.Table (Suffix);

         if Element.Index = Lang then
            Found := True;
            Element.Value.Value := Default_Spec_Suffix;
            In_Tree.Array_Elements.Table (Suffix) := Element;

         else
            Suffix := Element.Next;
         end if;
      end loop;

      --  If none can be found, create a new one.

      if not Found then
         Element :=
           (Index     => Lang,
            Src_Index => 0,
            Index_Case_Sensitive => False,
            Value => (Project  => No_Project,
                      Kind     => Single,
                      Location => No_Location,
                      Default  => False,
                      Value    => Default_Spec_Suffix,
                      Index    => 0),
            Next  => In_Tree.Private_Part.Default_Naming.Spec_Suffix);
         Array_Element_Table.Increment_Last (In_Tree.Array_Elements);
         In_Tree.Array_Elements.Table
           (Array_Element_Table.Last (In_Tree.Array_Elements)) :=
            Element;
         In_Tree.Private_Part.Default_Naming.Spec_Suffix :=
           Array_Element_Table.Last (In_Tree.Array_Elements);
      end if;

      Suffix := In_Tree.Private_Part.Default_Naming.Body_Suffix;
      Found := False;

      --  Look for an element of the body sufix array indexed by the language
      --  name. If one is found, put the default value.

      while Suffix /= No_Array_Element and then not Found loop
         Element := In_Tree.Array_Elements.Table (Suffix);

         if Element.Index = Lang then
            Found := True;
            Element.Value.Value := Default_Body_Suffix;
            In_Tree.Array_Elements.Table (Suffix) := Element;

         else
            Suffix := Element.Next;
         end if;
      end loop;

      --  If none can be found, create a new one.

      if not Found then
         Element :=
           (Index     => Lang,
            Src_Index => 0,
            Index_Case_Sensitive => False,
            Value => (Project  => No_Project,
                      Kind     => Single,
                      Location => No_Location,
                      Default  => False,
                      Value    => Default_Body_Suffix,
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
   begin
      Prj.Env.Initialize;
      Present_Language_Table.Init (Tree.Present_Languages);
      Supp_Suffix_Table.Init      (Tree.Supp_Suffixes);
      Name_List_Table.Init        (Tree.Name_Lists);
      Supp_Language_Table.Init    (Tree.Supp_Languages);
      Other_Source_Table.Init     (Tree.Other_Sources);
      String_Element_Table.Init   (Tree.String_Elements);
      Variable_Element_Table.Init (Tree.Variable_Elements);
      Array_Element_Table.Init    (Tree.Array_Elements);
      Array_Table.Init            (Tree.Arrays);
      Package_Table.Init          (Tree.Packages);
      Project_List_Table.Init     (Tree.Project_Lists);
      Project_Table.Init          (Tree.Projects);
      Unit_Table.Init             (Tree.Units);
      Units_Htable.Reset          (Tree.Units_HT);
      Files_Htable.Reset          (Tree.Files_HT);
      Naming_Table.Init           (Tree.Private_Part.Namings);
      Path_File_Table.Init        (Tree.Private_Part.Path_Files);
      Source_Path_Table.Init      (Tree.Private_Part.Source_Paths);
      Object_Path_Table.Init      (Tree.Private_Part.Object_Paths);
      Tree.Private_Part.Default_Naming := Std_Naming_Data;
      Register_Default_Naming_Scheme
        (Language            => Name_Ada,
         Default_Spec_Suffix => Default_Ada_Spec_Suffix,
         Default_Body_Suffix => Default_Ada_Body_Suffix,
         In_Tree             => Tree);
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
        and then Left.Ada_Spec_Suffix = Right.Ada_Spec_Suffix
        and then Left.Ada_Body_Suffix = Right.Ada_Body_Suffix
        and then Left.Separate_Suffix = Right.Separate_Suffix;
   end Same_Naming_Scheme;

   ---------
   -- Set --
   ---------

   procedure Set
     (Language   : Language_Index;
      Present    : Boolean;
      In_Project : in out Project_Data;
      In_Tree    : Project_Tree_Ref)
   is
   begin
      case Language is
         when No_Language_Index =>
            null;

         when First_Language_Indexes =>
            In_Project.Languages (Language) := Present;

         when others =>
            declare
               Supp : Supp_Language;
               Supp_Index : Supp_Language_Index := In_Project.Supp_Languages;

            begin
               while Supp_Index /= No_Supp_Language_Index loop
                  Supp := In_Tree.Present_Languages.Table
                                                                (Supp_Index);

                  if Supp.Index = Language then
                     In_Tree.Present_Languages.Table
                                            (Supp_Index).Present := Present;
                     return;
                  end if;

                  Supp_Index := Supp.Next;
               end loop;

               Supp := (Index => Language, Present => Present,
                        Next  => In_Project.Supp_Languages);
               Present_Language_Table.Increment_Last
                 (In_Tree.Present_Languages);
               Supp_Index := Present_Language_Table.Last
                 (In_Tree.Present_Languages);
               In_Tree.Present_Languages.Table (Supp_Index) :=
                 Supp;
               In_Project.Supp_Languages := Supp_Index;
            end;
      end case;
   end Set;

   procedure Set
     (Language_Processing : Language_Processing_Data;
      For_Language        : Language_Index;
      In_Project          : in out Project_Data;
      In_Tree             : Project_Tree_Ref)
   is
   begin
      case For_Language is
         when No_Language_Index =>
            null;

         when First_Language_Indexes =>
            In_Project.First_Language_Processing (For_Language) :=
              Language_Processing;

         when others =>
            declare
               Supp : Supp_Language_Data;
               Supp_Index : Supp_Language_Index :=
                 In_Project.Supp_Language_Processing;

            begin
               while Supp_Index /= No_Supp_Language_Index loop
                  Supp := In_Tree.Supp_Languages.Table (Supp_Index);

                  if Supp.Index = For_Language then
                     In_Tree.Supp_Languages.Table
                       (Supp_Index).Data := Language_Processing;
                     return;
                  end if;

                  Supp_Index := Supp.Next;
               end loop;

               Supp := (Index => For_Language, Data => Language_Processing,
                        Next  => In_Project.Supp_Language_Processing);
               Supp_Language_Table.Increment_Last
                 (In_Tree.Supp_Languages);
               Supp_Index := Supp_Language_Table.Last
                               (In_Tree.Supp_Languages);
               In_Tree.Supp_Languages.Table (Supp_Index) := Supp;
               In_Project.Supp_Language_Processing := Supp_Index;
            end;
      end case;
   end Set;

   procedure Set
     (Suffix       : Name_Id;
      For_Language : Language_Index;
      In_Project   : in out Project_Data;
      In_Tree      : Project_Tree_Ref)
   is
   begin
      case For_Language is
         when No_Language_Index =>
            null;

         when First_Language_Indexes =>
            In_Project.Naming.Impl_Suffixes (For_Language) := Suffix;

         when others =>
            declare
               Supp : Supp_Suffix;
               Supp_Index : Supp_Language_Index :=
                 In_Project.Naming.Supp_Suffixes;

            begin
               while Supp_Index /= No_Supp_Language_Index loop
                  Supp := In_Tree.Supp_Suffixes.Table
                                                            (Supp_Index);

                  if Supp.Index = For_Language then
                     In_Tree.Supp_Suffixes.Table
                       (Supp_Index).Suffix := Suffix;
                     return;
                  end if;

                  Supp_Index := Supp.Next;
               end loop;

               Supp := (Index => For_Language, Suffix => Suffix,
                        Next  => In_Project.Naming.Supp_Suffixes);
               Supp_Suffix_Table.Increment_Last
                 (In_Tree.Supp_Suffixes);
               Supp_Index := Supp_Suffix_Table.Last
                 (In_Tree.Supp_Suffixes);
               In_Tree.Supp_Suffixes.Table (Supp_Index) := Supp;
               In_Project.Naming.Supp_Suffixes := Supp_Index;
            end;
      end case;
   end Set;

   -----------
   -- Slash --
   -----------

   function Slash return Name_Id is
   begin
      return Slash_Id;
   end Slash;

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

   ---------------
   -- Suffix_Of --
   ---------------

   function Suffix_Of
     (Language   : Language_Index;
      In_Project : Project_Data;
      In_Tree    : Project_Tree_Ref) return Name_Id
   is
   begin
      case Language is
         when No_Language_Index =>
            return No_Name;

         when First_Language_Indexes =>
            return In_Project.Naming.Impl_Suffixes (Language);

         when others =>
            declare
               Supp : Supp_Suffix;
               Supp_Index : Supp_Language_Index :=
                 In_Project.Naming.Supp_Suffixes;

            begin
               while Supp_Index /= No_Supp_Language_Index loop
                  Supp := In_Tree.Supp_Suffixes.Table (Supp_Index);

                  if Supp.Index = Language then
                     return Supp.Suffix;
                  end if;

                  Supp_Index := Supp.Next;
               end loop;

               return No_Name;
            end;
      end case;
   end  Suffix_Of;

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
   --  Make sure that the standard project file extension is compatible
   --  with canonical case file naming.

   Canonical_Case_File_Name (Project_File_Extension);
end Prj;
