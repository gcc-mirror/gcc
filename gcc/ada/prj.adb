------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P R J                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2001-2003 Free Software Foundation, Inc.       --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Namet;    use Namet;
with Osint;    use Osint;
with Prj.Attr;
with Prj.Com;
with Prj.Env;
with Prj.Err;  use Prj.Err;
with Scans;    use Scans;
with Snames;   use Snames;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Prj is

   The_Empty_String : Name_Id;

   Ada_Language     : constant Name_Id := Name_Ada;

   subtype Known_Casing is Casing_Type range All_Upper_Case .. Mixed_Case;

   The_Casing_Images : constant array (Known_Casing) of String_Access :=
     (All_Lower_Case => new String'("lowercase"),
      All_Upper_Case => new String'("UPPERCASE"),
      Mixed_Case     => new String'("MixedCase"));

   Initialized : Boolean := False;

   Standard_Dot_Replacement      : constant Name_Id :=
     First_Name_Id + Character'Pos ('-');

   Std_Naming_Data : Naming_Data :=
     (Current_Language          => No_Name,
      Dot_Replacement           => Standard_Dot_Replacement,
      Dot_Repl_Loc              => No_Location,
      Casing                    => All_Lower_Case,
      Spec_Suffix               => No_Array_Element,
      Current_Spec_Suffix       => No_Name,
      Spec_Suffix_Loc           => No_Location,
      Body_Suffix               => No_Array_Element,
      Current_Body_Suffix       => No_Name,
      Body_Suffix_Loc           => No_Location,
      Separate_Suffix           => No_Name,
      Sep_Suffix_Loc            => No_Location,
      Specs                     => No_Array_Element,
      Bodies                    => No_Array_Element,
      Specification_Exceptions  => No_Array_Element,
      Implementation_Exceptions => No_Array_Element);

   Project_Empty : constant Project_Data :=
     (First_Referred_By              => No_Project,
      Name                           => No_Name,
      Path_Name                      => No_Name,
      Virtual                        => False,
      Display_Path_Name              => No_Name,
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
      Lib_Elaboration                => False,
      Standalone_Library             => False,
      Lib_Interface_ALIs             => Nil_String,
      Lib_Auto_Init                  => False,
      Symbol_Data                    => No_Symbols,
      Sources_Present                => True,
      Sources                        => Nil_String,
      Source_Dirs                    => Nil_String,
      Known_Order_Of_Source_Dirs     => True,
      Object_Directory               => No_Name,
      Display_Object_Dir             => No_Name,
      Exec_Directory                 => No_Name,
      Display_Exec_Dir               => No_Name,
      Extends                        => No_Project,
      Extended_By                    => No_Project,
      Naming                         => Std_Naming_Data,
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
      Flag1                          => False,
      Flag2                          => False,
      Depth                          => 0,
      Unkept_Comments                => False);

   -------------------
   -- Add_To_Buffer --
   -------------------

   procedure Add_To_Buffer (S : String) is
   begin
      --  If Buffer is too small, double its size

      if Buffer_Last + S'Length > Buffer'Last then
         declare
            New_Buffer : constant  String_Access :=
                           new String (1 .. 2 * Buffer'Last);

         begin
            New_Buffer (1 .. Buffer_Last) := Buffer (1 .. Buffer_Last);
            Free (Buffer);
            Buffer := New_Buffer;
         end;
      end if;

      Buffer (Buffer_Last + 1 .. Buffer_Last + S'Length) := S;
      Buffer_Last := Buffer_Last + S'Length;
   end Add_To_Buffer;

   -------------------
   -- Empty_Project --
   -------------------

   function Empty_Project return Project_Data is
   begin
      Initialize;
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
         Error_Msg (Token_Image & " expected", Token_Ptr);
      end if;
   end Expect;

   --------------------------------
   -- For_Every_Project_Imported --
   --------------------------------

   procedure For_Every_Project_Imported
     (By         : Project_Id;
      With_State : in out State)
   is

      procedure Check (Project : Project_Id);
      --  Check if a project has already been seen.
      --  If not seen, mark it as seen, call Action,
      --  and check all its imported projects.

      procedure Check (Project : Project_Id) is
         List : Project_List;

      begin
         if not Projects.Table (Project).Seen then
            Projects.Table (Project).Seen := True;
            Action (Project, With_State);

            List := Projects.Table (Project).Imported_Projects;
            while List /= Empty_Project_List loop
               Check (Project_Lists.Table (List).Project);
               List := Project_Lists.Table (List).Next;
            end loop;
         end if;
      end Check;

   begin
      for Project in Projects.First .. Projects.Last loop
         Projects.Table (Project).Seen := False;
      end loop;

      Check (Project => By);
   end For_Every_Project_Imported;

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

   procedure Initialize is
   begin
      if not Initialized then
         Initialized := True;
         Name_Len := 0;
         The_Empty_String := Name_Find;
         Empty_Name := The_Empty_String;
         Name_Len := 4;
         Name_Buffer (1 .. 4) := ".ads";
         Default_Ada_Spec_Suffix := Name_Find;
         Name_Len := 4;
         Name_Buffer (1 .. 4) := ".adb";
         Default_Ada_Body_Suffix := Name_Find;
         Name_Len := 1;
         Name_Buffer (1) := '/';
         Slash := Name_Find;
         Std_Naming_Data.Current_Spec_Suffix := Default_Ada_Spec_Suffix;
         Std_Naming_Data.Current_Body_Suffix := Default_Ada_Body_Suffix;
         Std_Naming_Data.Separate_Suffix     := Default_Ada_Body_Suffix;
         Register_Default_Naming_Scheme
           (Language            => Ada_Language,
            Default_Spec_Suffix => Default_Ada_Spec_Suffix,
            Default_Body_Suffix => Default_Ada_Body_Suffix);
         Prj.Env.Initialize;
         Prj.Attr.Initialize;
         Set_Name_Table_Byte (Name_Project,  Token_Type'Pos (Tok_Project));
         Set_Name_Table_Byte (Name_Extends,  Token_Type'Pos (Tok_Extends));
         Set_Name_Table_Byte (Name_External, Token_Type'Pos (Tok_External));
      end if;
   end Initialize;

   ------------------------------------
   -- Register_Default_Naming_Scheme --
   ------------------------------------

   procedure Register_Default_Naming_Scheme
     (Language            : Name_Id;
      Default_Spec_Suffix : Name_Id;
      Default_Body_Suffix : Name_Id)
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

      Suffix := Std_Naming_Data.Spec_Suffix;
      Found := False;

      --  Look for an element of the spec sufix array indexed by the language
      --  name. If one is found, put the default value.

      while Suffix /= No_Array_Element and then not Found loop
         Element := Array_Elements.Table (Suffix);

         if Element.Index = Lang then
            Found := True;
            Element.Value.Value := Default_Spec_Suffix;
            Array_Elements.Table (Suffix) := Element;

         else
            Suffix := Element.Next;
         end if;
      end loop;

      --  If none can be found, create a new one.

      if not Found then
         Element :=
           (Index => Lang,
            Index_Case_Sensitive => False,
            Value => (Project  => No_Project,
                      Kind     => Single,
                      Location => No_Location,
                      Default  => False,
                      Value    => Default_Spec_Suffix),
            Next  => Std_Naming_Data.Spec_Suffix);
         Array_Elements.Increment_Last;
         Array_Elements.Table (Array_Elements.Last) := Element;
         Std_Naming_Data.Spec_Suffix := Array_Elements.Last;
      end if;

      Suffix := Std_Naming_Data.Body_Suffix;
      Found := False;

      --  Look for an element of the body sufix array indexed by the language
      --  name. If one is found, put the default value.

      while Suffix /= No_Array_Element and then not Found loop
         Element := Array_Elements.Table (Suffix);

         if Element.Index = Lang then
            Found := True;
            Element.Value.Value := Default_Body_Suffix;
            Array_Elements.Table (Suffix) := Element;

         else
            Suffix := Element.Next;
         end if;
      end loop;

      --  If none can be found, create a new one.

      if not Found then
         Element :=
           (Index => Lang,
            Index_Case_Sensitive => False,
            Value => (Project  => No_Project,
                      Kind     => Single,
                      Location => No_Location,
                      Default  => False,
                      Value    => Default_Body_Suffix),
            Next  => Std_Naming_Data.Body_Suffix);
         Array_Elements.Increment_Last;
         Array_Elements.Table (Array_Elements.Last) := Element;
         Std_Naming_Data.Body_Suffix := Array_Elements.Last;
      end if;
   end Register_Default_Naming_Scheme;

   ------------
   --  Reset --
   ------------

   procedure Reset is
   begin
      Projects.Init;
      Project_Lists.Init;
      Packages.Init;
      Arrays.Init;
      Variable_Elements.Init;
      String_Elements.Init;
      Prj.Com.Units.Init;
      Prj.Com.Units_Htable.Reset;
   end Reset;

   ------------------------
   -- Same_Naming_Scheme --
   ------------------------

   function Same_Naming_Scheme
     (Left, Right : Naming_Data)
      return        Boolean
   is
   begin
      return Left.Dot_Replacement = Right.Dot_Replacement
        and then Left.Casing = Right.Casing
        and then Left.Current_Spec_Suffix = Right.Current_Spec_Suffix
        and then Left.Current_Body_Suffix = Right.Current_Body_Suffix
        and then Left.Separate_Suffix = Right.Separate_Suffix;
   end Same_Naming_Scheme;

   --------------------------
   -- Standard_Naming_Data --
   --------------------------

   function Standard_Naming_Data return Naming_Data is
   begin
      Initialize;
      return Std_Naming_Data;
   end Standard_Naming_Data;

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
