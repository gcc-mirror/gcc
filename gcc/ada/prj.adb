------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P R J                                   --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Errout;      use Errout;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Namet;       use Namet;
with Prj.Attr;
with Prj.Com;
with Prj.Env;
with Scans;       use Scans;
with Scn;
with Stringt;     use Stringt;
with Sinfo.CN;
with Snames;      use Snames;

package body Prj is

   The_Empty_String        : String_Id;

   subtype Known_Casing is Casing_Type range All_Upper_Case .. Mixed_Case;

   The_Casing_Images : array (Known_Casing) of String_Access :=
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
      Specification_Suffix      => No_Array_Element,
      Current_Spec_Suffix       => No_Name,
      Spec_Suffix_Loc           => No_Location,
      Implementation_Suffix     => No_Array_Element,
      Current_Impl_Suffix       => No_Name,
      Impl_Suffix_Loc           => No_Location,
      Separate_Suffix           => No_Name,
      Sep_Suffix_Loc            => No_Location,
      Specifications            => No_Array_Element,
      Bodies                    => No_Array_Element,
      Specification_Exceptions  => No_Array_Element,
      Implementation_Exceptions => No_Array_Element);

   Project_Empty : constant Project_Data :=
     (First_Referred_By            => No_Project,
      Name                         => No_Name,
      Path_Name                    => No_Name,
      Location                     => No_Location,
      Directory                    => No_Name,
      Library                      => False,
      Library_Dir                  => No_Name,
      Library_Name                 => No_Name,
      Library_Kind                 => Static,
      Lib_Internal_Name            => No_Name,
      Lib_Elaboration              => False,
      Sources_Present              => True,
      Sources                      => Nil_String,
      Source_Dirs                  => Nil_String,
      Object_Directory             => No_Name,
      Exec_Directory               => No_Name,
      Modifies                     => No_Project,
      Modified_By                  => No_Project,
      Naming                       => Std_Naming_Data,
      Decl                         => No_Declarations,
      Imported_Projects            => Empty_Project_List,
      Include_Path                 => null,
      Objects_Path                 => null,
      Config_File_Name             => No_Name,
      Config_File_Temp             => False,
      Config_Checked               => False,
      Language_Independent_Checked => False,
      Checked                      => False,
      Seen                         => False,
      Flag1                        => False,
      Flag2                        => False);

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

   function Empty_String return String_Id is
   begin
      return The_Empty_String;
   end Empty_String;

   ------------
   -- Expect --
   ------------

   procedure Expect (The_Token : Token_Type; Token_Image : String) is
   begin
      if Token /= The_Token then
         Error_Msg ("""" & Token_Image & """ expected", Token_Ptr);
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
            Projects.Table (Project).Seen := False;
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
         Stringt.Initialize;
         Start_String;
         The_Empty_String := End_String;
         Name_Len := 4;
         Name_Buffer (1 .. 4) := ".ads";
         Default_Ada_Spec_Suffix := Name_Find;
         Name_Len := 4;
         Name_Buffer (1 .. 4) := ".adb";
         Default_Ada_Impl_Suffix := Name_Find;
         Std_Naming_Data.Current_Spec_Suffix := Default_Ada_Spec_Suffix;
         Std_Naming_Data.Current_Impl_Suffix := Default_Ada_Impl_Suffix;
         Std_Naming_Data.Separate_Suffix     := Default_Ada_Impl_Suffix;
         Prj.Env.Initialize;
         Prj.Attr.Initialize;
         Set_Name_Table_Byte (Name_Project,  Token_Type'Pos (Tok_Project));
         Set_Name_Table_Byte (Name_Extends,  Token_Type'Pos (Tok_Extends));
         Set_Name_Table_Byte (Name_External, Token_Type'Pos (Tok_External));
      end if;
   end Initialize;

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
        and then Left.Current_Impl_Suffix = Right.Current_Impl_Suffix
        and then Left.Separate_Suffix = Right.Separate_Suffix;
   end Same_Naming_Scheme;

   ----------
   -- Scan --
   ----------

   procedure Scan is
   begin
      Scn.Scan;

      --  Change operator symbol to literal strings, since that's the way
      --  we treat all strings in a project file.

      if Token = Tok_Operator_Symbol then
         Sinfo.CN.Change_Operator_Symbol_To_String_Literal (Token_Node);
         Token := Tok_String_Literal;
      end if;
   end Scan;

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

end Prj;
