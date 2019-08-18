------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              O S I N T - C                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2019, Free Software Foundation, Inc.         --
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

with Opt;     use Opt;
with Tree_IO; use Tree_IO;

package body Osint.C is

   Output_Object_File_Name : String_Ptr;
   --  Argument of -o compiler option, if given. This is needed to verify
   --  consistency with the ALI file name.

   procedure Adjust_OS_Resource_Limits;
   pragma Import (C, Adjust_OS_Resource_Limits,
                  "__gnat_adjust_os_resource_limits");
   --  Procedure to make system specific adjustments to make GNAT run better

   function Create_Auxiliary_File
     (Src    : File_Name_Type;
      Suffix : String) return File_Name_Type;
   --  Common processing for Create_List_File, Create_Repinfo_File and
   --  Create_Debug_File. Src is the file name used to create the required
   --  output file and Suffix is the desired suffix (dg/rep/xxx for debug/
   --  repinfo/list file where xxx is specified extension.

   ------------------
   -- Close_C_File --
   ------------------

   procedure Close_C_File is
      Status : Boolean;

   begin
      Close (Output_FD, Status);

      if not Status then
         Fail
           ("error while closing file "
            & Get_Name_String (Output_File_Name));
      end if;
   end Close_C_File;

   ----------------------
   -- Close_Debug_File --
   ----------------------

   procedure Close_Debug_File is
      Status : Boolean;

   begin
      Close (Output_FD, Status);

      if not Status then
         Fail
           ("error while closing expanded source file "
            & Get_Name_String (Output_File_Name));
      end if;
   end Close_Debug_File;

   ------------------
   -- Close_H_File --
   ------------------

   procedure Close_H_File is
      Status : Boolean;

   begin
      Close (Output_FD, Status);

      if not Status then
         Fail
           ("error while closing file "
            & Get_Name_String (Output_File_Name));
      end if;
   end Close_H_File;

   ---------------------
   -- Close_List_File --
   ---------------------

   procedure Close_List_File is
      Status : Boolean;

   begin
      Close (Output_FD, Status);

      if not Status then
         Fail
           ("error while closing list file "
            & Get_Name_String (Output_File_Name));
      end if;
   end Close_List_File;

   -------------------------------
   -- Close_Output_Library_Info --
   -------------------------------

   procedure Close_Output_Library_Info is
      Status : Boolean;

   begin
      Close (Output_FD, Status);

      if not Status then
         Fail
           ("error while closing ALI file "
            & Get_Name_String (Output_File_Name));
      end if;
   end Close_Output_Library_Info;

   ------------------------
   -- Close_Repinfo_File --
   ------------------------

   procedure Close_Repinfo_File is
      Status : Boolean;

   begin
      Close (Output_FD, Status);

      if not Status then
         Fail
           ("error while closing representation info file "
            & Get_Name_String (Output_File_Name));
      end if;
   end Close_Repinfo_File;

   ---------------------------
   -- Create_Auxiliary_File --
   ---------------------------

   function Create_Auxiliary_File
     (Src    : File_Name_Type;
      Suffix : String) return File_Name_Type
   is
      Result : File_Name_Type;

   begin
      Get_Name_String (Src);

      Name_Buffer (Name_Len + 1) := '.';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len + 1 .. Name_Len + Suffix'Length) := Suffix;
      Name_Len := Name_Len + Suffix'Length;

      if Output_Object_File_Name /= null then
         for Index in reverse Output_Object_File_Name'Range loop
            if Output_Object_File_Name (Index) = Directory_Separator then
               declare
                  File_Name : constant String := Name_Buffer (1 .. Name_Len);
               begin
                  Name_Len := Index - Output_Object_File_Name'First + 1;
                  Name_Buffer (1 .. Name_Len) :=
                    Output_Object_File_Name
                      (Output_Object_File_Name'First .. Index);
                  Name_Buffer (Name_Len + 1 .. Name_Len + File_Name'Length) :=
                    File_Name;
                  Name_Len := Name_Len + File_Name'Length;
               end;

               exit;
            end if;
         end loop;
      end if;

      Result := Name_Find;
      Name_Buffer (Name_Len + 1) := ASCII.NUL;
      Create_File_And_Check (Output_FD, Text);
      return Result;
   end Create_Auxiliary_File;

   -------------------
   -- Create_C_File --
   -------------------

   procedure Create_C_File is
      Dummy : Boolean;
   begin
      Set_File_Name ("c");
      Delete_File (Name_Buffer (1 .. Name_Len), Dummy);
      Create_File_And_Check (Output_FD, Text);
   end Create_C_File;

   -----------------------
   -- Create_Debug_File --
   -----------------------

   function Create_Debug_File (Src : File_Name_Type) return File_Name_Type is
   begin
      return Create_Auxiliary_File (Src, "dg");
   end Create_Debug_File;

   -------------------
   -- Create_H_File --
   -------------------

   procedure Create_H_File is
      Dummy : Boolean;
   begin
      Set_File_Name ("h");
      Delete_File (Name_Buffer (1 .. Name_Len), Dummy);
      Create_File_And_Check (Output_FD, Text);
   end Create_H_File;

   ----------------------
   -- Create_List_File --
   ----------------------

   procedure Create_List_File (S : String) is
      Dummy : File_Name_Type;
   begin
      if S (S'First) = '.' then
         Dummy :=
           Create_Auxiliary_File (Current_Main, S (S'First + 1 .. S'Last));
      else
         Name_Buffer (1 .. S'Length) := S;
         Name_Len := S'Length + 1;
         Name_Buffer (Name_Len) := ASCII.NUL;
         Create_File_And_Check (Output_FD, Text);
      end if;
   end Create_List_File;

   --------------------------------
   -- Create_Output_Library_Info --
   --------------------------------

   procedure Create_Output_Library_Info is
      Dummy : Boolean;
   begin
      Set_File_Name (ALI_Suffix.all);
      Delete_File (Name_Buffer (1 .. Name_Len), Dummy);
      Create_File_And_Check (Output_FD, Text);
   end Create_Output_Library_Info;

   ------------------------------
   -- Open_Output_Library_Info --
   ------------------------------

   procedure Open_Output_Library_Info is
   begin
      Set_File_Name (ALI_Suffix.all);
      Open_File_To_Append_And_Check (Output_FD, Text);
   end Open_Output_Library_Info;

   -------------------------
   -- Create_Repinfo_File --
   -------------------------

   procedure Create_Repinfo_File (Src : String) is
      Discard : File_Name_Type;
   begin
      Name_Buffer (1 .. Src'Length) := Src;
      Name_Len := Src'Length;
      if List_Representation_Info_To_JSON then
         Discard := Create_Auxiliary_File (Name_Find, "json");
      else
         Discard := Create_Auxiliary_File (Name_Find, "rep");
      end if;
   end Create_Repinfo_File;

   ---------------------------
   -- Debug_File_Eol_Length --
   ---------------------------

   function Debug_File_Eol_Length return Nat is
   begin
      --  There has to be a cleaner way to do this ???

      if Directory_Separator = '/' then
         return 1;
      else
         return 2;
      end if;
   end Debug_File_Eol_Length;

   -------------------
   -- Delete_C_File --
   -------------------

   procedure Delete_C_File is
      Dummy : Boolean;
   begin
      Set_File_Name ("c");
      Delete_File (Name_Buffer (1 .. Name_Len), Dummy);
   end Delete_C_File;

   -------------------
   -- Delete_H_File --
   -------------------

   procedure Delete_H_File is
      Dummy : Boolean;
   begin
      Set_File_Name ("h");
      Delete_File (Name_Buffer (1 .. Name_Len), Dummy);
   end Delete_H_File;

   ---------------------------------
   -- Get_Output_Object_File_Name --
   ---------------------------------

   function Get_Output_Object_File_Name return String is
   begin
      pragma Assert (Output_Object_File_Name /= null);

      return Output_Object_File_Name.all;
   end Get_Output_Object_File_Name;

   -----------------------
   -- More_Source_Files --
   -----------------------

   function More_Source_Files return Boolean renames More_Files;

   ----------------------
   -- Next_Main_Source --
   ----------------------

   function Next_Main_Source return File_Name_Type renames Next_Main_File;

   -----------------------
   -- Read_Library_Info --
   -----------------------

   procedure Read_Library_Info
     (Name : out File_Name_Type;
      Text : out Text_Buffer_Ptr)
   is
   begin
      Set_File_Name (ALI_Suffix.all);

      --  Remove trailing NUL that comes from Set_File_Name above. This is
      --  needed for consistency with names that come from Scan_ALI and thus
      --  preventing repeated scanning of the same file.

      pragma Assert (Name_Len > 1 and then Name_Buffer (Name_Len) = ASCII.NUL);
      Name_Len := Name_Len - 1;

      Name := Name_Find;
      Text := Read_Library_Info (Name, Fatal_Err => False);
   end Read_Library_Info;

   -------------------
   -- Set_File_Name --
   -------------------

   procedure Set_File_Name (Ext : String) is
      Dot_Index : Natural;

   begin
      Get_Name_String (Current_Main);

      --  Find last dot since we replace the existing extension by .ali. The
      --  initialization to Name_Len + 1 provides for simply adding the .ali
      --  extension if the source file name has no extension.

      Dot_Index := Name_Len + 1;

      for J in reverse 1 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Dot_Index := J;
            exit;
         end if;
      end loop;

      --  If we are in multiple-units-per-file mode, then add a ~nnn extension
      --  to the name.

      if Multiple_Unit_Index /= 0 then
         declare
            Exten : constant String := Name_Buffer (Dot_Index .. Name_Len);
         begin
            Name_Len := Dot_Index - 1;
            Add_Char_To_Name_Buffer (Multi_Unit_Index_Character);
            Add_Nat_To_Name_Buffer (Multiple_Unit_Index);
            Dot_Index := Name_Len + 1;
            Add_Str_To_Name_Buffer (Exten);
         end;
      end if;

      --  Make sure that the output file name matches the source file name.
      --  To compare them, remove file name directories and extensions.

      if Output_Object_File_Name /= null then

         --  Make sure there is a dot at Dot_Index. This may not be the case
         --  if the source file name has no extension.

         Name_Buffer (Dot_Index) := '.';

         --  Remove extension preparing to replace it

         declare
            Name  : String  := Name_Buffer (1 .. Dot_Index);
            First : Positive;

         begin
            Name_Buffer (1 .. Output_Object_File_Name'Length) :=
              Output_Object_File_Name.all;

            --  Put two names in canonical case, to allow object file names
            --  with upper-case letters on Windows.

            Canonical_Case_File_Name (Name);
            Canonical_Case_File_Name
              (Name_Buffer (1 .. Output_Object_File_Name'Length));

            Dot_Index := 0;
            for J in reverse Output_Object_File_Name'Range loop
               if Name_Buffer (J) = '.' then
                  Dot_Index := J;
                  exit;
               end if;
            end loop;

            --  Dot_Index should not be zero now (we check for extension
            --  elsewhere).

            pragma Assert (Dot_Index /= 0);

            --  Look for first character of file name

            First := Dot_Index;
            while First > 1
              and then Name_Buffer (First - 1) /= Directory_Separator
              and then Name_Buffer (First - 1) /= '/'
            loop
               First := First - 1;
            end loop;

            --  Check name of object file is what we expect

            if Name /= Name_Buffer (First .. Dot_Index) then
               Fail ("incorrect object file name");
            end if;
         end;
      end if;

      Name_Buffer (Dot_Index) := '.';
      Name_Buffer (Dot_Index + 1 .. Dot_Index + Ext'Length) := Ext;
      Name_Buffer (Dot_Index + Ext'Length + 1) := ASCII.NUL;
      Name_Len := Dot_Index + Ext'Length + 1;
   end Set_File_Name;

   ---------------------------------
   -- Set_Output_Object_File_Name --
   ---------------------------------

   procedure Set_Output_Object_File_Name (Name : String) is
      Ext : constant String  := Target_Object_Suffix;
      NL  : constant Natural := Name'Length;
      EL  : constant Natural := Ext'Length;

   begin
      --  Make sure that the object file has the expected extension

      if NL <= EL
         or else
          (Name (NL - EL + Name'First .. Name'Last) /= Ext
             and then Name (NL - 2 + Name'First .. Name'Last) /= ".o"
             and then
               (not Generate_C_Code
                  or else Name (NL - 2 + Name'First .. Name'Last) /= ".c"))
      then
         Fail ("incorrect object file extension");
      end if;

      Output_Object_File_Name := new String'(Name);
   end Set_Output_Object_File_Name;

   ----------------
   -- Tree_Close --
   ----------------

   procedure Tree_Close is
      Status : Boolean;
   begin
      Tree_Write_Terminate;
      Close (Output_FD, Status);

      if not Status then
         Fail
           ("error while closing tree file "
            & Get_Name_String (Output_File_Name));
      end if;
   end Tree_Close;

   -----------------
   -- Tree_Create --
   -----------------

   procedure Tree_Create is
      Dot_Index : Natural;

   begin
      Get_Name_String (Current_Main);

      --  If an object file has been specified, then the ALI file
      --  will be in the same directory as the object file;
      --  so, we put the tree file in this same directory,
      --  even though no object file needs to be generated.

      if Output_Object_File_Name /= null then
         Name_Len := Output_Object_File_Name'Length;
         Name_Buffer (1 .. Name_Len) := Output_Object_File_Name.all;
      end if;

      Dot_Index := Name_Len + 1;

      for J in reverse 1 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Dot_Index := J;
            exit;
         end if;
      end loop;

      --  Should be impossible to not have an extension

      pragma Assert (Dot_Index /= 0);

      --  Change extension to adt

      Name_Buffer (Dot_Index) := '.';
      Name_Buffer (Dot_Index + 1) := 'a';
      Name_Buffer (Dot_Index + 2) := 'd';
      Name_Buffer (Dot_Index + 3) := 't';
      Name_Buffer (Dot_Index + 4) := ASCII.NUL;
      Name_Len := Dot_Index + 3;
      Create_File_And_Check (Output_FD, Binary);

      Tree_Write_Initialize (Output_FD);
   end Tree_Create;

   -----------------------
   -- Write_Debug_Info --
   -----------------------

   procedure Write_Debug_Info (Info : String) renames Write_Info;

   ------------------------
   -- Write_Library_Info --
   ------------------------

   procedure Write_Library_Info (Info : String) renames Write_Info;

   ---------------------
   -- Write_List_Info --
   ---------------------

   procedure Write_List_Info (S : String) is
   begin
      Write_With_Check (S'Address, S'Length);
   end Write_List_Info;

   ------------------------
   -- Write_Repinfo_Line --
   ------------------------

   procedure Write_Repinfo_Line (Info : String) renames Write_Info;

begin
   Adjust_OS_Resource_Limits;

   Opt.Create_Repinfo_File_Access := Create_Repinfo_File'Access;
   Opt.Write_Repinfo_Line_Access  := Write_Repinfo_Line'Access;
   Opt.Close_Repinfo_File_Access  := Close_Repinfo_File'Access;

   Opt.Create_List_File_Access := Create_List_File'Access;
   Opt.Write_List_Info_Access  := Write_List_Info'Access;
   Opt.Close_List_File_Access  := Close_List_File'Access;

   Set_Program (Compiler);
end Osint.C;
