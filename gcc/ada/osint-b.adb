------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              O S I N T - B                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2020, Free Software Foundation, Inc.         --
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

with Opt;      use Opt;
with Output;   use Output;

package body Osint.B is

   Current_List_File : File_Descriptor := Invalid_FD;

   -------------------------
   -- Close_Binder_Output --
   -------------------------

   procedure Close_Binder_Output is
      Status : Boolean;
   begin
      Close (Output_FD, Status);

      if not Status then
         Fail
           ("error while closing generated file "
            & Get_Name_String (Output_File_Name));
      end if;

   end Close_Binder_Output;

   ---------------------
   -- Close_List_File --
   ---------------------

   procedure Close_List_File is
   begin
      if Current_List_File /= Invalid_FD then
         Close (Current_List_File);
         Current_List_File := Invalid_FD;
         Set_Standard_Output;
      end if;
   end Close_List_File;

   --------------------------
   -- Create_Binder_Output --
   --------------------------

   procedure Create_Binder_Output
     (Output_File_Name : String;
      Typ              : Character;
      Bfile            : out Name_Id)
   is
      File_Name : String_Ptr;
      Findex1   : Natural;
      Findex2   : Natural;
      Flength   : Natural;

      Bind_File_Prefix_Len : constant Natural := 2;
      --  Length of binder file prefix (2 for b~)

   begin
      if Output_File_Name /= "" then
         Name_Buffer (1 .. Output_File_Name'Length) := Output_File_Name;
         Name_Buffer (Output_File_Name'Length + 1)  := ASCII.NUL;

         if Typ = 's' then
            Name_Buffer (Output_File_Name'Last) := 's';
         end if;

         Name_Len := Output_File_Name'Last;

      else
         Name_Buffer (1) := 'b';
         File_Name := File_Names (Current_File_Name_Index);

         Findex1 := File_Name'First;

         --  The ali file might be specified by a full path name. However,
         --  the binder generated file should always be created in the
         --  current directory, so the path might need to be stripped away.
         --  In addition to the default directory_separator allow the '/' to
         --  act as separator since this is allowed in MS-DOS and OS2 ports.

         for J in reverse File_Name'Range loop
            if File_Name (J) = Directory_Separator
              or else File_Name (J) = '/'
            then
               Findex1 := J + 1;
               exit;
            end if;
         end loop;

         Findex2 := File_Name'Last;
         while File_Name (Findex2) /=  '.' loop
            Findex2 := Findex2 - 1;
         end loop;

         Flength := Findex2 - Findex1;

         if Maximum_File_Name_Length > 0 then

            --  Make room for the extra two characters in "b?"

            while Int (Flength) >
              Maximum_File_Name_Length - Nat (Bind_File_Prefix_Len)
            loop
               Findex2 := Findex2 - 1;
               Flength := Findex2 - Findex1;
            end loop;
         end if;

         Name_Buffer
           (Bind_File_Prefix_Len + 1 .. Flength + Bind_File_Prefix_Len) :=
              File_Name (Findex1 .. Findex2 - 1);
         Name_Buffer (Flength + Bind_File_Prefix_Len + 1) := '.';

         --  Ada bind file, name is b~xxx.adb or b~xxx.ads

         Name_Buffer (2) := '~';

         Name_Buffer (Flength + Bind_File_Prefix_Len + 2) := 'a';
         Name_Buffer (Flength + Bind_File_Prefix_Len + 3) := 'd';
         Name_Buffer (Flength + Bind_File_Prefix_Len + 4) := Typ;
         Name_Buffer (Flength + Bind_File_Prefix_Len + 5) := ASCII.NUL;
         Name_Len := Flength + Bind_File_Prefix_Len + 4;
      end if;

      Bfile := Name_Find;

      Create_File_And_Check (Output_FD, Text);
   end Create_Binder_Output;

   --------------------
   -- More_Lib_Files --
   --------------------

   function More_Lib_Files return Boolean renames More_Files;

   ------------------------
   -- Next_Main_Lib_File --
   ------------------------

   function Next_Main_Lib_File return File_Name_Type renames Next_Main_File;

   ---------------------------------
   -- Set_Current_File_Name_Index --
   ---------------------------------

   procedure Set_Current_File_Name_Index (To : Int) is
   begin
      Current_File_Name_Index := To;
   end Set_Current_File_Name_Index;

   -------------------
   -- Set_List_File --
   -------------------

   procedure Set_List_File (Filename : String) is
   begin
      pragma Assert (Current_List_File = Invalid_FD);
      Current_List_File := Create_File (Filename, Text);

      if Current_List_File = Invalid_FD then
         Fail ("cannot create list file: " & Filename);
      else
         Set_Output (Current_List_File);
      end if;
   end Set_List_File;

   -----------------------
   -- Write_Binder_Info --
   -----------------------

   procedure Write_Binder_Info (Info : String) renames Write_Info;

begin
   Set_Program (Binder);
end Osint.B;
