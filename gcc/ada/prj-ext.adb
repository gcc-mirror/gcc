------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E X T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2008, Free Software Foundation, Inc.         --
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

with Hostparm;
with Makeutl;  use Makeutl;
with Output;   use Output;
with Osint;    use Osint;
with Sdefault;
with Table;

with GNAT.HTable;

package body Prj.Ext is

   Ada_Project_Path : constant String := "ADA_PROJECT_PATH";
   --  Name of alternate env. variable that contain path name(s) of directories
   --  where project files may reside. GPR_PROJECT_PATH has precedence over
   --  ADA_PROJECT_PATH.

   Gpr_Prj_Path : constant String_Access := Getenv (Gpr_Project_Path);
   Ada_Prj_Path : constant String_Access := Getenv (Ada_Project_Path);
   --  The path name(s) of directories where project files may reside.
   --  May be empty.

   No_Project_Default_Dir : constant String := "-";

   Current_Project_Path : String_Access;
   --  The project path. Initialized by procedure Initialize_Project_Path
   --  below.

   procedure Initialize_Project_Path;
   --  Initialize Current_Project_Path

   package Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Name_Id,
      No_Element => No_Name,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  External references are stored in this hash table, either by procedure
   --  Add (directly or through a call to function Check) or by function
   --  Value_Of when an environment variable is found non empty. Value_Of
   --  first for external reference in this table, before checking the
   --  environment. Htable is emptied (reset) by procedure Reset.

   package Search_Directories is new Table.Table
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 4,
      Table_Increment      => 100,
      Table_Name           => "Prj.Ext.Search_Directories");
   --  The table for the directories specified with -aP switches

   ---------
   -- Add --
   ---------

   procedure Add
     (External_Name : String;
      Value         : String)
   is
      The_Key   : Name_Id;
      The_Value : Name_Id;
   begin
      Name_Len := Value'Length;
      Name_Buffer (1 .. Name_Len) := Value;
      The_Value := Name_Find;
      Name_Len := External_Name'Length;
      Name_Buffer (1 .. Name_Len) := External_Name;
      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
      The_Key := Name_Find;
      Htable.Set (The_Key, The_Value);
   end Add;

   -----------
   ----------------------------------
   -- Add_Search_Project_Directory --
   ----------------------------------

   procedure Add_Search_Project_Directory (Path : String) is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Path);
      Search_Directories.Append (Name_Find);
   end Add_Search_Project_Directory;

   -- Check --
   -----------

   function Check (Declaration : String) return Boolean is
   begin
      for Equal_Pos in Declaration'Range loop
         if Declaration (Equal_Pos) = '=' then
            exit when Equal_Pos = Declaration'First;
            exit when Equal_Pos = Declaration'Last;
            Add
              (External_Name =>
                 Declaration (Declaration'First .. Equal_Pos - 1),
               Value =>
                 Declaration (Equal_Pos + 1 .. Declaration'Last));
            return True;
         end if;
      end loop;

      return False;
   end Check;

   -----------------------------
   -- Initialize_Project_Path --
   -----------------------------

   procedure Initialize_Project_Path is
      Add_Default_Dir : Boolean := True;
      First           : Positive;
      Last            : Positive;
      New_Len         : Positive;
      New_Last        : Positive;
      Prj_Path        : String_Access := Gpr_Prj_Path;

   begin
      if Gpr_Prj_Path.all /= "" then

         --  In Ada only mode, warn if both environment variables are defined

         if Get_Mode = Ada_Only and then Ada_Prj_Path.all /= "" then
            Write_Line
              ("Warning: ADA_PROJECT_PATH is not taken into account");
            Write_Line ("         when GPR_PROJECT_PATH is defined");
         end if;

      else
         Prj_Path := Ada_Prj_Path;
      end if;

      --  The current directory is always first

      Name_Len := 1;
      Name_Buffer (Name_Len) := '.';

      --  If there are directories in the Search_Directories table, add them

      for J in 1 .. Search_Directories.Last loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Path_Separator;
         Add_Str_To_Name_Buffer
           (Get_Name_String (Search_Directories.Table (J)));
      end loop;

      --  If environment variable is defined and not empty, add its content

      if Prj_Path.all /= "" then
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Path_Separator;

         Add_Str_To_Name_Buffer (Prj_Path.all);
      end if;

      --  Scan the directory path to see if "-" is one of the directories.
      --  Remove each occurrence of "-" and set Add_Default_Dir to False.
      --  Also resolve relative paths and symbolic links.

      First := 3;
      loop
         while First <= Name_Len
           and then (Name_Buffer (First) = Path_Separator)
         loop
            First := First + 1;
         end loop;

         exit when First > Name_Len;

         Last := First;

         while Last < Name_Len
           and then Name_Buffer (Last + 1) /= Path_Separator
         loop
            Last := Last + 1;
         end loop;

         --  If the directory is "-", set Add_Default_Dir to False and
         --  remove from path.

         if Name_Buffer (First .. Last) = No_Project_Default_Dir then
            Add_Default_Dir := False;

            for J in Last + 1 .. Name_Len loop
               Name_Buffer (J - No_Project_Default_Dir'Length - 1) :=
                 Name_Buffer (J);
            end loop;

            Name_Len := Name_Len - No_Project_Default_Dir'Length - 1;

            --  After removing the '-', go back one character to get the next
            --  directory correctly.

            Last := Last - 1;

         elsif not Hostparm.OpenVMS
           or else not Is_Absolute_Path (Name_Buffer (First .. Last))
         then
            --  On VMS, only expand relative path names, as absolute paths
            --  may correspond to multi-valued VMS logical names.

            declare
               New_Dir : constant String :=
                           Normalize_Pathname (Name_Buffer (First .. Last));

            begin
               --  If the absolute path was resolved and is different from
               --  the original, replace original with the resolved path.

               if New_Dir /= Name_Buffer (First .. Last)
                 and then New_Dir'Length /= 0
               then
                  New_Len := Name_Len + New_Dir'Length - (Last - First + 1);
                  New_Last := First + New_Dir'Length - 1;
                  Name_Buffer (New_Last + 1 .. New_Len) :=
                    Name_Buffer (Last + 1 .. Name_Len);
                  Name_Buffer (First .. New_Last) := New_Dir;
                  Name_Len := New_Len;
                  Last := New_Last;
               end if;
            end;
         end if;

         First := Last + 1;
      end loop;

      --  Set the initial value of Current_Project_Path

      if Add_Default_Dir then
         declare
            Prefix : String_Ptr := Sdefault.Search_Dir_Prefix;
         begin
            if Prefix = null then
               Prefix := new String'(Executable_Prefix_Path);

               if Prefix.all /= "" then
                  if Get_Mode = Multi_Language then
                     Add_Str_To_Name_Buffer
                       (Path_Separator & Prefix.all &
                        Directory_Separator & "share" &
                        Directory_Separator & "gpr");
                  end if;

                  Add_Str_To_Name_Buffer
                    (Path_Separator & Prefix.all &
                     Directory_Separator & "lib" &
                     Directory_Separator & "gnat");
               end if;

            else
               Current_Project_Path :=
                 new String'(Name_Buffer (1 .. Name_Len) & Path_Separator &
                             Prefix.all &
                             ".." &  Directory_Separator &
                             ".." & Directory_Separator &
                             ".." & Directory_Separator & "gnat");
            end if;
         end;
      end if;

      if Current_Project_Path = null then
         Current_Project_Path := new String'(Name_Buffer (1 .. Name_Len));
      end if;
   end Initialize_Project_Path;

   ------------------
   -- Project_Path --
   ------------------

   function Project_Path return String is
   begin
      if Current_Project_Path = null then
         Initialize_Project_Path;
      end if;

      return Current_Project_Path.all;
   end Project_Path;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Htable.Reset;
   end Reset;

   ----------------------
   -- Set_Project_Path --
   ----------------------

   procedure Set_Project_Path (New_Path : String) is
   begin
      Free (Current_Project_Path);
      Current_Project_Path := new String'(New_Path);
   end Set_Project_Path;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (External_Name : Name_Id;
      With_Default  : Name_Id := No_Name)
      return          Name_Id
   is
      The_Value : Name_Id;
      Name      : String := Get_Name_String (External_Name);

   begin
      Canonical_Case_File_Name (Name);
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      The_Value := Htable.Get (Name_Find);

      if The_Value /= No_Name then
         return The_Value;
      end if;

      --  Find if it is an environment, if it is, put value in the hash table

      declare
         Env_Value : String_Access := Getenv (Name);

      begin
         if Env_Value /= null and then Env_Value'Length > 0 then
            Name_Len := Env_Value'Length;
            Name_Buffer (1 .. Name_Len) := Env_Value.all;
            The_Value := Name_Find;
            Htable.Set (External_Name, The_Value);
            Free (Env_Value);
            return The_Value;

         else
            Free (Env_Value);
            return With_Default;
         end if;
      end;
   end Value_Of;

end Prj.Ext;
