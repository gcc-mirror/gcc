------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E X T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2005, Free Software Foundation, Inc.         --
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

with Namet;   use Namet;
with Output;  use Output;
with Osint;   use Osint;
with Sdefault;

with GNAT.HTable;

package body Prj.Ext is

   Gpr_Project_Path : constant String := "GPR_PROJECT_PATH";
   Ada_Project_Path : constant String := "ADA_PROJECT_PATH";
   --  Name of the env. variables that contain path name(s) of directories
   --  where project files may reside. GPR_PROJECT_PATH has precedence over
   --  ADA_PROJECT_PATH.

   Gpr_Prj_Path : constant String_Access := Getenv (Gpr_Project_Path);
   Ada_Prj_Path : constant String_Access := Getenv (Ada_Project_Path);
   --  The path name(s) of directories where project files may reside.
   --  May be empty.

   No_Project_Default_Dir : constant String := "-";

   Current_Project_Path : String_Access;
   --  The project path. Initialized during elaboration of package Contains at
   --  least the current working directory.

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

   ------------------
   -- Project_Path --
   ------------------

   function Project_Path return String is
   begin
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

begin
   --  Initialize Current_Project_Path during package elaboration

   declare
      Add_Default_Dir : Boolean := True;
      First           : Positive;
      Last            : Positive;
      New_Len         : Positive;
      New_Last        : Positive;
      Prj_Path        : String_Access := Gpr_Prj_Path;

   begin
      if Gpr_Prj_Path.all /= "" then

         --  Warn if both environment variables are defined

         if Ada_Prj_Path.all /= "" then
            Write_Line ("Warning: ADA_PROJECT_PATH is not taken into account");
            Write_Line ("         when GPR_PROJECT_PATH is defined");
         end if;

      else
         Prj_Path := Ada_Prj_Path;
      end if;

      --  The current directory is always first

      Name_Len := 1;
      Name_Buffer (Name_Len) := '.';

      --  If environment variable is defined and not empty, add its content

      if Prj_Path.all /= "" then
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Path_Separator;

         Add_Str_To_Name_Buffer (Prj_Path.all);

         --  Scan the directory path to see if "-" is one of the directories.
         --  Remove each occurence of "-" and set Add_Default_Dir to False.
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

            else
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
      end if;

      --  Set the initial value of Current_Project_Path

      if Add_Default_Dir then
         Current_Project_Path :=
           new String'(Name_Buffer (1 .. Name_Len) & Path_Separator &
                       Sdefault.Search_Dir_Prefix.all & ".." &
                       Directory_Separator & ".." & Directory_Separator &
                       ".." & Directory_Separator & "gnat");
      else
         Current_Project_Path := new String'(Name_Buffer (1 .. Name_Len));
      end if;
   end;
end Prj.Ext;
