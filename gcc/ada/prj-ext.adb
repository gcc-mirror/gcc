------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E X T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2010, Free Software Foundation, Inc.         --
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
with Makeutl;       use Makeutl;
with Opt;
with Osint;         use Osint;
with Prj.Tree;      use Prj.Tree;
with Sdefault;

package body Prj.Ext is

   No_Project_Default_Dir : constant String := "-";
   --  Indicator in the project path to indicate that the default search
   --  directories should not be added to the path

   Uninitialized_Prefix : constant String := '#' & Path_Separator;
   --  Prefix to indicate that the project path has not been initilized yet.
   --  Must be two characters long

   procedure Initialize_Project_Path (Tree : Prj.Tree.Project_Node_Tree_Ref);
   --  Initialize Current_Project_Path

   ---------
   -- Add --
   ---------

   procedure Add
     (Tree          : Prj.Tree.Project_Node_Tree_Ref;
      External_Name : String;
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
      Canonical_Case_Env_Var_Name (Name_Buffer (1 .. Name_Len));
      The_Key := Name_Find;
      Name_To_Name_HTable.Set (Tree.External_References, The_Key, The_Value);
   end Add;

   ----------------------------------
   -- Add_Search_Project_Directory --
   ----------------------------------

   procedure Add_Search_Project_Directory
     (Tree : Prj.Tree.Project_Node_Tree_Ref;
      Path : String)
   is
      Tmp : String_Access;
   begin
      if Tree.Project_Path = null then
         Tree.Project_Path := new String'(Uninitialized_Prefix & Path);
      else
         Tmp := Tree.Project_Path;
         Tree.Project_Path := new String'(Tmp.all & Path_Separator & Path);
         Free (Tmp);
      end if;
   end Add_Search_Project_Directory;

   -----------
   -- Check --
   -----------

   function Check
     (Tree        : Prj.Tree.Project_Node_Tree_Ref;
      Declaration : String) return Boolean
   is
   begin
      for Equal_Pos in Declaration'Range loop
         if Declaration (Equal_Pos) = '=' then
            exit when Equal_Pos = Declaration'First;
            Add
              (Tree          => Tree,
               External_Name =>
                 Declaration (Declaration'First .. Equal_Pos - 1),
               Value         =>
                 Declaration (Equal_Pos + 1 .. Declaration'Last));
            return True;
         end if;
      end loop;

      return False;
   end Check;

   -----------------------------
   -- Initialize_Project_Path --
   -----------------------------

   procedure Initialize_Project_Path (Tree : Prj.Tree.Project_Node_Tree_Ref) is
      Add_Default_Dir : Boolean := True;
      First           : Positive;
      Last            : Positive;
      New_Len         : Positive;
      New_Last        : Positive;

      Ada_Project_Path : constant String := "ADA_PROJECT_PATH";
      Gpr_Project_Path : constant String := "GPR_PROJECT_PATH";
      --  Name of alternate env. variable that contain path name(s) of
      --  directories where project files may reside. GPR_PROJECT_PATH has
      --  precedence over ADA_PROJECT_PATH.

      Gpr_Prj_Path : String_Access := Getenv (Gpr_Project_Path);
      Ada_Prj_Path : String_Access := Getenv (Ada_Project_Path);
      --  The path name(s) of directories where project files may reside.
      --  May be empty.

   begin
      --  The current directory is always first in the search path. Since the
      --  Project_Path currently starts with '#:' as a sign that it isn't
      --  initialized, we simply replace '#' with '.'

      if Tree.Project_Path = null then
         Tree.Project_Path := new String'('.' & Path_Separator);
      else
         Tree.Project_Path (Tree.Project_Path'First) := '.';
      end if;

      --  Then the reset of the project path (if any) currently contains the
      --  directories added through Add_Search_Project_Directory

      --  If environment variables are defined and not empty, add their content

      if Gpr_Prj_Path.all /= "" then
         Add_Search_Project_Directory (Tree, Gpr_Prj_Path.all);
      end if;

      Free (Gpr_Prj_Path);

      if Ada_Prj_Path.all /= "" then
         Add_Search_Project_Directory (Tree, Ada_Prj_Path.all);
      end if;

      Free (Ada_Prj_Path);

      --  Copy to Name_Buffer, since we will need to manipulate the path

      Name_Len := Tree.Project_Path'Length;
      Name_Buffer (1 .. Name_Len) := Tree.Project_Path.all;

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
                           Normalize_Pathname
                             (Name_Buffer (First .. Last),
                              Resolve_Links => Opt.Follow_Links_For_Dirs);

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

      Free (Tree.Project_Path);

      --  Set the initial value of Current_Project_Path

      if Add_Default_Dir then
         declare
            Prefix : String_Ptr := Sdefault.Search_Dir_Prefix;

         begin
            if Prefix = null then
               Prefix := new String'(Executable_Prefix_Path);

               if Prefix.all /= "" then
                  if Tree.Target_Name /= null
                    and then Tree.Target_Name.all /= ""
                  then
                     Add_Str_To_Name_Buffer
                       (Path_Separator & Prefix.all &
                        "lib" & Directory_Separator & "gpr" &
                        Directory_Separator & Tree.Target_Name.all);
                  end if;

                  Add_Str_To_Name_Buffer
                    (Path_Separator & Prefix.all &
                     "share" & Directory_Separator & "gpr");
                  Add_Str_To_Name_Buffer
                    (Path_Separator & Prefix.all &
                     "lib" & Directory_Separator & "gnat");
               end if;

            else
               Tree.Project_Path :=
                 new String'(Name_Buffer (1 .. Name_Len) & Path_Separator &
                             Prefix.all &
                             ".." &  Directory_Separator &
                             ".." & Directory_Separator &
                             ".." & Directory_Separator & "gnat");
            end if;

            Free (Prefix);
         end;
      end if;

      if Tree.Project_Path = null then
         Tree.Project_Path := new String'(Name_Buffer (1 .. Name_Len));
      end if;
   end Initialize_Project_Path;

   ------------------
   -- Project_Path --
   ------------------

   function Project_Path (Tree : Project_Node_Tree_Ref) return String is
   begin
      if Tree.Project_Path = null
        or else Tree.Project_Path (Tree.Project_Path'First) = '#'
      then
         Initialize_Project_Path (Tree);
      end if;

      return Tree.Project_Path.all;
   end Project_Path;

   -----------
   -- Reset --
   -----------

   procedure Reset (Tree : Prj.Tree.Project_Node_Tree_Ref) is
   begin
      Name_To_Name_HTable.Reset (Tree.External_References);
   end Reset;

   ----------------------
   -- Set_Project_Path --
   ----------------------

   procedure Set_Project_Path
     (Tree     : Project_Node_Tree_Ref;
      New_Path : String) is
   begin
      Free (Tree.Project_Path);
      Tree.Project_Path := new String'(New_Path);
   end Set_Project_Path;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Tree          : Prj.Tree.Project_Node_Tree_Ref;
      External_Name : Name_Id;
      With_Default  : Name_Id := No_Name)
      return          Name_Id
   is
      The_Value : Name_Id;
      Name      : String := Get_Name_String (External_Name);

   begin
      Canonical_Case_Env_Var_Name (Name);
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      The_Value :=
        Name_To_Name_HTable.Get (Tree.External_References, Name_Find);

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
            Name_To_Name_HTable.Set
              (Tree.External_References, External_Name, The_Value);
            Free (Env_Value);
            return The_Value;

         else
            Free (Env_Value);
            return With_Default;
         end if;
      end;
   end Value_Of;

end Prj.Ext;
