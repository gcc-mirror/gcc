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

with Osint;    use Osint;
with Prj.Tree; use Prj.Tree;

package body Prj.Ext is

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

   -----------
   -- Reset --
   -----------

   procedure Reset (Tree : Prj.Tree.Project_Node_Tree_Ref) is
   begin
      Name_To_Name_HTable.Reset (Tree.External_References);
   end Reset;

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
