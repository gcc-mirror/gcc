------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E X T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2000-2003 Free Software Foundation, Inc.       --
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

with Namet;   use Namet;
with Osint;   use Osint;
with Prj.Com; use Prj.Com;
with Types;   use Types;

with GNAT.HTable;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Prj.Ext is

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

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Htable.Reset;
   end Reset;

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

      --  Find if it is an environment.
      --  If it is, put the value in the hash table.

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
