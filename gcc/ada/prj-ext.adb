------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E X T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--             Copyright (C) 2000 Free Software Foundation, Inc.            --
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

with GNAT.HTable;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Namet;       use Namet;
with Prj.Com;     use Prj.Com;
with Stringt;     use Stringt;
with Types;       use Types;

package body Prj.Ext is

   package Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => String_Id,
      No_Element => No_String,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");

   ---------
   -- Add --
   ---------

   procedure Add
     (External_Name : String;
      Value         : String)
   is
      The_Key   : Name_Id;
      The_Value : String_Id;

   begin
      Start_String;
      Store_String_Chars (Value);
      The_Value := End_String;
      Name_Len := External_Name'Length;
      Name_Buffer (1 .. Name_Len) := External_Name;
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

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (External_Name : Name_Id;
      With_Default  : String_Id := No_String)
      return          String_Id
   is
      The_Value : String_Id;

   begin
      The_Value := Htable.Get (External_Name);

      if The_Value /= No_String then
         return The_Value;
      end if;

      --  Find if it is an environment.
      --  If it is, put the value in the hash table.

      declare
         Env_Value : constant String_Access :=
           Getenv (Get_Name_String (External_Name));

      begin
         if Env_Value /= null and then Env_Value'Length > 0 then
            Start_String;
            Store_String_Chars (Env_Value.all);
            The_Value := End_String;
            Htable.Set (External_Name, The_Value);
            return The_Value;

         else
            return With_Default;
         end if;
      end;
   end Value_Of;

end Prj.Ext;
