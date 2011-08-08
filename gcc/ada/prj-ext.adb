------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E X T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2011, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Deallocation;

package body Prj.Ext is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : out External_References;
      Copy_From : External_References := No_External_Refs)
   is
      N  : Name_To_Name_Ptr;
      N2 : Name_To_Name_Ptr;
   begin
      if Self.Refs = null then
         Self.Refs := new Name_To_Name_HTable.Instance;

         if Copy_From.Refs /= null then
            N := Name_To_Name_HTable.Get_First (Copy_From.Refs.all);
            while N /= null loop
               N2 := new Name_To_Name'
                           (Key    => N.Key,
                            Value  => N.Value,
                            Source => N.Source,
                            Next   => null);
               Name_To_Name_HTable.Set (Self.Refs.all, N2);
               N := Name_To_Name_HTable.Get_Next (Copy_From.Refs.all);
            end loop;
         end if;
      end if;
   end Initialize;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self          : External_References;
      External_Name : String;
      Value         : String;
      Source        : External_Source := External_Source'First)
   is
      Key : Name_Id;
      N   : Name_To_Name_Ptr;

   begin
      Name_Len := External_Name'Length;
      Name_Buffer (1 .. Name_Len) := External_Name;
      Canonical_Case_Env_Var_Name (Name_Buffer (1 .. Name_Len));
      Key := Name_Find;

      --  Check whether the value is already defined, to properly respect the
      --  overriding order.

      if Source /= External_Source'First then
         N := Name_To_Name_HTable.Get (Self.Refs.all, Key);

         if N /= null then
            if External_Source'Pos (N.Source) <
               External_Source'Pos (Source)
            then
               if Current_Verbosity = High then
                  Debug_Output
                    ("Not overridding existing variable '" & External_Name
                     & "', value was defined in " & N.Source'Img);
               end if;
               return;
            end if;
         end if;
      end if;

      Name_Len := Value'Length;
      Name_Buffer (1 .. Name_Len) := Value;
      N := new Name_To_Name'
                 (Key    => Key,
                  Source => Source,
                  Value  => Name_Find,
                  Next   => null);

      if Current_Verbosity = High then
         Debug_Output ("Add external (" & External_Name & ") is", N.Value);
      end if;

      Name_To_Name_HTable.Set (Self.Refs.all, N);
   end Add;

   -----------
   -- Check --
   -----------

   function Check
     (Self        : External_References;
      Declaration : String) return Boolean
   is
   begin
      for Equal_Pos in Declaration'Range loop
         if Declaration (Equal_Pos) = '=' then
            exit when Equal_Pos = Declaration'First;
            Add
              (Self          => Self,
               External_Name =>
                 Declaration (Declaration'First .. Equal_Pos - 1),
               Value         =>
                 Declaration (Equal_Pos + 1 .. Declaration'Last),
               Source        => From_Command_Line);
            return True;
         end if;
      end loop;

      return False;
   end Check;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : External_References) is
   begin
      if Self.Refs /= null then
         Debug_Output ("Reset external references");
         Name_To_Name_HTable.Reset (Self.Refs.all);
      end if;
   end Reset;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Self          : External_References;
      External_Name : Name_Id;
      With_Default  : Name_Id := No_Name)
      return          Name_Id
   is
      Value : Name_To_Name_Ptr;
      Val   : Name_Id;
      Name  : String := Get_Name_String (External_Name);

   begin
      Canonical_Case_Env_Var_Name (Name);

      if Self.Refs /= null then
         Name_Len := Name'Length;
         Name_Buffer (1 .. Name_Len) := Name;
         Value := Name_To_Name_HTable.Get (Self.Refs.all, Name_Find);

         if Value /= null then
            Debug_Output ("Value_Of (" & Name & ") is in cache", Value.Value);
            return Value.Value;
         end if;
      end if;

      --  Find if it is an environment, if it is, put value in the hash table

      declare
         Env_Value : String_Access := Getenv (Name);

      begin
         if Env_Value /= null and then Env_Value'Length > 0 then
            Name_Len := Env_Value'Length;
            Name_Buffer (1 .. Name_Len) := Env_Value.all;
            Val := Name_Find;

            if Current_Verbosity = High then
               Debug_Output ("Value_Of (" & Name & ") is", Val);
            end if;

            if Self.Refs /= null then
               Value := new Name_To_Name'
                 (Key    => External_Name,
                  Value  => Val,
                  Source => From_Environment,
                  Next   => null);
               Name_To_Name_HTable.Set (Self.Refs.all, Value);
            end if;

            Free (Env_Value);
            return Val;

         else
            if Current_Verbosity = High then
               Debug_Output
                 ("Value_Of (" & Name & ") is default", With_Default);
            end if;

            Free (Env_Value);
            return With_Default;
         end if;
      end;
   end Value_Of;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out External_References) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Name_To_Name_HTable.Instance, Instance_Access);
   begin
      if Self.Refs /= null then
         Reset (Self);
         Unchecked_Free (Self.Refs);
      end if;
   end Free;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (E : Name_To_Name_Ptr; Next : Name_To_Name_Ptr) is
   begin
      E.Next := Next;
   end Set_Next;

   ----------
   -- Next --
   ----------

   function Next (E : Name_To_Name_Ptr) return Name_To_Name_Ptr is
   begin
      return E.Next;
   end Next;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (E : Name_To_Name_Ptr) return Name_Id is
   begin
      return E.Key;
   end Get_Key;

end Prj.Ext;
