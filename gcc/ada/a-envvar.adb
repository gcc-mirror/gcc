------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . E N V I R O N M E N T _ V A R I A B L E S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2009-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.CRTL;
with Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;

package body Ada.Environment_Variables is

   -----------
   -- Clear --
   -----------

   procedure Clear (Name : String) is
      procedure Clear_Env_Var (Name : System.Address);
      pragma Import (C, Clear_Env_Var, "__gnat_unsetenv");

      F_Name  : String (1 .. Name'Length + 1);

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;

      Clear_Env_Var (F_Name'Address);
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear is
      procedure Clear_Env;
      pragma Import (C, Clear_Env, "__gnat_clearenv");
   begin
      Clear_Env;
   end Clear;

   ------------
   -- Exists --
   ------------

   function Exists (Name : String) return Boolean is
      use System;

      procedure Get_Env_Value_Ptr (Name, Length, Ptr : Address);
      pragma Import (C, Get_Env_Value_Ptr, "__gnat_getenv");

      Env_Value_Ptr    : aliased Address;
      Env_Value_Length : aliased Integer;
      F_Name           : aliased String (1 .. Name'Length + 1);

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;

      Get_Env_Value_Ptr
        (F_Name'Address, Env_Value_Length'Address, Env_Value_Ptr'Address);

      if Env_Value_Ptr = System.Null_Address then
         return False;
      end if;

      return True;
   end Exists;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access procedure (Name, Value : String))
   is
      use Interfaces.C.Strings;
      type C_String_Array is array (Natural) of aliased chars_ptr;
      type C_String_Array_Access is access C_String_Array;

      function Get_Env return C_String_Array_Access;
      pragma Import (C, Get_Env, "__gnat_environ");

      type String_Access is access all String;
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

      Env_Length : Natural := 0;
      Env        : constant C_String_Array_Access := Get_Env;

   begin
      --  If the environment is null return directly

      if Env = null then
         return;
      end if;

      --  First get the number of environment variables

      loop
         exit when Env (Env_Length) = Null_Ptr;
         Env_Length := Env_Length + 1;
      end loop;

      declare
         Env_Copy : array (1 .. Env_Length) of String_Access;

      begin
         --  Copy the environment

         for Iterator in 1 ..  Env_Length loop
            Env_Copy (Iterator) := new String'(Value (Env (Iterator - 1)));
         end loop;

         --  Iterate on the environment copy

         for Iterator in 1 .. Env_Length loop
            declare
               Current_Var : constant String := Env_Copy (Iterator).all;
               Value_Index : Natural := Env_Copy (Iterator)'First;

            begin
               loop
                  exit when Current_Var (Value_Index) = '=';
                  Value_Index := Value_Index + 1;
               end loop;

               Process
                 (Current_Var (Current_Var'First .. Value_Index - 1),
                  Current_Var (Value_Index + 1 .. Current_Var'Last));
            end;
         end loop;

         --  Free the copy of the environment

         for Iterator in 1 .. Env_Length loop
            Free (Env_Copy (Iterator));
         end loop;
      end;
   end Iterate;

   ---------
   -- Set --
   ---------

   procedure Set (Name : String; Value : String) is
      F_Name  : String (1 .. Name'Length + 1);
      F_Value : String (1 .. Value'Length + 1);

      procedure Set_Env_Value (Name, Value : System.Address);
      pragma Import (C, Set_Env_Value, "__gnat_setenv");

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;

      F_Value (1 .. Value'Length) := Value;
      F_Value (F_Value'Last)      := ASCII.NUL;

      Set_Env_Value (F_Name'Address, F_Value'Address);
   end Set;

   -----------
   -- Value --
   -----------

   function Value (Name : String) return String is
      use System, System.CRTL;

      procedure Get_Env_Value_Ptr (Name, Length, Ptr : Address);
      pragma Import (C, Get_Env_Value_Ptr, "__gnat_getenv");

      Env_Value_Ptr    : aliased Address;
      Env_Value_Length : aliased Integer;
      F_Name           : aliased String (1 .. Name'Length + 1);

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;

      Get_Env_Value_Ptr
        (F_Name'Address, Env_Value_Length'Address, Env_Value_Ptr'Address);

      if Env_Value_Ptr = System.Null_Address then
         raise Constraint_Error;
      end if;

      if Env_Value_Length > 0 then
         declare
            Result : aliased String (1 .. Env_Value_Length);
         begin
            strncpy (Result'Address, Env_Value_Ptr, size_t (Env_Value_Length));
            return Result;
         end;
      else
         return "";
      end if;
   end Value;

   function Value (Name : String; Default : String) return String is
   begin
      return (if Exists (Name) then Value (Name) else Default);
   end Value;

end Ada.Environment_Variables;
