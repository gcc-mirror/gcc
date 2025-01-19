------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               G N A T . C P P . S T D . T Y P E _ I N F O                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2022-2025, AdaCore                     --
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

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

package body GNAT.CPP.Std.Type_Info is

   function strcmp (L, R : chars_ptr) return Interfaces.C.int;
   pragma Import (Intrinsic, strcmp, "__builtin_strcmp");

   function Name_Starts_With_Asterisk (this : access constant type_info'Class)
                                       return Boolean;

   function Name_Past_Asterisk (this : access constant type_info'Class)
                                return chars_ptr;

   function To_Address is
      new Ada.Unchecked_Conversion (chars_ptr, System.Address);
   function To_Pointer is
      new Ada.Unchecked_Conversion (System.Address, chars_ptr);

   function Name_Starts_With_Asterisk (this : access constant type_info'Class)
                                       return Boolean is
      Addr : constant System.Address := To_Address (this.Raw_Name);
      C : aliased char;
      for C'Address use Addr;
   begin
      return C = '*';
   end Name_Starts_With_Asterisk;

   function Name_Past_Asterisk (this : access constant type_info'Class)
                                return chars_ptr is
      Addr : System.Address := To_Address (this.Raw_Name);
   begin
      if this.Name_Starts_With_Asterisk then
         Addr := Addr + Storage_Offset (1);
      end if;

      return To_Pointer (Addr);
   end Name_Past_Asterisk;

   ------------
   --  Name  --
   ------------

   function Name (this : access constant type_info'Class)
                  return chars_ptr
   is (this.Name_Past_Asterisk);

   --------------
   --  Before  --
   --------------

   function Before (this, that : access constant type_info'Class)
                    return       Boolean is
   begin
      if not this.Name_Starts_With_Asterisk
        or else not that.Name_Starts_With_Asterisk
      then
         return strcmp (this.Raw_Name, that.Raw_Name) < 0;
      end if;

      return To_Address (this.Raw_Name) < To_Address (that.Raw_Name);
   end Before;

   --------------
   --  Equals  --
   --------------

   function Equals (this, that : access constant type_info'Class)
                    return       Boolean is
   begin
      if this = that then
         return True;
      end if;

      if this.Name_Starts_With_Asterisk then
         return False;
      end if;

      return strcmp (this.Raw_Name, that.Raw_Name) = 0;
   end Equals;

   function Convert_Caught_Object (Choice, Except : access type_info'Class;
                                   Thrown         : in out Address;
                                   Lang           : Character)
                                   return           Interfaces.C.C_bool;
   pragma Export (Cpp, Convert_Caught_Object, "__gnat_convert_caught_object");
   --  Convert the exception object at Thrown, under Lang convention,
   --  from type Except to type Choice, adjusting Thrown as needed and
   --  returning True, or returning False in case the conversion
   --  fails.  This is called from raise-gcc, and it is placed here
   --  rather than in GNAT.CPP_Exceptions to avoid dragging all that
   --  in when the program doesn't use C++ exceptions.

   ---------------------------
   -- Convert_Caught_Object --
   ---------------------------

   function Convert_Caught_Object (Choice, Except : access type_info'Class;
                                   Thrown         : in out Address;
                                   Lang           : Character)
                                   return           Interfaces.C.C_bool is
   begin
      if Choice.Equals (Except) then
         return C_bool'(True);
      end if;

      if Lang = 'B' then
         if Except.Is_Pointer_P then
            declare
               Thrown_Indirect : Address;
               for Thrown_Indirect'Address use Thrown;
            begin
               Thrown := Thrown_Indirect;
            end;
         end if;

         if Choice.Do_Catch (Except, Thrown, 1) then
            return C_bool'(True);
         end if;
      end if;

      return C_bool'(False);
   end Convert_Caught_Object;

end GNAT.CPP.Std.Type_Info;
