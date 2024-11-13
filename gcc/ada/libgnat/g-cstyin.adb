------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               G N A T . C P P . S T D . T Y P E _ I N F O                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
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
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

package body GNAT.CPP.Std.Type_Info is

   function Name_Starts_With_Asterisk (this : access constant type_info'Class)
                                       return Boolean;

   function Name_Past_Asterisk (this : access constant type_info'Class)
                                return chars_ptr;

   function To_Address is
      new Ada.Unchecked_Conversion (chars_ptr, System.Address);

   type Char_Arr is array (Natural range <>) of aliased char;
   package CharPtr is
      new Interfaces.C.Pointers (Natural, char, Char_Arr, nul);
   type Char_Pointer is new CharPtr.Pointer;

   function To_Pointer is
      new Ada.Unchecked_Conversion (chars_ptr, Char_Pointer);
   function To_chars_ptr is
      new Ada.Unchecked_Conversion (Char_Pointer, chars_ptr);

   function Name_Starts_With_Asterisk (this : access constant type_info'Class)
                                       return Boolean is
      A : constant Address := To_Address (this.Raw_Name);
      C : aliased char;
      for C'Address use A;
   begin
      return C = '*';
   end Name_Starts_With_Asterisk;

   function Name_Past_Asterisk (this : access constant type_info'Class)
                                return chars_ptr is
      Addr : Char_Pointer := To_Pointer (this.Raw_Name);
   begin
      if this.Name_Starts_With_Asterisk then
         Increment (Addr);
      end if;

      return To_chars_ptr (Addr);
   end Name_Past_Asterisk;

   ------------
   --  Name  --
   ------------

   function Name (this : access constant type_info'Class)
                  return String
   is (Value (this.Name_Past_Asterisk));

   --------------
   --  Before  --
   --------------

   function Before (this, that : access constant type_info'Class)
                    return       Boolean is
   begin
      if this.Name_Starts_With_Asterisk
        or else that.Name_Starts_With_Asterisk
      then
         return this.Name < that.Name;
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

      return this.Name = that.Name;
   end Equals;

end GNAT.CPP.Std.Type_Info;
