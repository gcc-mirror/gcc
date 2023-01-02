------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 I N T E R F A C E S . C . S T R I N G S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1993-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. These preconditions
--  protect from Dereference_Error and Update_Error, but not from
--  Storage_Error.

pragma Assertion_Policy (Pre => Ignore);

package Interfaces.C.Strings with
  SPARK_Mode     => On,
  Abstract_State => (C_Memory),
  Initializes    => (C_Memory)
is
   pragma Preelaborate;

   type char_array_access is access all char_array;
   for char_array_access'Size use System.Parameters.ptr_bits;

   pragma No_Strict_Aliasing (char_array_access);
   --  Since this type is used for external interfacing, with the pointer
   --  coming from who knows where, it seems a good idea to turn off any
   --  strict aliasing assumptions for this type.

   type chars_ptr is private;
   pragma Preelaborable_Initialization (chars_ptr);

   type chars_ptr_array is array (size_t range <>) of aliased chars_ptr;

   Null_Ptr : constant chars_ptr;

   function To_Chars_Ptr
     (Item      : char_array_access;
      Nul_Check : Boolean := False) return chars_ptr
   with
     SPARK_Mode => Off;

   function New_Char_Array (Chars : char_array) return chars_ptr with
     Volatile_Function,
     Post   => New_Char_Array'Result /= Null_Ptr,
     Global => (Input => C_Memory);

   function New_String (Str : String) return chars_ptr with
     Volatile_Function,
     Post   => New_String'Result /= Null_Ptr,
     Global => (Input => C_Memory);

   procedure Free (Item : in out chars_ptr) with
     SPARK_Mode => Off;
   --  When deallocation is prohibited (eg: cert runtimes) this routine
   --  will raise Program_Error

   Dereference_Error : exception;

   function Value (Item : chars_ptr) return char_array with
     Pre    => Item /= Null_Ptr,
     Global => (Input => C_Memory);

   function Value
     (Item   : chars_ptr;
      Length : size_t) return char_array
   with
     Pre    => Item /= Null_Ptr,
     Global => (Input => C_Memory);

   function Value (Item : chars_ptr) return String with
     Pre    => Item /= Null_Ptr,
     Global => (Input => C_Memory);

   function Value
     (Item   : chars_ptr;
      Length : size_t) return String
   with
     Pre    => Item /= Null_Ptr,
     Global => (Input => C_Memory);

   function Strlen (Item : chars_ptr) return size_t with
     Pre    => Item /= Null_Ptr,
     Global => (Input => C_Memory);

   procedure Update
     (Item   : chars_ptr;
      Offset : size_t;
      Chars  : char_array;
      Check  : Boolean := True)
   with
     Pre    =>
       Item /= Null_Ptr
         and then
      (if Check then
         Strlen (Item) <= size_t'Last - Offset
           and then Strlen (Item) + Offset <= Chars'Length),
     Global => (In_Out => C_Memory);

   procedure Update
     (Item   : chars_ptr;
      Offset : size_t;
      Str    : String;
      Check  : Boolean := True)
   with
     Pre    =>
       Item /= Null_Ptr
         and then
      (if Check then
         Strlen (Item) <= size_t'Last - Offset
           and then Strlen (Item) + Offset <= Str'Length),
     Global => (In_Out => C_Memory);

   Update_Error : exception;

private
   pragma SPARK_Mode (Off);
   type chars_ptr is access all Character;
   for chars_ptr'Size use System.Parameters.ptr_bits;

   pragma No_Strict_Aliasing (chars_ptr);
   --  Since this type is used for external interfacing, with the pointer
   --  coming from who knows where, it seems a good idea to turn off any
   --  strict aliasing assumptions for this type.

   Null_Ptr : constant chars_ptr := null;
end Interfaces.C.Strings;
