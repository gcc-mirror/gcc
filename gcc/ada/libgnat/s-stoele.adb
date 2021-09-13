------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

pragma Compiler_Unit_Warning;

with Ada.Unchecked_Conversion;

package body System.Storage_Elements is

   pragma Suppress (All_Checks);

   --  Conversion to/from address

   --  Note qualification below of To_Address to avoid ambiguities systems
   --  where Address is a visible integer type.

   function To_Address is
     new Ada.Unchecked_Conversion (Storage_Offset, Address);
   function To_Offset  is
     new Ada.Unchecked_Conversion (Address, Storage_Offset);

   --  Conversion to/from integers

   --  These functions must be place first because they are inlined_always
   --  and are used and inlined in other subprograms defined in this unit.

   ----------------
   -- To_Address --
   ----------------

   function To_Address (Value : Integer_Address) return Address is
   begin
      return Address (Value);
   end To_Address;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : Address) return Integer_Address is
   begin
      return Integer_Address (Value);
   end To_Integer;

   --  Address arithmetic

   ---------
   -- "+" --
   ---------

   function "+" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Storage_Elements.To_Address
        (To_Integer (Left) + To_Integer (To_Address (Right)));
   end "+";

   function "+" (Left : Storage_Offset; Right : Address) return Address is
   begin
      return Storage_Elements.To_Address
        (To_Integer (To_Address (Left)) + To_Integer (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Storage_Elements.To_Address
        (To_Integer (Left) - To_Integer (To_Address (Right)));
   end "-";

   function "-" (Left, Right : Address) return Storage_Offset is
   begin
      return To_Offset (Storage_Elements.To_Address
                         (To_Integer (Left) - To_Integer (Right)));
   end "-";

   -----------
   -- "mod" --
   -----------

   function "mod"
     (Left  : Address;
      Right : Storage_Offset) return Storage_Offset
   is
   begin
      if Right > 0 then
         return Storage_Offset
           (To_Integer (Left) mod Integer_Address (Right));

         --  The negative case makes no sense since it is a case of a mod where
         --  the left argument is unsigned and the right argument is signed. In
         --  accordance with the (spirit of the) permission of RM 13.7.1(16),
         --  we raise CE, and also include the zero case here. Yes, the RM says
         --  PE, but this really is so obviously more like a constraint error.

      else
         raise Constraint_Error;
      end if;
   end "mod";

end System.Storage_Elements;
