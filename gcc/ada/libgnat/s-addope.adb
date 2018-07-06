------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             S Y S T E M . A D D R E S S _ O P E R A T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2018, Free Software Foundation, Inc.         --
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

package body System.Address_Operations is

   type IA is mod 2 ** Address'Size;
   --  The type used to provide the actual desired operations

   function I is new Ada.Unchecked_Conversion (Address, IA);
   function A is new Ada.Unchecked_Conversion (IA, Address);
   --  The operations are implemented by unchecked conversion to type IA,
   --  followed by doing the intrinsic operation on the IA values, followed
   --  by converting the result back to type Address.

   ----------
   -- AddA --
   ----------

   function AddA (Left, Right : Address) return Address is
   begin
      return A (I (Left) + I (Right));
   end AddA;

   ----------
   -- AndA --
   ----------

   function AndA (Left, Right : Address) return Address is
   begin
      return A (I (Left) and I (Right));
   end AndA;

   ----------
   -- DivA --
   ----------

   function DivA (Left, Right : Address) return Address is
   begin
      return A (I (Left) / I (Right));
   end DivA;

   ----------
   -- ModA --
   ----------

   function ModA (Left, Right : Address) return Address is
   begin
      return A (I (Left) mod I (Right));
   end ModA;

   ---------
   -- MulA --
   ---------

   function MulA (Left, Right : Address) return Address is
   begin
      return A (I (Left) * I (Right));
   end MulA;

   ---------
   -- OrA --
   ---------

   function OrA (Left, Right : Address) return Address is
   begin
      return A (I (Left) or I (Right));
   end OrA;

   ----------
   -- SubA --
   ----------

   function SubA (Left, Right : Address) return Address is
   begin
      return A (I (Left) - I (Right));
   end SubA;

end System.Address_Operations;
