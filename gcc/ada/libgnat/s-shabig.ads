------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                S Y S T E M . S H A R E D _ B I G N U M S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2012-2025, Free Software Foundation, Inc.       --
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

--  This package provides declarations shared across all instantiations of
--  System.Generic_Bignums.

with Ada.Unchecked_Conversion;
with Interfaces;

package System.Shared_Bignums is
   pragma Preelaborate;

   pragma Assert (Long_Long_Integer'Size = 64);
   --  This package assumes that Long_Long_Integer size is 64 bit (i.e. that it
   --  has a range of -2**63 to 2**63-1). The front end ensures that the mode
   --  ELIMINATED is not allowed for overflow checking if this is not the case.

   subtype Length is Natural range 0 .. 2 ** 23 - 1;
   --  Represent number of words in Digit_Vector

   Base : constant := 2 ** 32;
   --  Digit vectors use this base

   subtype SD is Interfaces.Unsigned_32;
   --  Single length digit

   type Digit_Vector is array (Length range <>) of SD;
   --  Represent digits of a number (most significant digit first)

   type Bignum_Data (Len : Length) is record
      Neg : Boolean;
      --  Set if value is negative, never set for zero

      D : Digit_Vector (1 .. Len);
      --  Digits of number, most significant first, represented in base
      --  2**Base. No leading zeroes are stored, and the value of zero is
      --  represented using an empty vector for D.
   end record;

   for Bignum_Data use record
      Len at 0 range 0 .. 23;
      Neg at 3 range 0 .. 7;
   end record;

   type Bignum is access all Bignum_Data;

   function To_Bignum is new Ada.Unchecked_Conversion (System.Address, Bignum);

   function To_Address is new
     Ada.Unchecked_Conversion (Bignum, System.Address);

end System.Shared_Bignums;
