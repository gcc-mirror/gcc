------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . G E N E R I C _ B I G N U M S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2012-2019, Free Software Foundation, Inc.       --
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

--  This package provides arbitrary precision signed integer arithmetic
--  and can be used either built into the compiler via System.Bignums or to
--  implement a default version of Ada.Numerics.Big_Numbers.Big_Integers.

--  If Use_Secondary_Stack is True then all Bignum values are allocated on the
--  secondary stack. If False, the heap is used and the caller is responsible
--  for memory management.

with Ada.Unchecked_Conversion;
with Interfaces;

generic
   Use_Secondary_Stack : Boolean;
package System.Generic_Bignums is
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
   --  This is the type that is used externally. Possibly this could be a
   --  private type, but we leave the structure exposed for now. For one
   --  thing it helps with debugging. Note that this package never shares
   --  an allocated Bignum value, so for example for X + 0, a copy of X is
   --  returned, not X itself.

   function To_Bignum is new Ada.Unchecked_Conversion (System.Address, Bignum);
   function To_Address is new
     Ada.Unchecked_Conversion (Bignum, System.Address);

   --  Note: none of the subprograms in this package modify the Bignum_Data
   --  records referenced by Bignum arguments of mode IN.

   function Big_Add (X, Y : Bignum) return Bignum;  --  "+"
   function Big_Sub (X, Y : Bignum) return Bignum;  --  "-"
   function Big_Mul (X, Y : Bignum) return Bignum;  --  "*"
   function Big_Div (X, Y : Bignum) return Bignum;  --  "/"
   function Big_Exp (X, Y : Bignum) return Bignum;  --  "**"
   function Big_Mod (X, Y : Bignum) return Bignum;  --  "mod"
   function Big_Rem (X, Y : Bignum) return Bignum;  --  "rem"
   function Big_Neg (X    : Bignum) return Bignum;  --  "-"
   function Big_Abs (X    : Bignum) return Bignum;  --  "abs"
   --  Perform indicated arithmetic operation on bignum values. No exception
   --  raised except for Div/Mod/Rem by 0 which raises Constraint_Error with
   --  an appropriate message.

   function Big_EQ  (X, Y : Bignum) return Boolean;  -- "="
   function Big_NE  (X, Y : Bignum) return Boolean;  -- "/="
   function Big_GE  (X, Y : Bignum) return Boolean;  -- ">="
   function Big_LE  (X, Y : Bignum) return Boolean;  -- "<="
   function Big_GT  (X, Y : Bignum) return Boolean;  --  ">"
   function Big_LT  (X, Y : Bignum) return Boolean;  --  "<"
   --  Perform indicated comparison on bignums, returning result as Boolean.
   --  No exception raised for any input arguments.

   function Bignum_In_LLI_Range (X : Bignum) return Boolean;
   --  Returns True if the Bignum value is in the range of Long_Long_Integer,
   --  so that a call to From_Bignum is guaranteed not to raise an exception.

   function To_Bignum (X : Long_Long_Integer) return Bignum;
   --  Convert Long_Long_Integer to Bignum. No exception can be raised for any
   --  input argument.

   function To_Bignum (X : Interfaces.Unsigned_64) return Bignum;
   --  Convert Unsigned_64 to Bignum. No exception can be raised for any
   --  input argument.

   function From_Bignum (X : Bignum) return Long_Long_Integer;
   --  Convert Bignum to Long_Long_Integer. Constraint_Error raised with
   --  appropriate message if value is out of range of Long_Long_Integer.

   function Is_Zero (X : Bignum) return Boolean;
   --  Return True if X = 0

end System.Generic_Bignums;
