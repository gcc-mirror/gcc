------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . G E N E R I C _ B I G N U M S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2012-2021, Free Software Foundation, Inc.       --
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

with Interfaces;
with System.Shared_Bignums;

generic
   type Big_Integer is private;

   with function Allocate_Big_Integer
          (D : Shared_Bignums.Digit_Vector; Neg : Boolean) return Big_Integer;
   --  Allocate Bignum value with the given contents

   with procedure Free_Big_Integer (X : in out Big_Integer);
   --  Free the memory associated with X

   with function To_Bignum
          (X : aliased in out Big_Integer) return Shared_Bignums.Bignum;
   --  Convert the given Big_Integer to a Bignum

package System.Generic_Bignums is
   pragma Preelaborate;

   subtype Bignum is Shared_Bignums.Bignum;

   --  Note that this package never shares an allocated Big_Integer value, so
   --  so for example for X + 0, a copy of X is returned, not X itself.

   --  Note: none of the subprograms in this package modify the Bignum_Data
   --  records referenced by Bignum arguments of mode IN.

   function Big_Add (X, Y : Bignum) return Big_Integer;  --  "+"
   function Big_Sub (X, Y : Bignum) return Big_Integer;  --  "-"
   function Big_Mul (X, Y : Bignum) return Big_Integer;  --  "*"
   function Big_Div (X, Y : Bignum) return Big_Integer;  --  "/"
   function Big_Exp (X, Y : Bignum) return Big_Integer;  --  "**"
   function Big_Mod (X, Y : Bignum) return Big_Integer;  --  "mod"
   function Big_Rem (X, Y : Bignum) return Big_Integer;  --  "rem"
   function Big_Neg (X    : Bignum) return Big_Integer;  --  "-"
   function Big_Abs (X    : Bignum) return Big_Integer;  --  "abs"
   --  Perform indicated arithmetic operation on bignum values. No exception
   --  raised except for Div/Mod/Rem by 0 which raises Constraint_Error with
   --  an appropriate message.

   function Big_And (X, Y : Bignum) return Big_Integer;  --  "and"
   function Big_Or  (X, Y : Bignum) return Big_Integer;  --  "or"
   --  Perform indicated bitwise operation on big num values.
   --  The negative flags of X and Y are also combined.

   function Big_Shift_Left  (X : Bignum; Amount : Natural) return Big_Integer;
   function Big_Shift_Right (X : Bignum; Amount : Natural) return Big_Integer;
   --  Perform indicated bitwise operation on big num values.
   --  Constraint_Error is raised if X is negative.

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

   function To_Bignum (X : Long_Long_Integer) return Big_Integer;
   --  Convert Long_Long_Integer to a big integer. No exception can be raised
   --  for any input argument.

   function To_Bignum (X : Long_Long_Long_Integer) return Big_Integer;
   --  Convert Long_Long_Long_Integer to a big integer. No exception can be
   --  raised.

   function To_Bignum (X : Interfaces.Unsigned_64) return Big_Integer;
   --  Convert Unsigned_64 to a big integer. No exception can be raised for any
   --  input argument.

   function To_Bignum (X : Interfaces.Unsigned_128) return Big_Integer;
   --  Convert Unsigned_128 to a big integer. No exception can be raised for
   --  any input argument.

   function From_Bignum (X : Bignum) return Long_Long_Integer;
   --  Convert Bignum to Long_Long_Integer. Constraint_Error raised with
   --  appropriate message if value is out of range of Long_Long_Integer.

   function To_String
     (X : Bignum; Width : Natural := 0; Base : Positive := 10)
      return String;
   --  Return the image of X, based on the given Width and Base, as defined
   --  in the RM for Ada.Text_IO. Base should really be in the range 2 .. 16.

   function Is_Zero (X : Bignum) return Boolean;
   --  Return True if X = 0

end System.Generic_Bignums;
