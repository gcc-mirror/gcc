------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . B I G N U M S                        --
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

--  This package provides arbitrary precision signed integer arithmetic for
--  use in computing intermediate values in expressions for the case where
--  pragma Overflow_Check (Eliminated) is in effect.

--  Note that we cannot use a straight instantiation of System.Generic_Bignums
--  because the rtsfind mechanism is not ready to handle instantiations.

with System.Shared_Bignums;

package System.Bignums is
   pragma Preelaborate;

   subtype Bignum is System.Shared_Bignums.Bignum;

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

   function From_Bignum (X : Bignum) return Long_Long_Integer;
   --  Convert Bignum to Long_Long_Integer. Constraint_Error raised with
   --  appropriate message if value is out of range of Long_Long_Integer.

private

   pragma Inline (Big_Add);
   pragma Inline (Big_Sub);
   pragma Inline (Big_Mul);
   pragma Inline (Big_Div);
   pragma Inline (Big_Exp);
   pragma Inline (Big_Mod);
   pragma Inline (Big_Rem);
   pragma Inline (Big_Neg);
   pragma Inline (Big_Abs);
   pragma Inline (Big_EQ);
   pragma Inline (Big_NE);
   pragma Inline (Big_GE);
   pragma Inline (Big_LE);
   pragma Inline (Big_GT);
   pragma Inline (Big_LT);
   pragma Inline (Bignum_In_LLI_Range);
   pragma Inline (To_Bignum);
   pragma Inline (From_Bignum);

end System.Bignums;
