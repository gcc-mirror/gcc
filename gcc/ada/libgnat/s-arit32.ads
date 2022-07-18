------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . A R I T H _ 3 2                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020-2024, Free Software Foundation, Inc.       --
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

--  This unit provides software routines for doing arithmetic on 32-bit
--  signed integer values in cases where either overflow checking is
--  required, or intermediate results are longer than 32 bits.

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced
--  by setting the corresponding assertion policy to Ignore. Postconditions
--  and contract cases should not be executed at runtime as well, in order
--  not to slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

with Interfaces;
with Ada.Numerics.Big_Numbers.Big_Integers_Ghost;

package System.Arith_32
  with Pure, SPARK_Mode
is
   use type Ada.Numerics.Big_Numbers.Big_Integers_Ghost.Big_Integer;
   use type Interfaces.Integer_32;

   subtype Int32 is Interfaces.Integer_32;

   subtype Big_Integer is
     Ada.Numerics.Big_Numbers.Big_Integers_Ghost.Big_Integer
   with Ghost;

   package Signed_Conversion is new
     Ada.Numerics.Big_Numbers.Big_Integers_Ghost.Signed_Conversions
     (Int => Int32);

   function Big (Arg : Int32) return Big_Integer is
     (Signed_Conversion.To_Big_Integer (Arg))
   with Ghost;

   function In_Int32_Range (Arg : Big_Integer) return Boolean is
     (Ada.Numerics.Big_Numbers.Big_Integers_Ghost.In_Range
       (Arg, Big (Int32'First), Big (Int32'Last)))
   with Ghost;

   function Same_Sign (X, Y : Big_Integer) return Boolean is
     (X = Big (Int32'(0))
        or else Y = Big (Int32'(0))
        or else (X < Big (Int32'(0))) = (Y < Big (Int32'(0))))
   with Ghost;

   function Round_Quotient (X, Y, Q, R : Big_Integer) return Big_Integer is
     (if abs R > (abs Y - Big (Int32'(1))) / Big (Int32'(2)) then
       (if Same_Sign (X, Y) then Q + Big (Int32'(1))
        else Q - Big (Int32'(1)))
      else
        Q)
   with
     Ghost,
     Pre => Y /= 0 and then Q = X / Y and then R = X rem Y;

   procedure Scaled_Divide32
     (X, Y, Z : Int32;
      Q, R    : out Int32;
      Round   : Boolean)
   with
     Pre  => Z /= 0
       and then In_Int32_Range
         (if Round then Round_Quotient (Big (X) * Big (Y), Big (Z),
                                        Big (X) * Big (Y) / Big (Z),
                                        Big (X) * Big (Y) rem Big (Z))
          else Big (X) * Big (Y) / Big (Z)),
     Post => Big (R) = Big (X) * Big (Y) rem Big (Z)
       and then
         (if Round then
            Big (Q) = Round_Quotient (Big (X) * Big (Y), Big (Z),
                                      Big (X) * Big (Y) / Big (Z), Big (R))
          else
            Big (Q) = Big (X) * Big (Y) / Big (Z));
   --  Performs the division of (``X`` * ``Y``) / ``Z``, storing the quotient
   --  in ``Q`` and the remainder in ``R``.
   --
   --  Constraint_Error is raised if ``Z`` is zero, or if the quotient does not
   --  fit in 32-bits.
   --
   --  ``Round`` indicates if the result should be rounded. If ``Round`` is
   --  False, then ``Q``, ``R`` are the normal quotient and remainder from a
   --  truncating division. If ``Round`` is True, then ``Q`` is the rounded
   --  quotient. The remainder ``R`` is not affected by the setting of the
   --  ``Round`` flag.
   --
   --  The multiplication is done using pencil and paper algorithm using base
   --  2**32. The multiplication is done on unsigned values. The result is a
   --  128 bit value.
   --
   --  The overflow is detected on the intermediate value.
   --
   --  If Z is a 32 bit value, the division is done using pencil and paper
   --  algorithm.
   --
   --  Otherwise, the division is performed using the algorithm D from section
   --  4.3.1 of "The Art of Computer Programming Vol. 2" [TACP2]. Rounding is
   --  applied on the result.
   --
   --  Finally, the sign is applied to the result and returned.

end System.Arith_32;
