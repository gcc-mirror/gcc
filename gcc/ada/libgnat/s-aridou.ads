------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . A R I T H _ D O U B L E                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This generic package provides software routines for doing arithmetic on
--  double word signed integer values in cases where either overflow checking
--  is required, or intermediate results are longer than the result type.

generic

   type Double_Int is range <>;

   type Double_Uns is mod <>;

   type Single_Uns is mod <>;

   with function Shift_Left (A : Double_Uns; B : Natural) return Double_Uns
     is <>;

   with function Shift_Right (A : Double_Uns; B : Natural) return Double_Uns
     is <>;

   with function Shift_Left (A : Single_Uns; B : Natural) return Single_Uns
     is <>;

package System.Arith_Double
  with Pure, SPARK_Mode
is
   ----------------
   -- Double_Int --
   ----------------

   function Add_With_Ovflo_Check (X, Y : Double_Int) return Double_Int;
   --  Raises Constraint_Error if sum of operands overflows Double_Int,
   --  otherwise returns this sum of operands as Double_Int.
   --
   --  The sum of ``X`` and ``Y`` is first computed using wrap-around
   --  semantics.
   --
   --  If the sign of ``X`` and ``Y`` are opposed, no overflow is possible and
   --  the result is correct.
   --
   --  Otherwise, ``X`` and ``Y`` have the same sign; if the sign of the result
   --  is not identical to ``X`` (or ``Y``), then an overflow occurred and
   --  the exception *Constraint_Error* is raised; otherwise the result is
   --  correct.

   function Subtract_With_Ovflo_Check (X, Y : Double_Int) return Double_Int;
   --  Raises Constraint_Error if difference of operands overflows Double_Int,
   --  otherwise returns this difference of operands as Double_Int.
   --
   --  The logic of the implementation is reversed from *Add_With_Ovflo_Check*:
   --  if ``X`` and ``Y`` have the same sign, no overflow is checked, otherwise
   --  a sign of the result is compared with the sign of ``X`` to check for
   --  overflow.

   function Multiply_With_Ovflo_Check (X, Y : Double_Int) return Double_Int
   with Convention => C;
   --  Raises Constraint_Error if product of operands overflows Double_Int,
   --  otherwise returns this product of operands as Double_Int. The code
   --  generator may also generate direct calls to this routine.
   --
   --  The multiplication is done using pencil and paper algorithm applied to
   --  Single_Uns, that is to say done on unsigned values, then the correct
   --  signed value is returned. Overflow check is performed by looking at
   --  higher digits.

   procedure Scaled_Divide
     (X, Y, Z : Double_Int;
      Q, R    : out Double_Int;
      Round   : Boolean);
   --  Performs the division of (``X`` * ``Y``) / ``Z``, storing the quotient
   --  in ``Q`` and the remainder in ``R``.
   --
   --  Constraint_Error is raised if ``Z`` is zero, or if the quotient does not
   --  fit in ``Double_Int``.
   --
   --  ``Round`` indicates if the result should be rounded. If ``Round`` is
   --  False, then ``Q``, ``R`` are the normal quotient and remainder from a
   --  truncating division. If ``Round`` is True, then ``Q`` is the rounded
   --  quotient. The remainder ``R`` is not affected by the setting of the
   --  ``Round`` flag.
   --
   --  The multiplication is done using pencil and paper algorithm applied to
   --  Single_Uns, that is to say done on unsigned values. The result is a
   --  pair of Double_Uns values.
   --
   --  The overflow is detected on the intermediate value.
   --
   --  If Z is a Single_Uns value, the division is done using pencil and paper
   --  algorithm.
   --
   --  Otherwise, the division is performed using the algorithm D from section
   --  4.3.1 of "The Art of Computer Programming Vol. 2" [TACP2]. Rounding is
   --  applied on the result.
   --
   --  Finally, the sign is applied to the result and returned.

   procedure Double_Divide
     (X, Y, Z : Double_Int;
      Q, R    : out Double_Int;
      Round   : Boolean);
   --  Performs the division ``X`` / (``Y`` * ``Z``), storing the quotient in
   --  ``Q`` and the remainder in ``R``. Constraint_Error is raised if ``Y`` or
   --  ``Z`` is zero, or if the quotient does not fit in ``Double_Int``.
   --
   --  ``Round`` indicates if the result should be rounded. If ``Round`` is
   --  False, then ``Q``, ``R`` are the normal quotient and remainder from a
   --  truncating division. If ``Round`` is True, then ``Q`` is the rounded
   --  quotient. The remainder ``R`` is not affected by the setting of the
   --  ``Round`` flag.
   --
   --  Division by 0 is first detected.
   --
   --  The intermediate value ``Y`` * ``Z`` is then computed as a pair of
   --  Double_Uns value. that is to say done on unsigned values.
   --
   --  If the high Double_Uns of the intermediate value is not 0, then 0 is
   --  returned. The overflow case of the largest negative number divided by
   --  -1 is detected here.
   --
   --  Double_Uns division is then performed, the result is rounded, its sign
   --  is corrected, and then returned.

   ----------------
   -- Double_Uns --
   ----------------

   function Add_With_Ovflo_Check (X, Y : Double_Uns) return Double_Uns;
   --  Raises Constraint_Error if sum of operands overflows Double_Uns,
   --  otherwise returns this sum of operands as Double_Uns.
   --
   --  The sum of ``X`` and ``Y`` is first computed. If the result is
   --  lower than the first operand, then an overflow occurred and the
   --  exception *Constraint_Error* is raised; otherwise the result is
   --  correct.

   function Subtract_With_Ovflo_Check (X, Y : Double_Uns) return Double_Uns;
   --  Raises Constraint_Error if difference of operands overflows Double_Uns,
   --  otherwise returns this difference of operands as Double_Int.
   --
   --  The subtraction of ``X`` and ``Y`` is first computed. If the result
   --  is greater than the first operand, then an overflow occurred and the
   --  exception *Constraint_Error* is raised; otherwise the result is
   --  correct.

   function Multiply_With_Ovflo_Check (X, Y : Double_Uns) return Double_Uns
   with Convention => C;
   --  Raises Constraint_Error if product of operands overflows Double_Uns,
   --  otherwise returns this product of operands as Double_Uns. The code
   --  generator may also generate direct calls to this routine.
   --
   --  The multiplication is done using pencil and paper algorithm applied to
   --  Single_Uns, then the correct Double_Uns value is returned. Overflow
   --  check is performed by looking at higher digits.

end System.Arith_Double;
