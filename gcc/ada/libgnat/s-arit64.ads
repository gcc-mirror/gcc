------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . A R I T H _ 6 4                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

--  This unit provides software routines for doing arithmetic on 64-bit
--  signed integer values in cases where either overflow checking is
--  required, or intermediate results are longer than 64 bits.

pragma Restrictions (No_Elaboration_Code);
--  Allow direct call from gigi generated code

with Interfaces;

package System.Arith_64 is
   pragma Pure;

   subtype Int64 is Interfaces.Integer_64;

   function Add_With_Ovflo_Check (X, Y : Int64) return Int64;
   --  Raises Constraint_Error if sum of operands overflows 64 bits,
   --  otherwise returns the 64-bit signed integer sum.

   function Subtract_With_Ovflo_Check (X, Y : Int64) return Int64;
   --  Raises Constraint_Error if difference of operands overflows 64
   --  bits, otherwise returns the 64-bit signed integer difference.

   function Multiply_With_Ovflo_Check (X, Y : Int64) return Int64;
   pragma Export (C, Multiply_With_Ovflo_Check, "__gnat_mulv64");
   --  Raises Constraint_Error if product of operands overflows 64
   --  bits, otherwise returns the 64-bit signed integer product.
   --  GIGI may also call this routine directly.

   procedure Scaled_Divide
     (X, Y, Z : Int64;
      Q, R    : out Int64;
      Round   : Boolean);
   --  Performs the division of (X * Y) / Z, storing the quotient in Q
   --  and the remainder in R. Constraint_Error is raised if Z is zero,
   --  or if the quotient does not fit in 64-bits. Round indicates if
   --  the result should be rounded. If Round is False, then Q, R are
   --  the normal quotient and remainder from a truncating division.
   --  If Round is True, then Q is the rounded quotient. The remainder
   --  R is not affected by the setting of the Round flag.

   procedure Double_Divide
     (X, Y, Z : Int64;
      Q, R    : out Int64;
      Round   : Boolean);
   --  Performs the division X / (Y * Z), storing the quotient in Q and
   --  the remainder in R. Constraint_Error is raised if Y or Z is zero,
   --  or if the quotient does not fit in 64-bits. Round indicates if the
   --  result should be rounded. If Round is False, then Q, R are the normal
   --  quotient and remainder from a truncating division. If Round is True,
   --  then Q is the rounded quotient. The remainder R is not affected by the
   --  setting of the Round flag.

end System.Arith_64;
