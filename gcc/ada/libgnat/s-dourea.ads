------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . D O U B L E _ R E A L                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2021-2024, Free Software Foundation, Inc.       --
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

--  This package contains routines for supporting floating-point computations
--  in double precision, i.e. using a second number to estimate the error due
--  to rounding and more generally performing computations with twice as many
--  bits of mantissa. It is based on the Double-Double library available at
--  https://www.davidhbailey.com/dhbsoftware written by David H.Bailey et al.

generic

   type Num is digits <>;

package System.Double_Real is
   pragma Pure;

   type Double_T is record
     Hi, Lo : Num;
   end record;

   function To_Double (N : Num) return Double_T is ((Hi => N, Lo => 0.0));
   --  Convert a single to a double real

   function To_Single (D : Double_T) return Num is (D.Hi);
   --  Convert a double to a single real

   function Quick_Two_Sum (A, B : Num) return Double_T
     with Pre => A = 0.0 or else abs (A) >= abs (B);
   --  Compute A + B and its rounding error exactly, but assume |A| >= |B|

   function Two_Sum (A, B : Num) return Double_T;
   --  Compute A + B and its rounding error exactly

   function Two_Diff (A, B : Num) return Double_T;
   --  Compute A - B and its rounding error exactly

   function Two_Prod (A, B : Num) return Double_T;
   --  Compute A * B and its rounding error exactly

   function Two_Sqr (A : Num) return Double_T;
   --  Compute A * A and its rounding error exactly

   function "+" (A : Double_T; B : Num) return Double_T;
   function "-" (A : Double_T; B : Num) return Double_T;
   function "*" (A : Double_T; B : Num) return Double_T;
   function "/" (A : Double_T; B : Num) return Double_T
     with Pre => B /= 0.0;
   --  Mixed precision arithmetic operations

   function "+" (A, B : Double_T) return Double_T;
   function "-" (A, B : Double_T) return Double_T;
   function "*" (A, B : Double_T) return Double_T;
   function "/" (A, B : Double_T) return Double_T
     with Pre => B.Hi /= 0.0;
   --  Double precision arithmetic operations

   function Sqr (A : Double_T) return Double_T;
   --  Faster version of A * A

   function "=" (A : Double_T; B : Num) return Boolean is
     (A.Hi = B and then A.Lo = 0.0);
   function "<" (A : Double_T; B : Num) return Boolean is
     (A.Hi < B or else (A.Hi = B and then A.Lo < 0.0));
   function "<=" (A : Double_T; B : Num) return Boolean is
     (A.Hi < B or else (A.Hi = B and then A.Lo <= 0.0));
   function ">" (A : Double_T; B : Num) return Boolean is
     (A.Hi > B or else (A.Hi = B and then A.Lo > 0.0));
   function ">=" (A : Double_T; B : Num) return Boolean is
     (A.Hi > B or else (A.Hi = B and then A.Lo >= 0.0));
   --  Mixed precision comparisons

   function "=" (A, B : Double_T) return Boolean is
     (A.Hi = B.Hi and then A.Lo = B.Lo);
   function "<" (A, B : Double_T) return Boolean is
     (A.Hi < B.Hi or else (A.Hi = B.Hi and then A.Lo < B.Lo));
   function "<=" (A, B : Double_T) return Boolean is
     (A.Hi < B.Hi or else (A.Hi = B.Hi and then A.Lo <= B.Lo));
   function ">" (A, B : Double_T) return Boolean is
     (A.Hi > B.Hi or else (A.Hi = B.Hi and then A.Lo > B.Lo));
   function ">=" (A, B : Double_T) return Boolean is
     (A.Hi > B.Hi or else (A.Hi = B.Hi and then A.Lo >= B.Lo));
   --  Double precision comparisons

   generic
      type Uns is mod <>;
   function From_Unsigned (U : Uns) return Double_T;
   --  Convert Uns to Double_T

   generic
      type Uns is mod <>;
   function To_Unsigned (D : Double_T) return Uns
     with Pre => D >= 0.0;
   --  Convert Double_T to Uns with truncation

end System.Double_Real;
