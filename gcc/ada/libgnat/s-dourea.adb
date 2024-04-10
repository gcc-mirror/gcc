------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . D O U B L E _ R E A L                   --
--                                                                          --
--                                 B o d y                                  --
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

package body System.Double_Real is

   function Is_NaN (N : Num) return Boolean is (N /= N);
   --  Return True if N is a NaN

   function Is_Infinity (N : Num) return Boolean is (Is_NaN (N - N));
   --  Return True if N is an infinity. Used to avoid propagating meaningless
   --  errors when the result of a product is an infinity.

   function Is_Zero (N : Num) return Boolean is (N = -N);
   --  Return True if N is a Zero. Used to preserve the sign when the result of
   --  a product is a zero.

   package Product is
      function Two_Prod (A, B : Num) return Double_T;
      function Two_Sqr (A : Num) return Double_T;
   end Product;
   --  The low-level implementation of multiplicative operations

   package body Product is separate;
   --  This is a separate body because the implementation depends on whether a
   --  Fused Multiply-Add instruction is available on the target.

   -------------------
   -- Quick_Two_Sum --
   -------------------

   function Quick_Two_Sum (A, B : Num) return Double_T is
      S : constant Num := A + B;
      V : constant Num := S - A;
      E : constant Num := B - V;

   begin
      return (S, E);
   end Quick_Two_Sum;

   -------------
   -- Two_Sum --
   -------------

   function Two_Sum (A, B : Num) return Double_T is
      S : constant Num := A + B;
      V : constant Num := S - A;
      E : constant Num := (A - (S - V)) + (B - V);

   begin
      return (S, E);
   end Two_Sum;

   --------------
   -- Two_Diff --
   --------------

   function Two_Diff (A, B : Num) return Double_T is
      S : constant Num := A - B;
      V : constant Num := S - A;
      E : constant Num := (A - (S - V)) - (B + V);

   begin
      return (S, E);
   end Two_Diff;

   --------------
   -- Two_Prod --
   --------------

   function Two_Prod (A, B : Num) return Double_T renames Product.Two_Prod;

   -------------
   -- Two_Sqr --
   -------------

   function Two_Sqr (A : Num) return Double_T renames Product.Two_Sqr;

   ---------
   -- "+" --
   ---------

   function "+" (A : Double_T; B : Num) return Double_T is
      S : constant Double_T := Two_Sum (A.Hi, B);

   begin
      return Quick_Two_Sum (S.Hi, S.Lo + A.Lo);
   end "+";

   function "+" (A, B : Double_T) return Double_T is
      S1 : constant Double_T := Two_Sum (A.Hi, B.Hi);
      S2 : constant Double_T := Two_Sum (A.Lo, B.Lo);
      S3 : constant Double_T := Quick_Two_Sum (S1.Hi, S1.Lo + S2.Hi);

   begin
      return Quick_Two_Sum (S3.Hi, S3.Lo + S2.Lo);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (A : Double_T; B : Num) return Double_T is
      D : constant Double_T := Two_Diff (A.Hi, B);

   begin
      return Quick_Two_Sum (D.Hi, D.Lo + A.Lo);
   end "-";

   function "-" (A, B : Double_T) return Double_T is
      D1 : constant Double_T := Two_Diff (A.Hi, B.Hi);
      D2 : constant Double_T := Two_Diff (A.Lo, B.Lo);
      D3 : constant Double_T := Quick_Two_Sum (D1.Hi, D1.Lo + D2.Hi);

   begin
      return Quick_Two_Sum (D3.Hi, D3.Lo + D2.Lo);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (A : Double_T; B : Num) return Double_T is
      P : constant Double_T := Two_Prod (A.Hi, B);

   begin
      if Is_Infinity (P.Hi) or else Is_Zero (P.Hi) then
         return (P.Hi, 0.0);
      else
         return Quick_Two_Sum (P.Hi, P.Lo + A.Lo * B);
      end if;
   end "*";

   function "*" (A, B : Double_T) return Double_T is
      P : constant Double_T := Two_Prod (A.Hi, B.Hi);

   begin
      if Is_Infinity (P.Hi) or else Is_Zero (P.Hi) then
         return (P.Hi, 0.0);
      else
         return Quick_Two_Sum (P.Hi, P.Lo + A.Hi * B.Lo + A.Lo * B.Hi);
      end if;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (A : Double_T; B : Num) return Double_T is
      Q1, Q2 : Num;
      P, R   : Double_T;

   begin
      if Is_Infinity (B) or else Is_Zero (B) then
         return (A.Hi / B, 0.0);
      end if;
      pragma Annotate (CodePeer, Intentional, "test always false",
                       "code deals with infinity");

      Q1 := A.Hi / B;

      --  Compute R = A - B * Q1

      P := Two_Prod (B, Q1);
      R := Two_Diff (A.Hi, P.Hi);
      R.Lo := (R.Lo + A.Lo) - P.Lo;

      Q2 := (R.Hi + R.Lo) / B;

      return Quick_Two_Sum (Q1, Q2);
   end "/";

   function "/" (A, B : Double_T) return Double_T is
      Q1, Q2, Q3 : Num;
      R, S       : Double_T;

   begin
      if Is_Infinity (B.Hi) or else Is_Zero (B.Hi) then
         return (A.Hi / B.Hi, 0.0);
      end if;
      pragma Annotate (CodePeer, Intentional, "test always false",
                       "code deals with infinity");

      Q1 := A.Hi / B.Hi;
      R := A - B * Q1;

      Q2 := R.Hi / B.Hi;
      R := R - B * Q2;

      Q3 := R.Hi / B.Hi;

      S := Quick_Two_Sum (Q1, Q2);
      return Quick_Two_Sum (S.Hi, S.Lo + Q3);
   end "/";

   ---------
   -- Sqr --
   ---------

   function Sqr (A : Double_T) return Double_T is
      Q : constant Double_T := Two_Sqr (A.Hi);

   begin
      if Is_Infinity (Q.Hi) or else Is_Zero (Q.Hi) then
         return (Q.Hi, 0.0);
      else
         return Quick_Two_Sum (Q.Hi, Q.Lo + 2.0 * A.Hi * A.Lo + A.Lo * A.Lo);
      end if;
   end Sqr;

   -------------------
   -- From_Unsigned --
   -------------------

   function From_Unsigned (U : Uns) return Double_T is
   begin
      return To_Double (Num (U));
   end From_Unsigned;

   -----------------
   -- To_Unsigned --
   -----------------

   function To_Unsigned (D : Double_T) return Uns is
      Hi : constant Num := Num'Truncation (D.Hi);

   begin
      --  If the high part is already an integer, add Floor of the low part,
      --  which means subtract Ceiling of its opposite if it is negative.

      if Hi = D.Hi then
         if D.Lo < 0.0 then
            return Uns (Hi) - Uns (Num'Ceiling (-D.Lo));
         else
            return Uns (Hi) + Uns (Num'Floor (D.Lo));
         end if;

      else
         return Uns (Hi);
      end if;
   end To_Unsigned;

end System.Double_Real;
