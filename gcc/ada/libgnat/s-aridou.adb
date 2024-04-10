------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . A R I T H _ D O U B L E                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

pragma Annotate (Gnatcheck, Exempt_On, "Metrics_LSLOC",
                 "limit exceeded due to proof code");

with Ada.Unchecked_Conversion;
with System.SPARK.Cut_Operations; use System.SPARK.Cut_Operations;

package body System.Arith_Double
  with SPARK_Mode
is
   --  Contracts, ghost code, loop invariants and assertions in this unit are
   --  meant for analysis only, not for run-time checking, as it would be too
   --  costly otherwise. This is enforced by setting the assertion policy to
   --  Ignore.

   pragma Assertion_Policy (Pre            => Ignore,
                            Post           => Ignore,
                            Contract_Cases => Ignore,
                            Ghost          => Ignore,
                            Loop_Invariant => Ignore,
                            Assert         => Ignore,
                            Assert_And_Cut => Ignore);

   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);

   function To_Uns is new Ada.Unchecked_Conversion (Double_Int, Double_Uns);
   function To_Int is new Ada.Unchecked_Conversion (Double_Uns, Double_Int);

   Double_Size : constant Natural := Double_Int'Size;
   Single_Size : constant Natural := Double_Int'Size / 2;

   --  Log of Single_Size in base 2, so that Single_Size = 2 ** Log_Single_Size
   Log_Single_Size : constant Natural :=
     (case Single_Size is
        when 32  => 5,
        when 64  => 6,
        when 128 => 7,
        when others => raise Program_Error)
   with Ghost;

   --  Power-of-two constants

   pragma Warnings
     (Off, "non-preelaborable call not allowed in preelaborated unit",
      Reason => "Ghost code is not compiled");
   pragma Warnings
     (Off, "non-static constant in preelaborated unit",
      Reason => "Ghost code is not compiled");
   Big_0 : constant Big_Integer :=
     Big (Double_Uns'(0))
   with Ghost;
   Big_2xxSingle : constant Big_Integer :=
     Big (Double_Int'(2 ** Single_Size))
   with Ghost;
   Big_2xxDouble_Minus_1 : constant Big_Integer :=
     Big (Double_Uns'(2 ** (Double_Size - 1)))
   with Ghost;
   Big_2xxDouble : constant Big_Integer :=
     Big (Double_Uns'(2 ** Double_Size - 1)) + 1
   with Ghost;
   pragma Warnings
     (On, "non-preelaborable call not allowed in preelaborated unit");
   pragma Warnings (On, "non-static constant in preelaborated unit");

   pragma Annotate (Gnatcheck, Exempt_On, "Improper_Returns",
                    "early returns for performance");

   -----------------------
   -- Local Subprograms --
   -----------------------

   function "+" (A, B : Single_Uns) return Double_Uns is
     (Double_Uns (A) + Double_Uns (B));
   function "+" (A : Double_Uns; B : Single_Uns) return Double_Uns is
     (A + Double_Uns (B));
   --  Length doubling additions

   function "*" (A, B : Single_Uns) return Double_Uns is
     (Double_Uns (A) * Double_Uns (B));
   --  Length doubling multiplication

   function "/" (A : Double_Uns; B : Single_Uns) return Double_Uns is
     (A / Double_Uns (B))
   with
     Pre => B /= 0;
   --  Length doubling division

   function "&" (Hi, Lo : Single_Uns) return Double_Uns is
     (Shift_Left (Double_Uns (Hi), Single_Size) or Double_Uns (Lo));
   --  Concatenate hi, lo values to form double result

   function "abs" (X : Double_Int) return Double_Uns is
     (if X = Double_Int'First
      then Double_Uns'(2 ** (Double_Size - 1))
      else Double_Uns (Double_Int'(abs X)));
   --  Convert absolute value of X to unsigned. Note that we can't just use
   --  the expression of the Else since it overflows for X = Double_Int'First.

   function "rem" (A : Double_Uns; B : Single_Uns) return Double_Uns is
     (A rem Double_Uns (B))
   with
     Pre => B /= 0;
   --  Length doubling remainder

   function Big_2xx (N : Natural) return Big_Positive is
     (Big (Double_Uns'(2 ** N)))
   with
     Ghost,
     Pre  => N < Double_Size,
     Post => Big_2xx'Result > 0;
   --  2**N as a big integer

   function Big3 (X1, X2, X3 : Single_Uns) return Big_Natural is
     (Big_2xxSingle * Big_2xxSingle * Big (Double_Uns (X1))
                    + Big_2xxSingle * Big (Double_Uns (X2))
                                    + Big (Double_Uns (X3)))
   with
     Ghost,
     Annotate => (GNATprove, Inline_For_Proof);
   --  X1&X2&X3 as a big integer

   function Le3 (X1, X2, X3, Y1, Y2, Y3 : Single_Uns) return Boolean
   with
     Post => Le3'Result = (Big3 (X1, X2, X3) <= Big3 (Y1, Y2, Y3));
   --  Determines if (3 * Single_Size)-bit value X1&X2&X3 <= Y1&Y2&Y3

   function Lo (A : Double_Uns) return Single_Uns is
     (Single_Uns (A and (2 ** Single_Size - 1)));
   --  Low order half of double value

   function Hi (A : Double_Uns) return Single_Uns is
     (Single_Uns (Shift_Right (A, Single_Size)));
   --  High order half of double value

   procedure Sub3 (X1, X2, X3 : in out Single_Uns; Y1, Y2, Y3 : Single_Uns)
   with
     Pre  => Big3 (X1, X2, X3) >= Big3 (Y1, Y2, Y3),
     Post => Big3 (X1, X2, X3) = Big3 (X1, X2, X3)'Old - Big3 (Y1, Y2, Y3);
   --  Computes X1&X2&X3 := X1&X2&X3 - Y1&Y1&Y3 mod 2 ** (3 * Single_Size)

   function To_Neg_Int (A : Double_Uns) return Double_Int
   with
     Pre  => In_Double_Int_Range (-Big (A)),
     Post => Big (To_Neg_Int'Result) = -Big (A);
   --  Convert to negative integer equivalent. If the input is in the range
   --  0 .. 2 ** (Double_Size - 1), then the corresponding nonpositive signed
   --  integer (obtained by negating the given value) is returned, otherwise
   --  constraint error is raised.

   function To_Pos_Int (A : Double_Uns) return Double_Int
   with
     Pre  => In_Double_Int_Range (Big (A)),
     Post => Big (To_Pos_Int'Result) = Big (A);
   --  Convert to positive integer equivalent. If the input is in the range
   --  0 .. 2 ** (Double_Size - 1) - 1, then the corresponding non-negative
   --  signed integer is returned, otherwise constraint error is raised.

   procedure Raise_Error;
   pragma No_Return (Raise_Error);
   --  Raise constraint error with appropriate message

   ------------------
   -- Local Lemmas --
   ------------------

   procedure Inline_Le3 (X1, X2, X3, Y1, Y2, Y3 : Single_Uns)
   with
     Ghost,
     Pre  => Le3 (X1, X2, X3, Y1, Y2, Y3),
     Post => Big3 (X1, X2, X3) <= Big3 (Y1, Y2, Y3);

   procedure Lemma_Abs_Commutation (X : Double_Int)
   with
     Ghost,
     Post => abs (Big (X)) = Big (Double_Uns'(abs X));

   procedure Lemma_Abs_Div_Commutation (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => abs (X / Y) = abs X / abs Y;

   procedure Lemma_Abs_Mult_Commutation (X, Y : Big_Integer)
   with
     Ghost,
     Post => abs (X * Y) = abs X * abs Y;

   procedure Lemma_Abs_Range (X : Big_Integer)
   with
     Ghost,
     Pre  => In_Double_Int_Range (X),
     Post => abs (X) <= Big_2xxDouble_Minus_1
       and then In_Double_Int_Range (-abs (X));

   procedure Lemma_Abs_Rem_Commutation (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => abs (X rem Y) = (abs X) rem (abs Y);

   procedure Lemma_Add_Commutation (X : Double_Uns; Y : Single_Uns)
   with
     Ghost,
     Pre  => X <= 2 ** Double_Size - 2 ** Single_Size,
     Post => Big (X) + Big (Double_Uns (Y)) = Big (X + Double_Uns (Y));

   procedure Lemma_Add_One (X : Double_Uns)
   with
     Ghost,
     Pre  => X /= Double_Uns'Last,
     Post => Big (X + Double_Uns'(1)) = Big (X) + 1;

   procedure Lemma_Big_Of_Double_Uns (X : Double_Uns)
   with
     Ghost,
     Post => Big (X) < Big_2xxDouble;

   procedure Lemma_Big_Of_Double_Uns_Of_Single_Uns (X : Single_Uns)
   with
     Ghost,
     Post => Big (Double_Uns (X)) >= 0
       and then Big (Double_Uns (X)) < Big_2xxSingle;

   procedure Lemma_Bounded_Powers_Of_2_Increasing (M, N : Natural)
   with
     Ghost,
     Pre  => M < N and then N < Double_Size,
     Post => Double_Uns'(2)**M < Double_Uns'(2)**N;

   procedure Lemma_Concat_Definition (X, Y : Single_Uns)
   with
     Ghost,
     Post => Big (X & Y) = Big_2xxSingle * Big (Double_Uns (X))
                                         + Big (Double_Uns (Y));

   procedure Lemma_Deep_Mult_Commutation
     (Factor : Big_Integer;
      X, Y   : Single_Uns)
   with
     Ghost,
     Post =>
       Factor * Big (Double_Uns (X)) * Big (Double_Uns (Y)) =
         Factor * Big (Double_Uns (X) * Double_Uns (Y));

   procedure Lemma_Div_Commutation (X, Y : Double_Uns)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => Big (X) / Big (Y) = Big (X / Y);

   procedure Lemma_Div_Definition
     (A : Double_Uns;
      B : Single_Uns;
      Q : Double_Uns;
      R : Double_Uns)
   with
     Ghost,
     Pre  => B /= 0 and then Q = A / B and then R = A rem B,
     Post => Big (A) = Big (Double_Uns (B)) * Big (Q) + Big (R);

   procedure Lemma_Div_Ge (X, Y, Z : Big_Integer)
   with
     Ghost,
     Pre  => Z > 0 and then X >= Y * Z,
     Post => X / Z >= Y;

   procedure Lemma_Div_Lt (X, Y, Z : Big_Natural)
   with
     Ghost,
     Pre  => Z > 0 and then X < Y * Z,
     Post => X / Z < Y;

   procedure Lemma_Div_Eq (A, B, S, R : Big_Integer)
   with
     Ghost,
     Pre  => A * S = B * S + R and then S /= 0,
     Post => A = B + R / S;

   procedure Lemma_Div_Mult (X : Big_Natural; Y : Big_Positive)
   with
     Ghost,
     Post => X / Y * Y > X - Y;

   procedure Lemma_Double_Big_2xxSingle
   with
     Ghost,
     Post => Big_2xxSingle * Big_2xxSingle = Big_2xxDouble;

   procedure Lemma_Double_Shift (X : Double_Uns; S, S1 : Double_Uns)
   with
     Ghost,
     Pre  => S <= Double_Uns (Double_Size)
       and then S1 <= Double_Uns (Double_Size),
     Post => Shift_Left (Shift_Left (X, Natural (S)), Natural (S1)) =
             Shift_Left (X, Natural (S + S1));

   procedure Lemma_Double_Shift (X : Single_Uns; S, S1 : Natural)
   with
     Ghost,
     Pre  => S <= Single_Size - S1,
     Post => Shift_Left (Shift_Left (X, S), S1) = Shift_Left (X, S + S1);

   procedure Lemma_Double_Shift (X : Double_Uns; S, S1 : Natural)
   with
     Ghost,
     Pre  => S <= Double_Size - S1,
     Post => Shift_Left (Shift_Left (X, S), S1) = Shift_Left (X, S + S1);

   procedure Lemma_Double_Shift_Left (X : Double_Uns; S, S1 : Double_Uns)
   with
     Ghost,
     Pre  => S <= Double_Uns (Double_Size)
       and then S1 <= Double_Uns (Double_Size),
     Post => Shift_Left (Shift_Left (X, Natural (S)), Natural (S1)) =
             Shift_Left (X, Natural (S + S1));

   procedure Lemma_Double_Shift_Left (X : Double_Uns; S, S1 : Natural)
   with
     Ghost,
     Pre  => S <= Double_Size - S1,
     Post => Shift_Left (Shift_Left (X, S), S1) = Shift_Left (X, S + S1);

   procedure Lemma_Double_Shift_Right (X : Double_Uns; S, S1 : Double_Uns)
   with
     Ghost,
     Pre  => S <= Double_Uns (Double_Size)
       and then S1 <= Double_Uns (Double_Size),
     Post => Shift_Right (Shift_Right (X, Natural (S)), Natural (S1)) =
             Shift_Right (X, Natural (S + S1));

   procedure Lemma_Double_Shift_Right (X : Double_Uns; S, S1 : Natural)
   with
     Ghost,
     Pre  => S <= Double_Size - S1,
     Post => Shift_Right (Shift_Right (X, S), S1) = Shift_Right (X, S + S1);

   procedure Lemma_Ge_Commutation (A, B : Double_Uns)
   with
     Ghost,
     Pre  => A >= B,
     Post => Big (A) >= Big (B);

   procedure Lemma_Ge_Mult (A, B, C, D : Big_Integer)
   with
     Ghost,
     Pre  => A >= B and then B * C >= D and then C > 0,
     Post => A * C >= D;

   procedure Lemma_Gt_Commutation (A, B : Double_Uns)
   with
     Ghost,
     Pre  => A > B,
     Post => Big (A) > Big (B);

   procedure Lemma_Gt_Mult (A, B, C, D : Big_Integer)
   with
     Ghost,
     Pre  => A >= B and then B * C > D and then C > 0,
     Post => A * C > D;

   procedure Lemma_Hi_Lo (Xu : Double_Uns; Xhi, Xlo : Single_Uns)
   with
     Ghost,
     Pre  => Xhi = Hi (Xu) and Xlo = Lo (Xu),
     Post => Big (Xu) =
       Big_2xxSingle * Big (Double_Uns (Xhi)) + Big (Double_Uns (Xlo));

   procedure Lemma_Hi_Lo_3 (Xu : Double_Uns; Xhi, Xlo : Single_Uns)
   with
     Ghost,
     Pre  => Xhi = Hi (Xu) and then Xlo = Lo (Xu),
     Post => Big (Xu) = Big3 (0, Xhi, Xlo);

   procedure Lemma_Lo_Is_Ident (X : Double_Uns)
   with
     Ghost,
     Pre  => Big (X) < Big_2xxSingle,
     Post => Double_Uns (Lo (X)) = X;

   procedure Lemma_Lt_Commutation (A, B : Double_Uns)
   with
     Ghost,
     Pre  => A < B,
     Post => Big (A) < Big (B);

   procedure Lemma_Lt_Mult (A, B, C, D : Big_Integer)
   with
     Ghost,
     Pre  => A < B and then B * C <= D and then C > 0,
     Post => A * C < D;

   procedure Lemma_Mult_Commutation (X, Y : Single_Uns)
   with
     Ghost,
     Post =>
       Big (Double_Uns (X)) * Big (Double_Uns (Y)) =
         Big (Double_Uns (X) * Double_Uns (Y));

   procedure Lemma_Mult_Commutation (X, Y : Double_Int)
   with
     Ghost,
     Pre  => In_Double_Int_Range (Big (X) * Big (Y)),
     Post => Big (X) * Big (Y) = Big (X * Y);

   procedure Lemma_Mult_Commutation (X, Y, Z : Double_Uns)
   with
     Ghost,
     Pre  => Big (X) * Big (Y) < Big_2xxDouble and then Z = X * Y,
     Post => Big (X) * Big (Y) = Big (Z);

   procedure Lemma_Mult_Decomposition
     (Mult               : Big_Integer;
      Xu, Yu             : Double_Uns;
      Xhi, Xlo, Yhi, Ylo : Single_Uns)
   with
     Ghost,
     Pre  => Mult = Big (Xu) * Big (Yu)
       and then Xhi = Hi (Xu)
       and then Xlo = Lo (Xu)
       and then Yhi = Hi (Yu)
       and then Ylo = Lo (Yu),
     Post => Mult =
       Big_2xxSingle * Big_2xxSingle * (Big (Double_Uns'(Xhi * Yhi)))
                     + Big_2xxSingle * (Big (Double_Uns'(Xhi * Ylo)))
                     + Big_2xxSingle * (Big (Double_Uns'(Xlo * Yhi)))
                                     + (Big (Double_Uns'(Xlo * Ylo)));

   procedure Lemma_Mult_Distribution (X, Y, Z : Big_Integer)
   with
     Ghost,
     Post => X * (Y + Z) = X * Y + X * Z;

   procedure Lemma_Mult_Div (A, B : Big_Integer)
   with
     Ghost,
     Pre  => B /= 0,
     Post => A * B / B = A;

   procedure Lemma_Mult_Non_Negative (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => (X >= 0 and then Y >= 0)
       or else (X <= 0 and then Y <= 0),
     Post => X * Y >= 0;

   procedure Lemma_Mult_Non_Positive (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => (X <= Big_0 and then Y >= Big_0)
       or else (X >= Big_0 and then Y <= Big_0),
     Post => X * Y <= Big_0;

   procedure Lemma_Mult_Positive (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => (X > Big_0 and then Y > Big_0)
       or else (X < Big_0 and then Y < Big_0),
     Post => X * Y > Big_0;

   procedure Lemma_Neg_Div (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => X / Y = (-X) / (-Y);

   procedure Lemma_Neg_Rem (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => X rem Y = X rem (-Y);

   procedure Lemma_Not_In_Range_Big2xx64
   with
     Post => not In_Double_Int_Range (Big_2xxDouble)
       and then not In_Double_Int_Range (-Big_2xxDouble);

   procedure Lemma_Powers (A : Big_Natural; B, C : Natural)
   with
     Ghost,
     Pre  => B <= Natural'Last - C,
     Post => A**B * A**C = A**(B + C);

   procedure Lemma_Powers_Of_2 (M, N : Natural)
   with
     Ghost,
     Pre  => M < Double_Size
       and then N < Double_Size
       and then M + N <= Double_Size,
     Post =>
       Big_2xx (M) * Big_2xx (N) =
         (if M + N = Double_Size then Big_2xxDouble else Big_2xx (M + N));

   procedure Lemma_Powers_Of_2_Commutation (M : Natural)
   with
     Ghost,
     Subprogram_Variant => (Decreases => M),
     Pre  => M <= Double_Size,
     Post => Big (Double_Uns'(2))**M =
              (if M < Double_Size then Big_2xx (M) else Big_2xxDouble);

   procedure Lemma_Powers_Of_2_Increasing (M, N : Natural)
   with
     Ghost,
     Subprogram_Variant => (Increases => M),
     Pre  => M < N,
     Post => Big (Double_Uns'(2))**M < Big (Double_Uns'(2))**N;

   procedure Lemma_Rem_Abs (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => X rem Y = X rem (abs Y);
   pragma Unreferenced (Lemma_Rem_Abs);

   procedure Lemma_Rem_Commutation (X, Y : Double_Uns)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => Big (X) rem Big (Y) = Big (X rem Y);

   procedure Lemma_Rem_Is_Ident (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => abs X < abs Y,
     Post => X rem Y = X;
   pragma Unreferenced (Lemma_Rem_Is_Ident);

   procedure Lemma_Rem_Sign (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => Same_Sign (X rem Y, X);
   pragma Unreferenced (Lemma_Rem_Sign);

   procedure Lemma_Rev_Div_Definition (A, B, Q, R : Big_Natural)
   with
     Ghost,
     Pre  => A = B * Q + R and then R < B,
     Post => Q = A / B and then R = A rem B;

   procedure Lemma_Shift_Left (X : Double_Uns; Shift : Natural)
   with
     Ghost,
     Pre  => Shift < Double_Size
       and then Big (X) * Big_2xx (Shift) < Big_2xxDouble,
     Post => Big (Shift_Left (X, Shift)) = Big (X) * Big_2xx (Shift);

   procedure Lemma_Shift_Right (X : Double_Uns; Shift : Natural)
   with
     Ghost,
     Pre  => Shift < Double_Size,
     Post => Big (Shift_Right (X, Shift)) = Big (X) / Big_2xx (Shift);

   procedure Lemma_Shift_Without_Drop
     (X, Y  : Double_Uns;
      Mask  : Single_Uns;
      Shift : Natural)
   with
     Ghost,
     Pre  => (Hi (X) and Mask) = 0  --  X has the first Shift bits off
       and then Shift <= Single_Size
       and then Mask = Shift_Left (Single_Uns'Last, Single_Size - Shift)
       and then Y = Shift_Left (X, Shift),
     Post => Big (Y) = Big_2xx (Shift) * Big (X);

   procedure Lemma_Simplify (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => X * Y / Y = X;

   procedure Lemma_Substitution (A, B, C, C1, D : Big_Integer)
   with
     Ghost,
     Pre  => C = C1 and then A = B * C + D,
     Post => A = B * C1 + D;

   procedure Lemma_Subtract_Commutation (X, Y : Double_Uns)
   with
     Ghost,
     Pre  => X >= Y,
     Post => Big (X) - Big (Y) = Big (X - Y);

   procedure Lemma_Subtract_Double_Uns (X, Y : Double_Int)
   with
     Ghost,
     Pre  => X >= 0 and then X <= Y,
     Post => Double_Uns (Y - X) = Double_Uns (Y) - Double_Uns (X);

   procedure Lemma_Word_Commutation (X : Single_Uns)
   with
     Ghost,
     Post => Big_2xxSingle * Big (Double_Uns (X))
       = Big (2**Single_Size * Double_Uns (X));

   -----------------------------
   -- Local lemma null bodies --
   -----------------------------

   procedure Inline_Le3 (X1, X2, X3, Y1, Y2, Y3 : Single_Uns) is null;
   procedure Lemma_Abs_Commutation (X : Double_Int) is null;
   procedure Lemma_Abs_Mult_Commutation (X, Y : Big_Integer) is null;
   procedure Lemma_Abs_Range (X : Big_Integer) is null;
   procedure Lemma_Add_Commutation (X : Double_Uns; Y : Single_Uns) is null;
   procedure Lemma_Add_One (X : Double_Uns) is null;
   procedure Lemma_Big_Of_Double_Uns (X : Double_Uns) is null;
   procedure Lemma_Big_Of_Double_Uns_Of_Single_Uns (X : Single_Uns) is null;
   procedure Lemma_Bounded_Powers_Of_2_Increasing (M, N : Natural) is null;
   procedure Lemma_Deep_Mult_Commutation
     (Factor : Big_Integer;
      X, Y   : Single_Uns)
   is null;
   procedure Lemma_Div_Commutation (X, Y : Double_Uns) is null;
   procedure Lemma_Div_Definition
     (A : Double_Uns;
      B : Single_Uns;
      Q : Double_Uns;
      R : Double_Uns)
   is null;
   procedure Lemma_Div_Ge (X, Y, Z : Big_Integer) is null;
   procedure Lemma_Div_Lt (X, Y, Z : Big_Natural) is null;
   procedure Lemma_Div_Mult (X : Big_Natural; Y : Big_Positive) is null;
   procedure Lemma_Double_Big_2xxSingle is null;
   procedure Lemma_Double_Shift (X : Double_Uns; S, S1 : Double_Uns) is null;
   procedure Lemma_Double_Shift (X : Single_Uns; S, S1 : Natural) is null;
   procedure Lemma_Double_Shift_Left (X : Double_Uns; S, S1 : Double_Uns)
   is null;
   procedure Lemma_Double_Shift_Right (X : Double_Uns; S, S1 : Double_Uns)
   is null;
   procedure Lemma_Ge_Commutation (A, B : Double_Uns) is null;
   procedure Lemma_Ge_Mult (A, B, C, D : Big_Integer) is null;
   procedure Lemma_Gt_Commutation (A, B : Double_Uns) is null;
   procedure Lemma_Gt_Mult (A, B, C, D : Big_Integer) is null;
   procedure Lemma_Lo_Is_Ident (X : Double_Uns) is null;
   procedure Lemma_Lt_Commutation (A, B : Double_Uns) is null;
   procedure Lemma_Lt_Mult (A, B, C, D : Big_Integer) is null;
   procedure Lemma_Mult_Commutation (X, Y : Single_Uns) is null;
   procedure Lemma_Mult_Commutation (X, Y : Double_Int) is null;
   procedure Lemma_Mult_Commutation (X, Y, Z : Double_Uns) is null;
   procedure Lemma_Mult_Distribution (X, Y, Z : Big_Integer) is null;
   procedure Lemma_Mult_Non_Negative (X, Y : Big_Integer) is null;
   procedure Lemma_Mult_Non_Positive (X, Y : Big_Integer) is null;
   procedure Lemma_Mult_Positive (X, Y : Big_Integer) is null;
   procedure Lemma_Neg_Rem (X, Y : Big_Integer) is null;
   procedure Lemma_Not_In_Range_Big2xx64 is null;
   procedure Lemma_Powers (A : Big_Natural; B, C : Natural) is null;
   procedure Lemma_Rem_Commutation (X, Y : Double_Uns) is null;
   procedure Lemma_Rem_Is_Ident (X, Y : Big_Integer) is null;
   procedure Lemma_Rem_Sign (X, Y : Big_Integer) is null;
   procedure Lemma_Rev_Div_Definition (A, B, Q, R : Big_Natural) is null;
   procedure Lemma_Simplify (X, Y : Big_Integer) is null;
   procedure Lemma_Substitution (A, B, C, C1, D : Big_Integer) is null;
   procedure Lemma_Subtract_Commutation (X, Y : Double_Uns) is null;
   procedure Lemma_Subtract_Double_Uns (X, Y : Double_Int) is null;
   procedure Lemma_Word_Commutation (X : Single_Uns) is null;

   --------------------------
   -- Add_With_Ovflo_Check --
   --------------------------

   function Add_With_Ovflo_Check (X, Y : Double_Int) return Double_Int is
      R : constant Double_Int := To_Int (To_Uns (X) + To_Uns (Y));

      --  Local lemmas

      procedure Prove_Negative_X
      with
        Ghost,
        Pre  => X < 0 and then (Y > 0 or else R < 0),
        Post => R = X + Y;

      procedure Prove_Non_Negative_X
      with
        Ghost,
        Pre  => X >= 0 and then (Y < 0 or else R >= 0),
        Post => R = X + Y;

      procedure Prove_Overflow_Case
      with
        Ghost,
        Pre  =>
          (if X >= 0 then Y >= 0 and then R < 0
                     else Y <= 0 and then R >= 0),
        Post => not In_Double_Int_Range (Big (X) + Big (Y));

      ----------------------
      -- Prove_Negative_X --
      ----------------------

      procedure Prove_Negative_X is
      begin
         if X = Double_Int'First then
            if Y > 0 then
               null;
            else
               pragma Assert
                 (To_Uns (X) + To_Uns (Y) =
                    2 ** (Double_Size - 1) - Double_Uns (-Y));
               pragma Assert  --  as R < 0
                 (To_Uns (X) + To_Uns (Y) >= 2 ** (Double_Size - 1));
               pragma Assert (Y = 0);
            end if;

         elsif Y = Double_Int'First then
            pragma Assert
              (To_Uns (X) + To_Uns (Y) =
                 2 ** (Double_Size - 1) - Double_Uns (-X));
            pragma Assert (False);

         elsif Y <= 0 then
            pragma Assert
              (To_Uns (X) + To_Uns (Y) = -Double_Uns (-X) - Double_Uns (-Y));

         else  --  Y > 0, 0 > X > Double_Int'First
            declare
               Ru : constant Double_Uns := To_Uns (X) + To_Uns (Y);
            begin
               pragma Assert (Ru = -Double_Uns (-X) + Double_Uns (Y));
               if Ru < 2 ** (Double_Size - 1) then  --  R >= 0
                  Lemma_Subtract_Double_Uns (-X, Y);
                  pragma Assert (Ru = Double_Uns (X + Y));

               elsif Ru = 2 ** (Double_Size - 1) then
                  pragma Assert (Double_Uns (Y) < 2 ** (Double_Size - 1));
                  pragma Assert (Double_Uns (-X) < 2 ** (Double_Size - 1));
                  pragma Assert (False);

               else
                  pragma Assert
                    (R = -Double_Int (-(-Double_Uns (-X) + Double_Uns (Y))));
                  pragma Assert
                    (R = -Double_Int (-Double_Uns (Y) + Double_Uns (-X)));
               end if;
            end;
         end if;
      end Prove_Negative_X;

      --------------------------
      -- Prove_Non_Negative_X --
      --------------------------

      procedure Prove_Non_Negative_X is
      begin
         if Y >= 0 or else Y = Double_Int'First then
            null;
         else
            pragma Assert
              (To_Uns (X) + To_Uns (Y) = Double_Uns (X) - Double_Uns (-Y));
         end if;
      end Prove_Non_Negative_X;

      -------------------------
      -- Prove_Overflow_Case --
      -------------------------

      procedure Prove_Overflow_Case is
      begin
         if X < 0 and then X /= Double_Int'First and then Y /= Double_Int'First
         then
            pragma Assert
              (To_Uns (X) + To_Uns (Y) = -Double_Uns (-X) - Double_Uns (-Y));
         end if;
      end Prove_Overflow_Case;

   --  Start of processing for Add_With_Ovflo_Check

   begin
      if X >= 0 then
         if Y < 0 or else R >= 0 then
            Prove_Non_Negative_X;
            return R;
         end if;

      else -- X < 0
         if Y > 0 or else R < 0 then
            Prove_Negative_X;
            return R;
         end if;
      end if;

      Prove_Overflow_Case;
      Raise_Error;
   end Add_With_Ovflo_Check;

   -------------------
   -- Double_Divide --
   -------------------

   pragma Annotate (Gnatcheck, Exempt_On, "Metrics_Cyclomatic_Complexity",
                    "limit exceeded due to proof code");
   procedure Double_Divide
     (X, Y, Z : Double_Int;
      Q, R    : out Double_Int;
      Round   : Boolean)
   is
      Xu  : constant Double_Uns := abs X;
      Yu  : constant Double_Uns := abs Y;

      Yhi : constant Single_Uns := Hi (Yu);
      Ylo : constant Single_Uns := Lo (Yu);

      Zu  : constant Double_Uns := abs Z;
      Zhi : constant Single_Uns := Hi (Zu);
      Zlo : constant Single_Uns := Lo (Zu);

      T1, T2     : Double_Uns;
      Du, Qu, Ru : Double_Uns;
      Den_Pos    : constant Boolean := (Y < 0) = (Z < 0);

      --  Local ghost variables

      Mult  : constant Big_Integer := abs (Big (Y) * Big (Z)) with Ghost;
      Quot  : Big_Integer with Ghost;
      Big_R : Big_Integer with Ghost;
      Big_Q : Big_Integer with Ghost;

      --  Local lemmas

      procedure Prove_Overflow_Case
      with
        Ghost,
        Pre  => X = Double_Int'First and then Big (Y) * Big (Z) = -1,
        Post => not In_Double_Int_Range (Big (X) / (Big (Y) * Big (Z)))
          and then not In_Double_Int_Range
            (Round_Quotient (Big (X), Big (Y) * Big (Z),
                             Big (X) / (Big (Y) * Big (Z)),
                             Big (X) rem (Big (Y) * Big (Z))));
      --  Proves the special case where -2**(Double_Size - 1) is divided by -1,
      --  generating an overflow.

      procedure Prove_Quotient_Zero
      with
        Ghost,
        Pre  => Mult >= Big_2xxDouble
          and then
            not (Mult = Big_2xxDouble
                   and then X = Double_Int'First
                   and then Round)
          and then Q = 0
          and then R = X,
        Post => Big (R) = Big (X) rem (Big (Y) * Big (Z))
          and then
            (if Round then
               Big (Q) = Round_Quotient (Big (X), Big (Y) * Big (Z),
                                         Big (X) / (Big (Y) * Big (Z)),
                                         Big (R))
             else Big (Q) = Big (X) / (Big (Y) * Big (Z)));
      --  Proves the general case where divisor doesn't fit in Double_Uns and
      --  quotient is 0.

      procedure Prove_Round_To_One
      with
        Ghost,
        Pre  => Mult = Big_2xxDouble
          and then X = Double_Int'First
          and then Q = (if Den_Pos then -1 else 1)
          and then R = X
          and then Round,
        Post => Big (R) = Big (X) rem (Big (Y) * Big (Z))
          and then Big (Q) = Round_Quotient (Big (X), Big (Y) * Big (Z),
                                             Big (X) / (Big (Y) * Big (Z)),
                                             Big (R));
      --  Proves the special case where the divisor doesn't fit in Double_Uns
      --  but quotient is still 1 or -1 due to rounding
      --  (abs (Y*Z) = 2**Double_Size and X = -2**(Double_Size - 1) and Round).

      procedure Prove_Rounding_Case
      with
        Ghost,
        Pre  => Mult /= 0
          and then Quot = Big (X) / (Big (Y) * Big (Z))
          and then Big_R = Big (X) rem (Big (Y) * Big (Z))
          and then Big_Q =
            Round_Quotient (Big (X), Big (Y) * Big (Z), Quot, Big_R)
          and then Big (Ru) = abs Big_R
          and then Big (Du) = Mult
          and then Big (Qu) =
            (if Ru > (Du - Double_Uns'(1)) / Double_Uns'(2)
             then abs Quot + 1
             else abs Quot),
        Post => abs Big_Q = Big (Qu);
      --  Proves correctness of the rounding of the unsigned quotient

      procedure Prove_Sign_Quotient
      with
        Ghost,
        Pre  => Mult /= 0
          and then Quot = Big (X) / (Big (Y) * Big (Z))
          and then Big_R = Big (X) rem (Big (Y) * Big (Z))
          and then Big_Q =
            (if Round then
               Round_Quotient (Big (X), Big (Y) * Big (Z), Quot, Big_R)
             else Quot),
        Post =>
          (if X >= 0 then
             (if Den_Pos then Big_Q >= 0 else Big_Q <= 0)
           else
             (if Den_Pos then Big_Q <= 0 else Big_Q >= 0));
      --  Proves the correct sign of the signed quotient Big_Q

      procedure Prove_Signs
      with
        Ghost,
        Pre  => Mult /= 0
          and then Quot = Big (X) / (Big (Y) * Big (Z))
          and then Big_R = Big (X) rem (Big (Y) * Big (Z))
          and then Big_Q =
            (if Round then
               Round_Quotient (Big (X), Big (Y) * Big (Z), Quot, Big_R)
             else Quot)
          and then Big (Ru) = abs Big_R
          and then Big (Qu) = abs Big_Q
          and then R = (if X >= 0 then To_Int (Ru) else To_Int (-Ru))
          and then
            Q = (if (X >= 0) = Den_Pos then To_Int (Qu) else To_Int (-Qu))
          and then not (X = Double_Int'First and then Big (Y) * Big (Z) = -1),
        Post => Big (R) = Big (X) rem (Big (Y) * Big (Z))
          and then
            (if Round then
               Big (Q) = Round_Quotient (Big (X), Big (Y) * Big (Z),
                                         Big (X) / (Big (Y) * Big (Z)),
                                         Big (R))
             else Big (Q) = Big (X) / (Big (Y) * Big (Z)));
      --  Proves final signs match the intended result after the unsigned
      --  division is done.

      -----------------------------
      -- Local lemma null bodies --
      -----------------------------

      procedure Prove_Overflow_Case is null;
      procedure Prove_Quotient_Zero is null;
      procedure Prove_Round_To_One is null;
      procedure Prove_Sign_Quotient is null;

      -------------------------
      -- Prove_Rounding_Case --
      -------------------------

      procedure Prove_Rounding_Case is
      begin
         if Same_Sign (Big (X), Big (Y) * Big (Z)) then
            null;
         end if;
      end Prove_Rounding_Case;

      -----------------
      -- Prove_Signs --
      -----------------

      procedure Prove_Signs is
      begin
         if (X >= 0) = Den_Pos then
            pragma Assert (Quot >= 0);
            pragma Assert (Big_Q >= 0);
            pragma Assert (Q >= 0);
            pragma Assert (Big (Q) = Big_Q);
         else
            pragma Assert ((X >= 0) /= (Big (Y) * Big (Z) >= 0));
            pragma Assert (Quot <= 0);
            pragma Assert (Big_Q <= 0);
            pragma Assert (if X >= 0 then R >= 0);
            pragma Assert (if X < 0 then R <= 0);
            pragma Assert (Big (R) = Big_R);
         end if;
      end Prove_Signs;

   --  Start of processing for Double_Divide

   begin
      if Yu = 0 or else Zu = 0 then
         Raise_Error;
      end if;

      pragma Assert (Mult /= 0);
      pragma Assert (Den_Pos = (Big (Y) * Big (Z) > 0));
      Quot := Big (X) / (Big (Y) * Big (Z));
      Big_R := Big (X) rem (Big (Y) * Big (Z));
      if Round then
         Big_Q := Round_Quotient (Big (X), Big (Y) * Big (Z), Quot, Big_R);
      else
         Big_Q := Quot;
      end if;
      Lemma_Abs_Mult_Commutation (Big (Y), Big (Z));
      Lemma_Mult_Decomposition (Mult, Yu, Zu, Yhi, Ylo, Zhi, Zlo);

      --  Compute Y * Z. Note that if the result overflows Double_Uns, then
      --  the rounded result is zero, except for the very special case where
      --  X = -2 ** (Double_Size - 1) and abs (Y * Z) = 2 ** Double_Size, when
      --  Round is True.

      if Yhi /= 0 then
         if Zhi /= 0 then
            R := X;

            --  Handle the special case when Round is True

            if Yhi = 1
              and then Zhi = 1
              and then Ylo = 0
              and then Zlo = 0
              and then X = Double_Int'First
              and then Round
            then
               Q := (if Den_Pos then -1 else 1);

               Prove_Round_To_One;

            else
               Q := 0;

               pragma Assert (Double_Uns'(Yhi * Zhi) >= Double_Uns (Yhi));
               pragma Assert (Double_Uns'(Yhi * Zhi) >= Double_Uns (Zhi));
               pragma Assert (Big (Double_Uns'(Yhi * Zhi)) >= 1);
               if Yhi > 1 or else Zhi > 1 then
                  pragma Assert (Big (Double_Uns'(Yhi * Zhi)) > 1);
                  pragma Assert (if X = Double_Int'First and then Round then
                                    Mult > Big_2xxDouble);
               elsif Zlo > 0 then
                  pragma Assert (Big (Double_Uns'(Yhi * Zlo)) > 0);
                  pragma Assert (if X = Double_Int'First and then Round then
                                    Mult > Big_2xxDouble);
               elsif Ylo > 0 then
                  pragma Assert (Double_Uns'(Ylo * Zhi) > 0);
                  pragma Assert (Big (Double_Uns'(Ylo * Zhi)) > 0);
                  pragma Assert (if X = Double_Int'First and then Round then
                                    Mult > Big_2xxDouble);
               else
                  pragma Assert (not (X = Double_Int'First and then Round));
               end if;
               Prove_Quotient_Zero;
            end if;

            return;
         else
            T2 := Yhi * Zlo;
            pragma Assert (Big (T2) = Big (Double_Uns'(Yhi * Zlo)));
            pragma Assert (Big_0 = Big (Double_Uns'(Ylo * Zhi)));
         end if;

      else
         T2 := Ylo * Zhi;
         pragma Assert (Big (T2) = Big (Double_Uns'(Ylo * Zhi)));
         pragma Assert (Big_0 = Big (Double_Uns'(Yhi * Zlo)));
      end if;

      T1 := Ylo * Zlo;

      Lemma_Mult_Distribution (Big_2xxSingle,
                               Big (Double_Uns'(Yhi * Zlo)),
                               Big (Double_Uns'(Ylo * Zhi)));
      Lemma_Hi_Lo (T1, Hi (T1), Lo (T1));
      Lemma_Mult_Distribution (Big_2xxSingle,
                               Big (T2),
                               Big (Double_Uns (Hi (T1))));
      Lemma_Add_Commutation (T2, Hi (T1));

      T2 := T2 + Hi (T1);

      Lemma_Hi_Lo (T2, Hi (T2), Lo (T2));
      Lemma_Mult_Distribution (Big_2xxSingle,
                               Big (Double_Uns (Hi (T2))),
                               Big (Double_Uns (Lo (T2))));
      Lemma_Double_Big_2xxSingle;

      if Hi (T2) /= 0 then
         R := X;

         --  Handle the special case when Round is True

         if Hi (T2) = 1
           and then Lo (T2) = 0
           and then Lo (T1) = 0
           and then X = Double_Int'First
           and then Round
         then
            Q := (if Den_Pos then -1 else 1);

            Prove_Round_To_One;

         else
            Q := 0;

            pragma Assert (Big (Double_Uns (Hi (T2))) >= 1);
            pragma Assert (Big (Double_Uns (Lo (T2))) >= 0);
            pragma Assert (Big (Double_Uns (Lo (T1))) >= 0);
            pragma Assert (Big_2xxSingle * Big (Double_Uns (Lo (T2)))
                                         + Big (Double_Uns (Lo (T1))) >= 0);
            pragma Assert (Mult >= Big_2xxDouble * Big (Double_Uns (Hi (T2))));
            pragma Assert (Mult >= Big_2xxDouble);
            if Hi (T2) > 1 then
               pragma Assert (Big (Double_Uns (Hi (T2))) > 1);
               pragma Assert (if X = Double_Int'First and then Round then
                                 Mult > Big_2xxDouble);
            elsif Lo (T2) > 0 then
               pragma Assert (Big (Double_Uns (Lo (T2))) > 0);
               pragma Assert (Big_2xxSingle > 0);
               pragma Assert (Big_2xxSingle * Big (Double_Uns (Lo (T2))) > 0);
               pragma Assert (Big_2xxSingle * Big (Double_Uns (Lo (T2)))
                                            + Big (Double_Uns (Lo (T1))) > 0);
               pragma Assert (if X = Double_Int'First and then Round then
                                 Mult > Big_2xxDouble);
            elsif Lo (T1) > 0 then
               pragma Assert (Double_Uns (Lo (T1)) > 0);
               Lemma_Gt_Commutation (Double_Uns (Lo (T1)), 0);
               pragma Assert (Big (Double_Uns (Lo (T1))) > 0);
               pragma Assert (if X = Double_Int'First and then Round then
                                 Mult > Big_2xxDouble);
            else
               pragma Assert (not (X = Double_Int'First and then Round));
            end if;
            Prove_Quotient_Zero;
         end if;

         return;
      end if;

      Du := Lo (T2) & Lo (T1);

      Lemma_Hi_Lo (Du, Lo (T2), Lo (T1));
      pragma Assert (Mult = Big (Du));
      pragma Assert (Du /= 0);
      --  Multiplication of 2-limb arguments Yu and Zu leads to 4-limb result
      --  (where each limb is a single value). Cases where 4 limbs are needed
      --  require Yhi /= 0 and Zhi /= 0 and lead to early exit. Remaining cases
      --  where 3 limbs are needed correspond to Hi(T2) /= 0 and lead to early
      --  exit. Thus, at this point, the result fits in 2 limbs which are
      --  exactly Lo (T2) and Lo (T1), which corresponds to the value of Du.
      --  As the case where one of Yu or Zu is null also led to early exit,
      --  we have Du /= 0 here.

      --  Check overflow case of largest negative number divided by -1

      if X = Double_Int'First and then Du = 1 and then not Den_Pos then
         Prove_Overflow_Case;
         Raise_Error;
      end if;

      --  Perform the actual division

      pragma Assert (Du /= 0);
      --  Multiplication of 2-limb arguments Yu and Zu leads to 4-limb result
      --  (where each limb is a single value). Cases where 4 limbs are needed
      --  require Yhi/=0 and Zhi/=0 and lead to early exit. Remaining cases
      --  where 3 limbs are needed correspond to Hi(T2)/=0 and lead to early
      --  exit. Thus, at this point, the result fits in 2 limbs which are
      --  exactly Lo(T2) and Lo(T1), which corresponds to the value of Du.
      --  As the case where one of Yu or Zu is null also led to early exit,
      --  we have Du/=0 here.

      Qu := Xu / Du;
      Ru := Xu rem Du;

      Lemma_Div_Commutation (Xu, Du);
      Lemma_Abs_Div_Commutation (Big (X), Big (Y) * Big (Z));
      Lemma_Abs_Commutation (X);
      pragma Assert (abs Quot = Big (Qu));
      Lemma_Rem_Commutation (Xu, Du);
      Lemma_Abs_Rem_Commutation (Big (X), Big (Y) * Big (Z));
      pragma Assert (abs Big_R = Big (Ru));

      --  Deal with rounding case

      if Round then
         if Ru > (Du - Double_Uns'(1)) / Double_Uns'(2) then
            Lemma_Add_Commutation (Qu, 1);

            Qu := Qu + Double_Uns'(1);
         end if;

         Prove_Rounding_Case;
      end if;

      pragma Assert (abs Big_Q = Big (Qu));
      Prove_Sign_Quotient;

      --  Set final signs (RM 4.5.5(27-30))

      --  Case of dividend (X) sign positive

      if X >= 0 then
         R := To_Int (Ru);
         Q := (if Den_Pos then To_Int (Qu) else To_Int (-Qu));

      --  We perform the unary minus operation on the unsigned value
      --  before conversion to signed, to avoid a possible overflow
      --  for value -2 ** (Double_Size - 1), both for computing R and Q.

      --  Case of dividend (X) sign negative

      else
         R := To_Int (-Ru);
         Q := (if Den_Pos then To_Int (-Qu) else To_Int (Qu));
      end if;

      Prove_Signs;
   end Double_Divide;
   pragma Annotate (Gnatcheck, Exempt_Off, "Metrics_Cyclomatic_Complexity");

   ---------
   -- Le3 --
   ---------

   function Le3 (X1, X2, X3, Y1, Y2, Y3 : Single_Uns) return Boolean is
   begin
      if X1 < Y1 then
         return True;
      elsif X1 > Y1 then
         return False;
      elsif X2 < Y2 then
         return True;
      elsif X2 > Y2 then
         return False;
      else
         return X3 <= Y3;
      end if;
   end Le3;

   -------------------------------
   -- Lemma_Abs_Div_Commutation --
   -------------------------------

   procedure Lemma_Abs_Div_Commutation (X, Y : Big_Integer) is
   begin
      if Y < 0 then
         if X < 0 then
            pragma Assert (abs (X / Y) = abs (X / (-Y)));
         else
            Lemma_Neg_Div (X, Y);
            pragma Assert (abs (X / Y) = abs ((-X) / (-Y)));
         end if;
      end if;
   end Lemma_Abs_Div_Commutation;

   -------------------------------
   -- Lemma_Abs_Rem_Commutation --
   -------------------------------

   procedure Lemma_Abs_Rem_Commutation (X, Y : Big_Integer) is
   begin
      if Y < 0 then
         Lemma_Neg_Rem (X, Y);
         if X < 0 then
            pragma Assert (X rem Y = -((-X) rem (-Y)));
            pragma Assert (abs (X rem Y) = (abs X) rem (abs Y));
         else
            pragma Assert (abs (X rem Y) = (abs X) rem (abs Y));
         end if;
      end if;
   end Lemma_Abs_Rem_Commutation;

   -----------------------------
   -- Lemma_Concat_Definition --
   -----------------------------

   procedure Lemma_Concat_Definition (X, Y : Single_Uns) is
      Hi : constant Double_Uns := Shift_Left (Double_Uns (X), Single_Size);
      Lo : constant Double_Uns := Double_Uns (Y);
   begin
      pragma Assert (Hi = Double_Uns'(2 ** Single_Size) * Double_Uns (X));
      pragma Assert ((Hi or Lo) = Hi + Lo);
   end Lemma_Concat_Definition;

   ------------------
   -- Lemma_Div_Eq --
   ------------------

   procedure Lemma_Div_Eq (A, B, S, R : Big_Integer) is
   begin
      pragma Assert ((A - B) * S = R);
      pragma Assert ((A - B) * S / S = R / S);
      Lemma_Mult_Div (A - B, S);
      pragma Assert (A - B = R / S);
   end Lemma_Div_Eq;

   ------------------------
   -- Lemma_Double_Shift --
   ------------------------

   procedure Lemma_Double_Shift (X : Double_Uns; S, S1 : Natural) is
   begin
      Lemma_Double_Shift (X, Double_Uns (S), Double_Uns (S1));
      pragma Assert (Shift_Left (Shift_Left (X, S), S1)
        = Shift_Left (Shift_Left (X, S), Natural (Double_Uns (S1))));
      pragma Assert (Shift_Left (X, S + S1)
        = Shift_Left (X, Natural (Double_Uns (S + S1))));
   end Lemma_Double_Shift;

   -----------------------------
   -- Lemma_Double_Shift_Left --
   -----------------------------

   procedure Lemma_Double_Shift_Left (X : Double_Uns; S, S1 : Natural) is
   begin
      Lemma_Double_Shift_Left (X, Double_Uns (S), Double_Uns (S1));
      pragma Assert (Shift_Left (Shift_Left (X, S), S1)
        = Shift_Left (Shift_Left (X, S), Natural (Double_Uns (S1))));
      pragma Assert (Shift_Left (X, S + S1)
        = Shift_Left (X, Natural (Double_Uns (S + S1))));
   end Lemma_Double_Shift_Left;

   ------------------------------
   -- Lemma_Double_Shift_Right --
   ------------------------------

   procedure Lemma_Double_Shift_Right (X : Double_Uns; S, S1 : Natural) is
   begin
      Lemma_Double_Shift_Right (X, Double_Uns (S), Double_Uns (S1));
      pragma Assert (Shift_Right (Shift_Right (X, S), S1)
        = Shift_Right (Shift_Right (X, S), Natural (Double_Uns (S1))));
      pragma Assert (Shift_Right (X, S + S1)
        = Shift_Right (X, Natural (Double_Uns (S + S1))));
   end Lemma_Double_Shift_Right;

   -----------------
   -- Lemma_Hi_Lo --
   -----------------

   procedure Lemma_Hi_Lo (Xu : Double_Uns; Xhi, Xlo : Single_Uns) is
   begin
      pragma Assert (Double_Uns (Xhi) = Xu / Double_Uns'(2 ** Single_Size));
      pragma Assert (Double_Uns (Xlo) = Xu mod 2 ** Single_Size);
   end Lemma_Hi_Lo;

   -------------------
   -- Lemma_Hi_Lo_3 --
   -------------------

   procedure Lemma_Hi_Lo_3 (Xu : Double_Uns; Xhi, Xlo : Single_Uns) is
   begin
      Lemma_Hi_Lo (Xu, Xhi, Xlo);
   end Lemma_Hi_Lo_3;

   ------------------------------
   -- Lemma_Mult_Decomposition --
   ------------------------------

   procedure Lemma_Mult_Decomposition
     (Mult               : Big_Integer;
      Xu, Yu             : Double_Uns;
      Xhi, Xlo, Yhi, Ylo : Single_Uns)
   is
   begin
      Lemma_Hi_Lo (Xu, Xhi, Xlo);
      Lemma_Hi_Lo (Yu, Yhi, Ylo);

      pragma Assert
        (Mult =
           (Big_2xxSingle * Big (Double_Uns (Xhi)) + Big (Double_Uns (Xlo))) *
           (Big_2xxSingle * Big (Double_Uns (Yhi)) + Big (Double_Uns (Ylo))));
      pragma Assert (Mult =
        Big_2xxSingle
          * Big_2xxSingle * Big (Double_Uns (Xhi)) * Big (Double_Uns (Yhi))
          + Big_2xxSingle * Big (Double_Uns (Xhi)) * Big (Double_Uns (Ylo))
          + Big_2xxSingle * Big (Double_Uns (Xlo)) * Big (Double_Uns (Yhi))
                          + Big (Double_Uns (Xlo)) * Big (Double_Uns (Ylo)));
      Lemma_Deep_Mult_Commutation (Big_2xxSingle * Big_2xxSingle, Xhi, Yhi);
      Lemma_Deep_Mult_Commutation (Big_2xxSingle, Xhi, Ylo);
      Lemma_Deep_Mult_Commutation (Big_2xxSingle, Xlo, Yhi);
      Lemma_Mult_Commutation (Xlo, Ylo);
      pragma Assert (Mult =
        Big_2xxSingle * Big_2xxSingle * Big (Double_Uns'(Xhi * Yhi))
                      + Big_2xxSingle * Big (Double_Uns'(Xhi * Ylo))
                      + Big_2xxSingle * Big (Double_Uns'(Xlo * Yhi))
                                      + Big (Double_Uns'(Xlo * Ylo)));
   end Lemma_Mult_Decomposition;

   --------------------
   -- Lemma_Mult_Div --
   --------------------

   procedure Lemma_Mult_Div (A, B : Big_Integer) is
   begin
      if B > 0 then
         pragma Assert (A * B / B = A);
      else
         pragma Assert (A * (-B) / (-B) = A);
      end if;
   end Lemma_Mult_Div;

   -------------------
   -- Lemma_Neg_Div --
   -------------------

   procedure Lemma_Neg_Div (X, Y : Big_Integer) is
   begin
      pragma Assert ((-X) / (-Y) = -(X / (-Y)));
      pragma Assert (X / (-Y) = -(X / Y));
   end Lemma_Neg_Div;

   -----------------------
   -- Lemma_Powers_Of_2 --
   -----------------------

   procedure Lemma_Powers_Of_2 (M, N : Natural) is
   begin
      if M + N < Double_Size then
         pragma Assert (Double_Uns'(2**M) * Double_Uns'(2**N)
                        = Double_Uns'(2**(M + N)));
      end if;

      Lemma_Powers_Of_2_Commutation (M);
      Lemma_Powers_Of_2_Commutation (N);
      Lemma_Powers_Of_2_Commutation (M + N);
      Lemma_Powers (Big (Double_Uns'(2)), M, N);

      if M + N < Double_Size then
         pragma Assert (Big (Double_Uns'(2))**M * Big (Double_Uns'(2))**N
                        = Big (Double_Uns'(2))**(M + N));
         Lemma_Powers_Of_2_Increasing (M + N, Double_Size);
         Lemma_Mult_Commutation (2 ** M, 2 ** N, 2 ** (M + N));
      else
         pragma Assert (Big (Double_Uns'(2))**M * Big (Double_Uns'(2))**N
                        = Big (Double_Uns'(2))**(M + N));
      end if;
   end Lemma_Powers_Of_2;

   -----------------------------------
   -- Lemma_Powers_Of_2_Commutation --
   -----------------------------------

   procedure Lemma_Powers_Of_2_Commutation (M : Natural) is
   begin
      if M > 0 then
         Lemma_Powers_Of_2_Commutation (M - 1);
         pragma Assert (Big (Double_Uns'(2))**(M - 1) = Big_2xx (M - 1));
         pragma Assert (Big (Double_Uns'(2))**M = Big_2xx (M - 1) * 2);
         if M < Double_Size then
            Lemma_Powers_Of_2_Increasing (M - 1, Double_Size - 1);
            Lemma_Bounded_Powers_Of_2_Increasing (M - 1, Double_Size - 1);
            pragma Assert (Double_Uns'(2 ** (M - 1)) * 2 = Double_Uns'(2**M));
            Lemma_Mult_Commutation
              (Double_Uns'(2 ** (M - 1)), 2, Double_Uns'(2**M));
            pragma Assert (Big (Double_Uns'(2))**M = Big_2xx (M));
         end if;
      else
         pragma Assert (Big (Double_Uns'(2))**M = Big_2xx (M));
      end if;
   end Lemma_Powers_Of_2_Commutation;

   ----------------------------------
   -- Lemma_Powers_Of_2_Increasing --
   ----------------------------------

   procedure Lemma_Powers_Of_2_Increasing (M, N : Natural) is
   begin
      if M + 1 < N then
         Lemma_Powers_Of_2_Increasing (M + 1, N);
      end if;
   end Lemma_Powers_Of_2_Increasing;

   -------------------
   -- Lemma_Rem_Abs --
   -------------------

   procedure Lemma_Rem_Abs (X, Y : Big_Integer) is
   begin
      Lemma_Neg_Rem (X, Y);
   end Lemma_Rem_Abs;

   ----------------------
   -- Lemma_Shift_Left --
   ----------------------

   procedure Lemma_Shift_Left (X : Double_Uns; Shift : Natural) is

      procedure Lemma_Mult_Pow2 (X : Double_Uns; I : Natural)
      with
        Ghost,
        Pre  => I < Double_Size - 1,
        Post => X * Double_Uns'(2) ** I * Double_Uns'(2)
          = X * Double_Uns'(2) ** (I + 1);

      procedure Lemma_Mult_Pow2 (X : Double_Uns; I : Natural) is
         Mul1 : constant Double_Uns := Double_Uns'(2) ** I;
         Mul2 : constant Double_Uns := Double_Uns'(2);
         Left : constant Double_Uns := X * Mul1 * Mul2;
      begin
         pragma Assert (Left = X * (Mul1 * Mul2));
         pragma Assert (Mul1 * Mul2 = Double_Uns'(2) ** (I + 1));
      end Lemma_Mult_Pow2;

      XX : Double_Uns := X;

   begin
      for J in 1 .. Shift loop
         declare
            Cur_XX : constant Double_Uns := XX;
         begin
            XX := Shift_Left (XX, 1);
            pragma Assert (XX = Cur_XX * Double_Uns'(2));
            Lemma_Mult_Pow2 (X, J - 1);
         end;
         Lemma_Double_Shift_Left (X, J - 1, 1);
         pragma Loop_Invariant (XX = Shift_Left (X, J));
         pragma Loop_Invariant (XX = X * Double_Uns'(2) ** J);
      end loop;
   end Lemma_Shift_Left;

   -----------------------
   -- Lemma_Shift_Right --
   -----------------------

   procedure Lemma_Shift_Right (X : Double_Uns; Shift : Natural) is

      procedure Lemma_Div_Pow2 (X : Double_Uns; I : Natural)
      with
        Ghost,
        Pre  => I < Double_Size - 1,
        Post => X / Double_Uns'(2) ** I / Double_Uns'(2)
          = X / Double_Uns'(2) ** (I + 1);

      procedure Lemma_Quot_Rem (X, Div, Q, R : Double_Uns)
      with
        Ghost,
        Pre  => Div /= 0
          and then X = Q * Div + R
          and then Q <= Double_Uns'Last / Div
          and then R <= Double_Uns'Last - Q * Div
          and then R < Div,
        Post => Q = X / Div;
      pragma Annotate (GNATprove, False_Positive, "postcondition might fail",
                       "Q is the quotient of X by Div");

      procedure Lemma_Div_Pow2 (X : Double_Uns; I : Natural) is

         --  Local lemmas

         procedure Lemma_Mult_Le (X, Y, Z : Double_Uns)
         with
           Ghost,
           Pre  => X <= 1,
           Post => X * Z <= Z;

         procedure Lemma_Mult_Le (X, Y, Z : Double_Uns) is null;

         --  Local variables

         Div1 : constant Double_Uns := Double_Uns'(2) ** I;
         Div2 : constant Double_Uns := Double_Uns'(2);
         Left : constant Double_Uns := X / Div1 / Div2;
         R2   : constant Double_Uns := X / Div1 - Left * Div2;
         pragma Assert (R2 <= Div2 - 1);
         R1   : constant Double_Uns := X - X / Div1 * Div1;
         pragma Assert (R1 < Div1);

      --  Start of processing for Lemma_Div_Pow2

      begin
         pragma Assert (X = Left * (Div1 * Div2) + R2 * Div1 + R1);
         Lemma_Mult_Le (R2, Div2 - 1, Div1);
         pragma Assert (R2 * Div1 + R1 < Div1 * Div2);
         Lemma_Quot_Rem (X, Div1 * Div2, Left, R2 * Div1 + R1);
         pragma Assert (Left = X / (Div1 * Div2));
         pragma Assert (Div1 * Div2 = Double_Uns'(2) ** (I + 1));
      end Lemma_Div_Pow2;

      procedure Lemma_Quot_Rem (X, Div, Q, R : Double_Uns) is null;

      XX : Double_Uns := X;

   begin
      for J in 1 .. Shift loop
         declare
            Cur_XX : constant Double_Uns := XX;
         begin
            XX := Shift_Right (XX, 1);
            pragma Assert (XX = Cur_XX / Double_Uns'(2));
            Lemma_Div_Pow2 (X, J - 1);
         end;
         Lemma_Double_Shift_Right (X, J - 1, 1);
         pragma Loop_Invariant (XX = Shift_Right (X, J));
         pragma Loop_Invariant (XX = X / Double_Uns'(2) ** J);
      end loop;
   end Lemma_Shift_Right;

   ------------------------------
   -- Lemma_Shift_Without_Drop --
   ------------------------------

   procedure Lemma_Shift_Without_Drop
     (X, Y  : Double_Uns;
      Mask  : Single_Uns;
      Shift : Natural)
   is
      pragma Unreferenced (Mask);

      procedure Lemma_Bound
      with
        Pre  => Shift <= Single_Size
          and then X <= 2**Single_Size
            * Double_Uns'(2**(Single_Size - Shift) - 1)
            + Single_Uns'(2**Single_Size - 1),
        Post => X <= 2**(Double_Size - Shift) - 1;

      procedure Lemma_Exp_Pos (N : Integer)
      with
        Pre  => N in 0 .. Double_Size - 1,
        Post => Double_Uns'(2**N) > 0;

      -----------------------------
      -- Local lemma null bodies --
      -----------------------------

      procedure Lemma_Bound is null;
      procedure Lemma_Exp_Pos (N : Integer) is null;

   --  Start of processing for Lemma_Shift_Without_Drop

   begin
      if Shift = 0 then
         pragma Assert (Big (Y) = Big_2xx (Shift) * Big (X));
         return;
      end if;

      Lemma_Bound;
      Lemma_Exp_Pos (Double_Size - Shift);
      pragma Assert (X < 2**(Double_Size - Shift));
      pragma Assert (Big (X) < Big_2xx (Double_Size - Shift));
      pragma Assert (Y = 2**Shift * X);
      Lemma_Lt_Mult (Big (X), Big_2xx (Double_Size - Shift), Big_2xx (Shift),
                     Big_2xx (Shift) * Big_2xx (Double_Size - Shift));
      pragma Assert (Big_2xx (Shift) * Big (X)
                     < Big_2xx (Shift) * Big_2xx (Double_Size - Shift));
      Lemma_Powers_Of_2 (Shift, Double_Size - Shift);
      Lemma_Mult_Commutation (2**Shift, X, Y);
      pragma Assert (Big (Y) = Big_2xx (Shift) * Big (X));
   end Lemma_Shift_Without_Drop;

   -------------------------------
   -- Multiply_With_Ovflo_Check --
   -------------------------------

   function Multiply_With_Ovflo_Check (X, Y : Double_Int) return Double_Int is
      Xu  : constant Double_Uns := abs X;
      Xhi : constant Single_Uns := Hi (Xu);
      Xlo : constant Single_Uns := Lo (Xu);

      Yu  : constant Double_Uns := abs Y;
      Yhi : constant Single_Uns := Hi (Yu);
      Ylo : constant Single_Uns := Lo (Yu);

      T1, T2 : Double_Uns;

      --  Local ghost variables

      Mult : constant Big_Integer := abs (Big (X) * Big (Y)) with Ghost;

      --  Local lemmas

      procedure Prove_Both_Too_Large
      with
        Ghost,
        Pre  => Xhi /= 0
          and then Yhi /= 0
          and then Mult =
            Big_2xxSingle * Big_2xxSingle * (Big (Double_Uns'(Xhi * Yhi)))
                      + Big_2xxSingle * (Big (Double_Uns'(Xhi * Ylo)))
                      + Big_2xxSingle * (Big (Double_Uns'(Xlo * Yhi)))
                                  + (Big (Double_Uns'(Xlo * Ylo))),
        Post => not In_Double_Int_Range (Big (X) * Big (Y));

      procedure Prove_Final_Decomposition
      with
        Ghost,
        Pre  => In_Double_Int_Range (Big (X) * Big (Y))
          and then Mult = Big_2xxSingle * Big (T2) + Big (Double_Uns (Lo (T1)))
          and then Hi (T2) = 0,
        Post => Mult = Big (Lo (T2) & Lo (T1));

      procedure Prove_Neg_Int
      with
        Ghost,
        Pre  => In_Double_Int_Range (Big (X) * Big (Y))
          and then Mult = Big (T2)
          and then ((X >= 0 and then Y < 0) or else (X < 0 and then Y >= 0)),
        Post => To_Neg_Int (T2) = X * Y;

      procedure Prove_Pos_Int
      with
        Ghost,
        Pre  => In_Double_Int_Range (Big (X) * Big (Y))
          and then Mult = Big (T2)
          and then ((X >= 0 and then Y >= 0) or else (X < 0 and then Y < 0)),
        Post => In_Double_Int_Range (Big (T2))
          and then To_Pos_Int (T2) = X * Y;

      procedure Prove_Result_Too_Large
      with
        Ghost,
        Pre  => Mult = Big_2xxSingle * Big (T2) + Big (Double_Uns (Lo (T1)))
          and then Hi (T2) /= 0,
        Post => not In_Double_Int_Range (Big (X) * Big (Y));

      procedure Prove_Too_Large
      with
        Ghost,
        Pre  => abs (Big (X) * Big (Y)) >= Big_2xxDouble,
        Post => not In_Double_Int_Range (Big (X) * Big (Y));

      --------------------------
      -- Prove_Both_Too_Large --
      --------------------------

      procedure Prove_Both_Too_Large is
      begin
         pragma Assert (Mult >=
           Big_2xxSingle * Big_2xxSingle * Big (Double_Uns'(Xhi * Yhi)));
         pragma Assert (Double_Uns (Xhi) * Double_Uns (Yhi) >= 1);
         pragma Assert (Mult >= Big_2xxSingle * Big_2xxSingle);
         Prove_Too_Large;
      end Prove_Both_Too_Large;

      -------------------------------
      -- Prove_Final_Decomposition --
      -------------------------------

      procedure Prove_Final_Decomposition is
      begin
         Lemma_Hi_Lo (T2, Hi (T2), Lo (T2));
         pragma Assert (Mult = Big_2xxSingle * Big (Double_Uns (Lo (T2)))
                                             + Big (Double_Uns (Lo (T1))));
         pragma Assert (Mult <= Big_2xxDouble_Minus_1);
         Lemma_Mult_Commutation (X, Y);
         pragma Assert (Mult = abs (Big (X * Y)));
         Lemma_Word_Commutation (Lo (T2));
         pragma Assert (Mult = Big (Double_Uns'(2 ** Single_Size)
                          * Double_Uns (Lo (T2)))
                          + Big (Double_Uns (Lo (T1))));
         Lemma_Add_Commutation (Double_Uns'(2 ** Single_Size)
                                  * Double_Uns (Lo (T2)),
                                Lo (T1));
         pragma Assert (Mult = Big (Double_Uns'(2 ** Single_Size)
                          * Double_Uns (Lo (T2)) + Lo (T1)));
         pragma Assert (Lo (T2) & Lo (T1) = Double_Uns'(2 ** Single_Size)
                          * Double_Uns (Lo (T2)) + Lo (T1));
      end Prove_Final_Decomposition;

      -------------------
      -- Prove_Neg_Int --
      -------------------

      procedure Prove_Neg_Int is
      begin
         pragma Assert (X * Y <= 0);
         pragma Assert (Mult = -Big (X * Y));
      end Prove_Neg_Int;

      -------------------
      -- Prove_Pos_Int --
      -------------------

      procedure Prove_Pos_Int is
      begin
         pragma Assert (X * Y >= 0);
         pragma Assert (Mult = Big (X * Y));
      end Prove_Pos_Int;

      ----------------------------
      -- Prove_Result_Too_Large --
      ----------------------------

      procedure Prove_Result_Too_Large is
      begin
         pragma Assert (Mult >= Big_2xxSingle * Big (T2));
         Lemma_Hi_Lo (T2, Hi (T2), Lo (T2));
         pragma Assert (Mult >=
           Big_2xxSingle * Big_2xxSingle * Big (Double_Uns (Hi (T2))));
         pragma Assert (Double_Uns (Hi (T2)) >= 1);
         pragma Assert (Mult >= Big_2xxSingle * Big_2xxSingle);
         Prove_Too_Large;
      end Prove_Result_Too_Large;

      ---------------------
      -- Prove_Too_Large --
      ---------------------

      procedure Prove_Too_Large is null;

   --  Start of processing for Multiply_With_Ovflo_Check

   begin
      Lemma_Mult_Decomposition (Mult, Xu, Yu, Xhi, Xlo, Yhi, Ylo);

      if Xhi /= 0 then
         if Yhi /= 0 then
            Prove_Both_Too_Large;
            Raise_Error;
         else
            T2 := Xhi * Ylo;
            pragma Assert (Big (T2) = Big (Double_Uns'(Xhi * Ylo))
                                    + Big (Double_Uns'(Xlo * Yhi)));
         end if;

      elsif Yhi /= 0 then
         T2 := Xlo * Yhi;
         pragma Assert (Big (T2) = Big (Double_Uns'(Xhi * Ylo))
                                 + Big (Double_Uns'(Xlo * Yhi)));

      else -- Yhi = Xhi = 0
         T2 := 0;
      end if;

      --  Here we have T2 set to the contribution to the upper half of the
      --  result from the upper halves of the input values.

      T1 := Xlo * Ylo;

      pragma Assert (Big (T2) = Big (Double_Uns'(Xhi * Ylo))
                              + Big (Double_Uns'(Xlo * Yhi)));
      Lemma_Mult_Distribution (Big_2xxSingle, Big (Double_Uns'(Xhi * Ylo)),
                                              Big (Double_Uns'(Xlo * Yhi)));
      pragma Assert (Mult = Big_2xxSingle * Big (T2) + Big (T1));
      Lemma_Add_Commutation (T2, Hi (T1));
      pragma Assert
        (Big (T2 + Hi (T1)) = Big (T2) + Big (Double_Uns (Hi (T1))));

      T2 := T2 + Hi (T1);

      Lemma_Hi_Lo (T1, Hi (T1), Lo (T1));
      pragma Assert
        (Mult = Big_2xxSingle * Big (T2) + Big (Double_Uns (Lo (T1))));

      if Hi (T2) /= 0 then
         Prove_Result_Too_Large;
         Raise_Error;
      end if;

      Prove_Final_Decomposition;

      T2 := Lo (T2) & Lo (T1);

      pragma Assert (Mult = Big (T2));

      if X >= 0 then
         if Y >= 0 then
            Prove_Pos_Int;
            return To_Pos_Int (T2);
            pragma Annotate (CodePeer, Intentional, "precondition",
                             "Intentional Unsigned->Signed conversion");
         else
            Prove_Neg_Int;
            Lemma_Abs_Range (Big (X) * Big (Y));
            return To_Neg_Int (T2);
         end if;
      else -- X < 0
         if Y < 0 then
            Prove_Pos_Int;
            return To_Pos_Int (T2);
            pragma Annotate (CodePeer, Intentional, "precondition",
                             "Intentional Unsigned->Signed conversion");
         else
            Prove_Neg_Int;
            Lemma_Abs_Range (Big (X) * Big (Y));
            return To_Neg_Int (T2);
         end if;
      end if;

   end Multiply_With_Ovflo_Check;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error is
   begin
      raise Constraint_Error with "Double arithmetic overflow";
      pragma Annotate
        (GNATprove, Intentional, "exception might be raised",
         "Procedure Raise_Error is called to signal input errors");
   end Raise_Error;

   -------------------
   -- Scaled_Divide --
   -------------------

   pragma Annotate (Gnatcheck, Exempt_On, "Metrics_Cyclomatic_Complexity",
                    "limit exceeded due to proof code");
   procedure Scaled_Divide
     (X, Y, Z : Double_Int;
      Q, R    : out Double_Int;
      Round   : Boolean)
   is
      Xu  : constant Double_Uns := abs X;
      Xhi : constant Single_Uns := Hi (Xu);
      Xlo : constant Single_Uns := Lo (Xu);

      Yu  : constant Double_Uns := abs Y;
      Yhi : constant Single_Uns := Hi (Yu);
      Ylo : constant Single_Uns := Lo (Yu);

      Zu  : Double_Uns := abs Z;
      Zhi : Single_Uns := Hi (Zu);
      Zlo : Single_Uns := Lo (Zu);

      D : array (1 .. 4) of Single_Uns with Relaxed_Initialization;
      --  The dividend, four digits (D(1) is high order)

      Qd : array (1 .. 2) of Single_Uns with Relaxed_Initialization;
      --  The quotient digits, two digits (Qd(1) is high order)

      S1, S2, S3 : Single_Uns;
      --  Value to subtract, three digits (S1 is high order)

      Qu : Double_Uns;
      Ru : Double_Uns;
      --  Unsigned quotient and remainder

      Mask : Single_Uns;
      --  Mask of bits used to compute the scaling factor below

      Scale : Natural;
      --  Scaling factor used for multiple-precision divide. Dividend and
      --  Divisor are multiplied by 2 ** Scale, and the final remainder is
      --  divided by the scaling factor. The reason for this scaling is to
      --  allow more accurate estimation of quotient digits.

      Shift : Natural;
      --  Shift factor used to compute the scaling factor above

      T1, T2, T3 : Double_Uns;
      --  Temporary values

      --  Local ghost variables

      Mult  : constant Big_Natural := abs (Big (X) * Big (Y)) with Ghost;
      Quot  : Big_Integer with Ghost;
      Big_R : Big_Integer with Ghost;
      Big_Q : Big_Integer with Ghost;
      Inter : Natural with Ghost;

      --  Local ghost functions

      function Is_Mult_Decomposition
        (D1, D2, D3, D4 : Big_Integer)
         return Boolean
      is
        (Mult = Big_2xxSingle * Big_2xxSingle * Big_2xxSingle * D1
                              + Big_2xxSingle * Big_2xxSingle * D2
                                              + Big_2xxSingle * D3
                                                              + D4)
      with
        Ghost,
        Annotate => (GNATprove, Inline_For_Proof);

      function Is_Scaled_Mult_Decomposition
        (D1, D2, D3, D4 : Big_Integer)
         return Boolean
      is
        (Mult * Big_2xx (Scale)
           = Big_2xxSingle * Big_2xxSingle * Big_2xxSingle * D1
                           + Big_2xxSingle * Big_2xxSingle * D2
                                           + Big_2xxSingle * D3
                                                           + D4)
      with
        Ghost,
        Annotate => (GNATprove, Inline_For_Proof),
        Pre => Scale < Double_Size;

      --  Local lemmas

      procedure Prove_Dividend_Scaling
      with
        Ghost,
        Pre  => D'Initialized
          and then Scale <= Single_Size
          and then Is_Mult_Decomposition (Big (Double_Uns (D (1))),
                                          Big (Double_Uns (D (2))),
                                          Big (Double_Uns (D (3))),
                                          Big (Double_Uns (D (4))))
          and then Big (D (1) & D (2)) * Big_2xx (Scale) < Big_2xxDouble
          and then T1 = Shift_Left (D (1) & D (2), Scale)
          and then T2 = Shift_Left (Double_Uns (D (3)), Scale)
          and then T3 = Shift_Left (Double_Uns (D (4)), Scale),
        Post => Is_Scaled_Mult_Decomposition
          (Big (Double_Uns (Hi (T1))),
           Big (Double_Uns (Lo (T1) or Hi (T2))),
           Big (Double_Uns (Lo (T2) or Hi (T3))),
           Big (Double_Uns (Lo (T3))));
      --  Proves the scaling of the 4-digit dividend actually multiplies it by
      --  2**Scale.

      procedure Prove_Multiplication (Q : Single_Uns)
      with
        Ghost,
        Pre  => T1 = Q * Lo (Zu)
          and then T2 = Q * Hi (Zu)
          and then S3 = Lo (T1)
          and then T3 = Hi (T1) + Lo (T2)
          and then S2 = Lo (T3)
          and then S1 = Hi (T3) + Hi (T2),
        Post => Big3 (S1, S2, S3) = Big (Double_Uns (Q)) * Big (Zu);
      --  Proves correctness of the multiplication of divisor by quotient to
      --  compute amount to subtract.

      procedure Prove_Mult_Decomposition_Split3
        (D1, D2, D3, D3_Hi, D3_Lo, D4 : Big_Integer)
      with
        Ghost,
        Pre  => Is_Mult_Decomposition (D1, D2, D3, D4)
          and then D3 = Big_2xxSingle * D3_Hi + D3_Lo,
        Post => Is_Mult_Decomposition (D1, D2 + D3_Hi, D3_Lo, D4);
      --  Proves decomposition of Mult after splitting third component

      procedure Prove_Negative_Dividend
      with
        Ghost,
        Pre  => Z /= 0
          and then Big (Qu) = abs Big_Q
          and then In_Double_Int_Range (Big_Q)
          and then Big (Ru) = abs Big_R
          and then ((X >= 0 and Y < 0) or (X < 0 and Y >= 0))
          and then Big_Q =
            (if Round then Round_Quotient (Big (X) * Big (Y), Big (Z),
                                           Big (X) * Big (Y) / Big (Z),
                                           Big (X) * Big (Y) rem Big (Z))
             else Big (X) * Big (Y) / Big (Z))
          and then Big_R = Big (X) * Big (Y) rem Big (Z),
         Post =>
           (if Z > 0 then Big_Q <= Big_0
              and then In_Double_Int_Range (-Big (Qu))
            else Big_Q >= Big_0
              and then In_Double_Int_Range (Big (Qu)))
           and then In_Double_Int_Range (-Big (Ru));
      --  Proves the sign of rounded quotient when dividend is non-positive

      procedure Prove_Overflow
      with
        Ghost,
        Pre  => Z /= 0
          and then Mult >= Big_2xxDouble * Big (Double_Uns'(abs Z)),
        Post => not In_Double_Int_Range (Big (X) * Big (Y) / Big (Z))
          and then not In_Double_Int_Range
            (Round_Quotient (Big (X) * Big (Y), Big (Z),
                             Big (X) * Big (Y) / Big (Z),
                             Big (X) * Big (Y) rem Big (Z)));
      --  Proves overflow case when the quotient has at least 3 digits

      procedure Prove_Positive_Dividend
      with
        Ghost,
        Pre  => Z /= 0
          and then Big (Qu) = abs Big_Q
          and then In_Double_Int_Range (Big_Q)
          and then Big (Ru) = abs Big_R
          and then ((X >= 0 and Y >= 0) or (X < 0 and Y < 0))
          and then Big_Q =
            (if Round then Round_Quotient (Big (X) * Big (Y), Big (Z),
                                           Big (X) * Big (Y) / Big (Z),
                                           Big (X) * Big (Y) rem Big (Z))
             else Big (X) * Big (Y) / Big (Z))
          and then Big_R = Big (X) * Big (Y) rem Big (Z),
        Post =>
           (if Z > 0 then Big_Q >= Big_0
              and then In_Double_Int_Range (Big (Qu))
            else Big_Q <= Big_0
              and then In_Double_Int_Range (-Big (Qu)))
           and then In_Double_Int_Range (Big (Ru));
      --  Proves the sign of rounded quotient when dividend is non-negative

      procedure Prove_Qd_Calculation_Part_1 (J : Integer)
      with
        Ghost,
        Pre  => J in 1 .. 2
          and then D'Initialized
          and then D (J) < Zhi
          and then Hi (Zu) = Zhi
          and then Qd (J)'Initialized
          and then Qd (J) = Lo ((D (J) & D (J + 1)) / Zhi),
        Post => Big (Double_Uns (Qd (J))) >=
          Big3 (D (J), D (J + 1), D (J + 2)) / Big (Zu);
      --  When dividing 3 digits by 2 digits, proves the initial calculation
      --  of the quotient given by dividing the first 2 digits of the dividend
      --  by the first digit of the divisor is not an underestimate (so
      --  readjusting down works).

      procedure Prove_Q_Too_Big
      with
        Ghost,
        Pre  => In_Double_Int_Range (Big_Q)
          and then abs Big_Q = Big_2xxDouble,
        Post => False;
      --  Proves the inconsistency when Q is equal to Big_2xx64

      procedure Prove_Rescaling
      with
        Ghost,
        Pre  => Scale <= Single_Size
          and then Z /= 0
          and then Mult * Big_2xx (Scale) = Big (Zu) * Big (Qu) + Big (Ru)
          and then Big (Ru) < Big (Zu)
          and then Big (Zu) = Big (Double_Uns'(abs Z)) * Big_2xx (Scale)
          and then Quot = Big (X) * Big (Y) / Big (Z)
          and then Big_R = Big (X) * Big (Y) rem Big (Z),
        Post => abs Quot = Big (Qu)
          and then abs Big_R = Big (Shift_Right (Ru, Scale));
      --  Proves scaling back only the remainder is the right thing to do after
      --  computing the scaled division.

      procedure Prove_Rounding_Case
      with
        Ghost,
        Pre  => Z /= 0
          and then Quot = Big (X) * Big (Y) / Big (Z)
          and then Big_R = Big (X) * Big (Y) rem Big (Z)
          and then Big_Q =
            Round_Quotient (Big (X) * Big (Y), Big (Z), Quot, Big_R)
          and then Big (Ru) = abs Big_R
          and then Big (Zu) = Big (Double_Uns'(abs Z)),
        Post => abs Big_Q =
          (if Ru > (Zu - Double_Uns'(1)) / Double_Uns'(2)
           then abs Quot + 1
           else abs Quot);
      --  Proves correctness of the rounding of the unsigned quotient

      procedure Prove_Scaled_Mult_Decomposition_Regroup24
        (D1, D2, D3, D4 : Big_Integer)
      with
        Ghost,
        Pre  => Scale < Double_Size
          and then Is_Scaled_Mult_Decomposition (D1, D2, D3, D4),
        Post => Is_Scaled_Mult_Decomposition
          (0, Big_2xxSingle * D1 + D2, 0, Big_2xxSingle * D3 + D4);
      --  Proves scaled decomposition of Mult after regrouping on second and
      --  fourth component.

      procedure Prove_Scaled_Mult_Decomposition_Regroup3
        (D1, D2, D3, D4 : Single_Uns)
      with
        Ghost,
        Pre  => Scale < Double_Size
          and then Is_Scaled_Mult_Decomposition
            (Big (Double_Uns (D1)), Big (Double_Uns (D2)),
             Big (Double_Uns (D3)), Big (Double_Uns (D4))),
        Post => Is_Scaled_Mult_Decomposition (0, 0, Big3 (D1, D2, D3),
                                              Big (Double_Uns (D4)));
      --  Proves scaled decomposition of Mult after regrouping on third
      --  component.

      procedure Prove_Sign_R
      with
        Ghost,
        Pre  => Z /= 0 and then Big_R = Big (X) * Big (Y) rem Big (Z),
        Post => In_Double_Int_Range (Big_R);

      procedure Prove_Signs
      with
        Ghost,
        Pre  => Z /= 0
          and then Quot = Big (X) * Big (Y) / Big (Z)
          and then Big_R = Big (X) * Big (Y) rem Big (Z)
          and then Big_Q =
            (if Round then
               Round_Quotient (Big (X) * Big (Y), Big (Z), Quot, Big_R)
             else Quot)
          and then Big (Ru) = abs Big_R
          and then Big (Qu) = abs Big_Q
          and then In_Double_Int_Range (Big_Q)
          and then In_Double_Int_Range (Big_R)
          and then R =
            (if (X >= 0) = (Y >= 0) then To_Pos_Int (Ru) else To_Neg_Int (Ru))
          and then Q =
            (if ((X >= 0) = (Y >= 0)) = (Z >= 0) then To_Pos_Int (Qu)
             else To_Neg_Int (Qu)),  --  need to ensure To_Pos_Int precondition
        Post => Big (R) = Big_R and then Big (Q) = Big_Q;
      --  Proves final signs match the intended result after the unsigned
      --  division is done.

      procedure Prove_Z_Low
      with
        Ghost,
        Pre  => Z /= 0
          and then D'Initialized
          and then Hi (abs Z) = 0
          and then Lo (abs Z) = Zlo
          and then Mult =
            Big_2xxSingle * Big_2xxSingle * Big (Double_Uns (D (2)))
                          + Big_2xxSingle * Big (Double_Uns (D (3)))
                                          + Big (Double_Uns (D (4)))
          and then D (2) < Zlo
          and then Quot = (Big (X) * Big (Y)) / Big (Z)
          and then Big_R = (Big (X) * Big (Y)) rem Big (Z)
          and then T1 = D (2) & D (3)
          and then T2 = Lo (T1 rem Zlo) & D (4)
          and then Qu = Lo (T1 / Zlo) & Lo (T2 / Zlo)
          and then Ru = T2 rem Zlo,
        Post => Big (Qu) = abs Quot
          and then Big (Ru) = abs Big_R;
      --  Proves the case where the divisor is only one digit

      ----------------------------
      -- Prove_Dividend_Scaling --
      ----------------------------

      procedure Prove_Dividend_Scaling is
         Big_D12 : constant Big_Integer :=
           Big_2xx (Scale) * Big (D (1) & D (2));
         Big_T1  : constant Big_Integer := Big (T1);
         Big_D3  : constant Big_Integer :=
           Big_2xx (Scale) * Big (Double_Uns (D (3)));
         Big_T2  : constant Big_Integer := Big (T2);
         Big_D4  : constant Big_Integer :=
           Big_2xx (Scale) * Big (Double_Uns (D (4)));
         Big_T3  : constant Big_Integer := Big (T3);

      begin
         Lemma_Shift_Left (D (1) & D (2), Scale);
         Lemma_Ge_Mult (Big_2xxSingle, Big_2xx (Scale), Big_2xxSingle,
                        Big_2xxSingle * Big_2xx (Scale));
         Lemma_Lt_Mult (Big (Double_Uns (D (3))), Big_2xxSingle,
                        Big_2xx (Scale), Big_2xxDouble);
         Lemma_Shift_Left (Double_Uns (D (3)), Scale);
         Lemma_Lt_Mult (Big (Double_Uns (D (4))), Big_2xxSingle,
                        Big_2xx (Scale), Big_2xxDouble);
         Lemma_Shift_Left (Double_Uns (D (4)), Scale);
         Lemma_Hi_Lo (D (1) & D (2), D (1), D (2));
         pragma Assert (Mult * Big_2xx (Scale) =
           Big_2xxSingle * Big_2xxSingle * Big_D12
                         + Big_2xxSingle * Big_D3
                                         + Big_D4);
         pragma Assert (Big_2xx (Scale) > 0);
         declare
            Two_xx_Scale : constant Double_Uns := Double_Uns'(2 ** Scale);
            D12          : constant Double_Uns := D (1) & D (2);
         begin
            pragma Assert (Big_2xx (Scale) * Big (D12) < Big_2xxDouble);
            pragma Assert (Big (Two_xx_Scale) * Big (D12) < Big_2xxDouble);
            Lemma_Mult_Commutation (Two_xx_Scale, D12, T1);
         end;
         pragma Assert (Big_D12 = Big_T1);
         pragma Assert (Big_2xxSingle * Big_2xxSingle * Big_D12
                        = Big_2xxSingle * Big_2xxSingle * Big_T1);
         Lemma_Mult_Commutation (2 ** Scale, Double_Uns (D (3)), T2);
         pragma Assert (Big_D3 = Big_T2);
         pragma Assert (Big_2xxSingle * Big_D3 = Big_2xxSingle * Big_T2);
         Lemma_Mult_Commutation (2 ** Scale, Double_Uns (D (4)), T3);
         pragma Assert
           (Is_Scaled_Mult_Decomposition (0, Big_T1, Big_T2, Big_T3));
         Lemma_Hi_Lo (T1, Hi (T1), Lo (T1));
         Lemma_Hi_Lo (T2, Hi (T2), Lo (T2));
         Lemma_Hi_Lo (T3, Hi (T3), Lo (T3));
         Lemma_Mult_Distribution (Big_2xxSingle * Big_2xxSingle,
                                  Big_2xxSingle * Big (Double_Uns (Hi (T1))),
                                  Big (Double_Uns (Lo (T1))));
         Lemma_Mult_Distribution (Big_2xxSingle,
                                  Big_2xxSingle * Big (Double_Uns (Hi (T2))),
                                  Big (Double_Uns (Lo (T2))));
         Lemma_Mult_Distribution (Big_2xxSingle * Big_2xxSingle,
                                  Big (Double_Uns (Lo (T1))),
                                  Big (Double_Uns (Hi (T2))));
         Lemma_Mult_Distribution (Big_2xxSingle,
                                  Big (Double_Uns (Lo (T2))),
                                  Big (Double_Uns (Hi (T3))));
         Lemma_Mult_Distribution (Big_2xxSingle * Big_2xxSingle,
                                  Big (Double_Uns (Lo (T1))),
                                  Big (Double_Uns (Hi (T2))));
         pragma Assert (Double_Uns (Lo (T1) or Hi (T2)) =
                          Double_Uns (Lo (T1)) + Double_Uns (Hi (T2)));
         pragma Assert (Double_Uns (Lo (T2) or Hi (T3)) =
                          Double_Uns (Lo (T2)) + Double_Uns (Hi (T3)));
         Lemma_Add_Commutation (Double_Uns (Lo (T1)), Hi (T2));
         Lemma_Add_Commutation (Double_Uns (Lo (T2)), Hi (T3));
      end Prove_Dividend_Scaling;

      --------------------------
      -- Prove_Multiplication --
      --------------------------

      procedure Prove_Multiplication (Q : Single_Uns) is
      begin
         Lemma_Hi_Lo (Zu, Hi (Zu), Lo (Zu));
         Lemma_Hi_Lo (T1, Hi (T1), S3);
         Lemma_Hi_Lo (T2, Hi (T2), Lo (T2));
         Lemma_Hi_Lo (T3, Hi (T3), S2);
         Lemma_Mult_Commutation (Double_Uns (Q), Double_Uns (Lo (Zu)), T1);
         Lemma_Mult_Commutation (Double_Uns (Q), Double_Uns (Hi (Zu)), T2);
         Lemma_Mult_Distribution (Big (Double_Uns (Q)),
                                  Big_2xxSingle * Big (Double_Uns (Hi (Zu))),
                                  Big (Double_Uns (Lo (Zu))));
         Lemma_Substitution
           (Big (Double_Uns (Q)) * Big (Zu),
            Big (Double_Uns (Q)),
            Big (Zu),
            Big_2xxSingle * Big (Double_Uns (Hi (Zu)))
              + Big (Double_Uns (Lo (Zu))),
            Big_0);
         pragma Assert (Big (Double_Uns (Q)) * Big (Zu) =
           Big_2xxSingle * Big_2xxSingle * Big (Double_Uns (Hi (T2)))
                         + Big_2xxSingle * Big (Double_Uns (Lo (T2)))
                         + Big_2xxSingle * Big (Double_Uns (Hi (T1)))
                                         + Big (Double_Uns (S3)));
         Lemma_Add_Commutation (Double_Uns (Lo (T2)), Hi (T1));
         pragma Assert
           (By (Big (Double_Uns (Q)) * Big (Zu) =
              Big_2xxSingle * Big_2xxSingle * Big (Double_Uns (Hi (T2)))
                            + Big_2xxSingle * Big (T3)
                                            + Big (Double_Uns (S3)),
              Big_2xxSingle * Big (Double_Uns (Lo (T2)))
            + Big_2xxSingle * Big (Double_Uns (Hi (T1)))
            = Big_2xxSingle * Big (T3)));
         pragma Assert (Double_Uns (Hi (T3)) + Hi (T2) = Double_Uns (S1));
         Lemma_Add_Commutation (Double_Uns (Hi (T3)), Hi (T2));
         pragma Assert
           (Big (Double_Uns (Hi (T3))) + Big (Double_Uns (Hi (T2))) =
              Big (Double_Uns (S1)));
         Lemma_Mult_Distribution (Big_2xxSingle * Big_2xxSingle,
                                  Big (Double_Uns (Hi (T3))),
                                  Big (Double_Uns (Hi (T2))));
      end Prove_Multiplication;

      -------------------------------------
      -- Prove_Mult_Decomposition_Split3 --
      -------------------------------------

      procedure Prove_Mult_Decomposition_Split3
        (D1, D2, D3, D3_Hi, D3_Lo, D4 : Big_Integer)
      is null;

      -----------------------------
      -- Prove_Negative_Dividend --
      -----------------------------

      procedure Prove_Negative_Dividend is
      begin
         Lemma_Mult_Non_Positive (Big (X), Big (Y));
      end Prove_Negative_Dividend;

      --------------------
      -- Prove_Overflow --
      --------------------

      procedure Prove_Overflow is
      begin
         Lemma_Div_Ge (Mult, Big_2xxDouble, Big (Double_Uns'(abs Z)));
         Lemma_Abs_Commutation (Z);
         Lemma_Abs_Div_Commutation (Big (X) * Big (Y), Big (Z));
      end Prove_Overflow;

      -----------------------------
      -- Prove_Positive_Dividend --
      -----------------------------

      procedure Prove_Positive_Dividend is
      begin
         Lemma_Mult_Non_Negative (Big (X), Big (Y));
      end Prove_Positive_Dividend;

      ---------------------------------
      -- Prove_Qd_Calculation_Part_1 --
      ---------------------------------

      procedure Prove_Qd_Calculation_Part_1 (J : Integer) is
      begin
         Lemma_Hi_Lo (D (J) & D (J + 1), D (J), D (J + 1));
         Lemma_Lt_Commutation (Double_Uns (D (J)), Double_Uns (Zhi));
         Lemma_Gt_Mult (Big (Double_Uns (Zhi)),
                        Big (Double_Uns (D (J))) + 1,
                        Big_2xxSingle, Big (D (J) & D (J + 1)));
         Lemma_Div_Lt
           (Big (D (J) & D (J + 1)), Big_2xxSingle, Big (Double_Uns (Zhi)));
         Lemma_Div_Commutation (D (J) & D (J + 1), Double_Uns (Zhi));
         Lemma_Lo_Is_Ident ((D (J) & D (J + 1)) / Zhi);
         Lemma_Div_Definition (D (J) & D (J + 1), Zhi, Double_Uns (Qd (J)),
                               (D (J) & D (J + 1)) rem Zhi);
         Lemma_Lt_Commutation
           ((D (J) & D (J + 1)) rem Zhi, Double_Uns (Zhi));
         Lemma_Gt_Mult
           ((Big (Double_Uns (Qd (J))) + 1) * Big (Double_Uns (Zhi)),
            Big (D (J) & D (J + 1)) + 1, Big_2xxSingle,
            Big3 (D (J), D (J + 1), D (J + 2)));
         Lemma_Hi_Lo (Zu, Zhi, Lo (Zu));
         Lemma_Gt_Mult (Big (Zu), Big_2xxSingle * Big (Double_Uns (Zhi)),
                        Big (Double_Uns (Qd (J))) + 1,
                        Big3 (D (J), D (J + 1), D (J + 2)));
         Lemma_Div_Lt (Big3 (D (J), D (J + 1), D (J + 2)),
                       Big (Double_Uns (Qd (J))) + 1, Big (Zu));
      end Prove_Qd_Calculation_Part_1;

      ---------------------
      -- Prove_Q_Too_Big --
      ---------------------

      procedure Prove_Q_Too_Big is
      begin
         pragma Assert (Big_Q = Big_2xxDouble or Big_Q = -Big_2xxDouble);
         Lemma_Not_In_Range_Big2xx64;
      end Prove_Q_Too_Big;

      ---------------------
      -- Prove_Rescaling --
      ---------------------

      procedure Prove_Rescaling is
      begin
         Lemma_Div_Lt (Big (Ru), Big (Double_Uns'(abs Z)), Big_2xx (Scale));
         Lemma_Div_Eq (Mult, Big (Double_Uns'(abs Z)) * Big (Qu),
                       Big_2xx (Scale), Big (Ru));
         Lemma_Rev_Div_Definition (Mult, Big (Double_Uns'(abs Z)),
                                   Big (Qu), Big (Ru) / Big_2xx (Scale));
         Lemma_Abs_Div_Commutation (Big (X) * Big (Y), Big (Z));
         Lemma_Abs_Rem_Commutation (Big (X) * Big (Y), Big (Z));
         Lemma_Abs_Commutation (Z);
         Lemma_Shift_Right (Ru, Scale);
      end Prove_Rescaling;

      -------------------------
      -- Prove_Rounding_Case --
      -------------------------

      procedure Prove_Rounding_Case is
      begin
         if Same_Sign (Big (X) * Big (Y), Big (Z)) then
            null;
         end if;
      end Prove_Rounding_Case;

      -----------------------------------------------
      -- Prove_Scaled_Mult_Decomposition_Regroup24 --
      -----------------------------------------------

      procedure Prove_Scaled_Mult_Decomposition_Regroup24
        (D1, D2, D3, D4 : Big_Integer)
      is null;

      ----------------------------------------------
      -- Prove_Scaled_Mult_Decomposition_Regroup3 --
      ----------------------------------------------

      procedure Prove_Scaled_Mult_Decomposition_Regroup3
        (D1, D2, D3, D4 : Single_Uns)
      is null;

      ------------------
      -- Prove_Sign_R --
      ------------------

      procedure Prove_Sign_R is
      begin
         pragma Assert (In_Double_Int_Range (Big (Z)));
      end Prove_Sign_R;

      -----------------
      -- Prove_Signs --
      -----------------

      procedure Prove_Signs is null;

      -----------------
      -- Prove_Z_Low --
      -----------------

      procedure Prove_Z_Low is
      begin
         Lemma_Hi_Lo (T1, D (2), D (3));
         Lemma_Add_Commutation (Double_Uns (D (2)), 1);
         pragma Assert
           (Big (Double_Uns (D (2))) + 1 <= Big (Double_Uns (Zlo)));
         Lemma_Div_Definition (T1, Zlo, T1 / Zlo, T1 rem Zlo);
         pragma Assert (Double_Uns (Lo (T1 rem Zlo)) = T1 rem Zlo);
         Lemma_Hi_Lo (T2, Lo (T1 rem Zlo), D (4));
         pragma Assert (T1 rem Zlo < Double_Uns (Zlo));
         pragma Assert (T1 rem Zlo + Double_Uns'(1) <= Double_Uns (Zlo));
         Lemma_Ge_Commutation (Double_Uns (Zlo), T1 rem Zlo + Double_Uns'(1));
         Lemma_Add_Commutation (T1 rem Zlo, 1);
         pragma Assert (Big (T1 rem Zlo) + 1 <= Big (Double_Uns (Zlo)));
         Lemma_Div_Definition (T2, Zlo, T2 / Zlo, Ru);
         pragma Assert
           (Mult = Big (Double_Uns (Zlo)) *
              (Big_2xxSingle * Big (T1 / Zlo) + Big (T2 / Zlo)) + Big (Ru));
         pragma Assert (Big_2xxSingle * Big (Double_Uns (D (2)))
                                      + Big (Double_Uns (D (3)))
                        < Big_2xxSingle * (Big (Double_Uns (D (2))) + 1));
         Lemma_Div_Lt (Big (T1), Big_2xxSingle, Big (Double_Uns (Zlo)));
         Lemma_Div_Commutation (T1, Double_Uns (Zlo));
         Lemma_Lo_Is_Ident (T1 / Zlo);
         pragma Assert
           (Big (T2) <= Big_2xxSingle * (Big (Double_Uns (Zlo)) - 1)
                                      + Big (Double_Uns (D (4))));
         Lemma_Div_Lt (Big (T2), Big_2xxSingle, Big (Double_Uns (Zlo)));
         Lemma_Div_Commutation (T2, Double_Uns (Zlo));
         Lemma_Lo_Is_Ident (T2 / Zlo);
         Lemma_Hi_Lo (Qu, Lo (T1 / Zlo), Lo (T2 / Zlo));
         Lemma_Substitution (Mult, Big (Double_Uns (Zlo)),
                             Big_2xxSingle * Big (T1 / Zlo) + Big (T2 / Zlo),
                             Big (Qu), Big (Ru));
         Lemma_Lt_Commutation (Ru, Double_Uns (Zlo));
         Lemma_Rev_Div_Definition
           (Mult, Big (Double_Uns (Zlo)), Big (Qu), Big (Ru));
         pragma Assert (Double_Uns (Zlo) = abs Z);
         Lemma_Abs_Commutation (Z);
         Lemma_Abs_Div_Commutation (Big (X) * Big (Y), Big (Z));
         Lemma_Abs_Rem_Commutation (Big (X) * Big (Y), Big (Z));
      end Prove_Z_Low;

   --  Start of processing for Scaled_Divide

   begin
      if Z = 0 then
         Raise_Error;
      end if;

      Quot := Big (X) * Big (Y) / Big (Z);
      Big_R := Big (X) * Big (Y) rem Big (Z);
      if Round then
         Big_Q := Round_Quotient (Big (X) * Big (Y), Big (Z), Quot, Big_R);
      else
         Big_Q := Quot;
      end if;

      --  First do the multiplication, giving the four digit dividend

      Lemma_Abs_Mult_Commutation (Big (X), Big (Y));
      Lemma_Abs_Commutation (X);
      Lemma_Abs_Commutation (Y);
      Lemma_Mult_Decomposition (Mult, Xu, Yu, Xhi, Xlo, Yhi, Ylo);

      T1 := Xlo * Ylo;
      D (4) := Lo (T1);
      D (3) := Hi (T1);

      Lemma_Hi_Lo (T1, D (3), D (4));

      if Yhi /= 0 then
         T1 := Xlo * Yhi;

         Lemma_Hi_Lo (T1, Hi (T1), Lo (T1));

         T2 := D (3) + Lo (T1);

         Lemma_Add_Commutation (Double_Uns (Lo (T1)), D (3));
         Lemma_Mult_Distribution (Big_2xxSingle,
                                  Big (Double_Uns (D (3))),
                                  Big (Double_Uns (Lo (T1))));
         Lemma_Hi_Lo (T2, Hi (T2), Lo (T2));

         D (3) := Lo (T2);
         D (2) := Hi (T1) + Hi (T2);

         pragma Assert (Double_Uns (Hi (T1)) + Hi (T2) = Double_Uns (D (2)));
         Lemma_Add_Commutation (Double_Uns (Hi (T1)), Hi (T2));
         pragma Assert
           (Big (Double_Uns (Hi (T1))) + Big (Double_Uns (Hi (T2))) =
              Big (Double_Uns (D (2))));

         if Xhi /= 0 then
            T1 := Xhi * Ylo;

            Lemma_Hi_Lo (T1, Hi (T1), Lo (T1));

            T2 := D (3) + Lo (T1);

            Lemma_Add_Commutation (Double_Uns (D (3)), Lo (T1));
            Lemma_Hi_Lo (T2, Hi (T2), Lo (T2));
            Prove_Mult_Decomposition_Split3
              (D1    => 0,
               D2    => Big (Double_Uns'(Xhi * Yhi)) + Big (Double_Uns (D (2)))
                 + Big (Double_Uns (Hi (T1))),
               D3    => Big (T2),
               D3_Hi => Big (Double_Uns (Hi (T2))),
               D3_Lo => Big (Double_Uns (Lo (T2))),
               D4    => Big (Double_Uns (D (4))));

            D (3) := Lo (T2);
            T3 := D (2) + Hi (T1);

            Lemma_Add_Commutation (Double_Uns (D (2)), Hi (T1));
            Lemma_Add_Commutation (T3, Hi (T2));

            T3 := T3 + Hi (T2);
            T2 := Double_Uns'(Xhi * Yhi);

            Lemma_Hi_Lo (T2, Hi (T2), Lo (T2));

            T1 := T3 + Lo (T2);
            D (2) := Lo (T1);

            Lemma_Add_Commutation (T3, Lo (T2));
            Lemma_Hi_Lo (T1, Hi (T1), Lo (T1));

            D (1) := Hi (T2) + Hi (T1);

            pragma Assert
              (Double_Uns (Hi (T2)) + Hi (T1) = Double_Uns (D (1)));
            Lemma_Add_Commutation (Double_Uns (Hi (T2)), Hi (T1));
            pragma Assert
              (Big (Double_Uns (Hi (T2))) + Big (Double_Uns (Hi (T1))) =
                   Big (Double_Uns (D (1))));
            pragma Assert
              (Is_Mult_Decomposition (D1 => Big (Double_Uns (D (1))),
                                      D2 => Big (Double_Uns (D (2))),
                                      D3 => Big (Double_Uns (D (3))),
                                      D4 => Big (Double_Uns (D (4)))));
         else
            D (1) := 0;

            pragma Assert
              (Is_Mult_Decomposition (D1 => Big (Double_Uns (D (1))),
                                      D2 => Big (Double_Uns (D (2))),
                                      D3 => Big (Double_Uns (D (3))),
                                      D4 => Big (Double_Uns (D (4)))));
         end if;

      else
         if Xhi /= 0 then
            T1 := Xhi * Ylo;

            Lemma_Hi_Lo (T1, Hi (T1), Lo (T1));
            pragma Assert
              (Is_Mult_Decomposition
                 (D1 => 0,
                  D2 => Big (Double_Uns (Hi (T1))),
                  D3 => Big (Double_Uns (Lo (T1))) + Big (Double_Uns (D (3))),
                  D4 => Big (Double_Uns (D (4)))));

            T2 := D (3) + Lo (T1);

            Lemma_Add_Commutation (Double_Uns (Lo (T1)), D (3));
            pragma Assert
              (Is_Mult_Decomposition
                 (D1 => 0,
                  D2 => Big (Double_Uns (Hi (T1))),
                  D3 => Big (T2),
                  D4 => Big (Double_Uns (D (4)))));
            Lemma_Hi_Lo (T2, Hi (T2), Lo (T2));

            D (3) := Lo (T2);
            D (2) := Hi (T1) + Hi (T2);

            pragma Assert
              (Double_Uns (Hi (T1)) + Hi (T2) = Double_Uns (D (2)));
            Lemma_Add_Commutation (Double_Uns (Hi (T1)), Hi (T2));
            pragma Assert
              (Big (Double_Uns (Hi (T1))) + Big (Double_Uns (Hi (T2))) =
                 Big (Double_Uns (D (2))));
            pragma Assert
              (Is_Mult_Decomposition
                 (D1 => 0,
                  D2 => Big (Double_Uns (D (2))),
                  D3 => Big (Double_Uns (D (3))),
                  D4 => Big (Double_Uns (D (4)))));
         else
            D (2) := 0;

            pragma Assert
              (Is_Mult_Decomposition
                 (D1 => 0,
                  D2 => Big (Double_Uns (D (2))),
                  D3 => Big (Double_Uns (D (3))),
                  D4 => Big (Double_Uns (D (4)))));
         end if;

         D (1) := 0;
      end if;

      pragma Assert_And_Cut
        --  Restate the precondition
        (Z /= 0
         and then In_Double_Int_Range
           (if Round then Round_Quotient (Big (X) * Big (Y), Big (Z),
                                          Big (X) * Big (Y) / Big (Z),
                                          Big (X) * Big (Y) rem Big (Z))
            else Big (X) * Big (Y) / Big (Z))
         --  Restate the value of local variables
         and then Zu = abs Z
         and then Zhi = Hi (Zu)
         and then Zlo = Lo (Zu)
         and then Mult = abs (Big (X) * Big (Y))
         and then Quot = Big (X) * Big (Y) / Big (Z)
         and then Big_R = Big (X) * Big (Y) rem Big (Z)
         and then
           (if Round then
              Big_Q = Round_Quotient (Big (X) * Big (Y), Big (Z), Quot, Big_R)
            else
              Big_Q = Quot)
         --  Summarize first part of the procedure
         and then D'Initialized
         and then Is_Mult_Decomposition (D1 => Big (Double_Uns (D (1))),
                                         D2 => Big (Double_Uns (D (2))),
                                         D3 => Big (Double_Uns (D (3))),
                                         D4 => Big (Double_Uns (D (4)))));

      --  Now it is time for the dreaded multiple precision division. First an
      --  easy case, check for the simple case of a one digit divisor.

      if Zhi = 0 then
         if D (1) /= 0 or else D (2) >= Zlo then
            if D (1) > 0 then
               Lemma_Double_Big_2xxSingle;
               Lemma_Mult_Positive (Big_2xxDouble, Big_2xxSingle);
               Lemma_Ge_Mult (Big (Double_Uns (D (1))),
                              1,
                              Big_2xxDouble * Big_2xxSingle,
                              Big_2xxDouble * Big_2xxSingle);
               Lemma_Mult_Positive (Big_2xxSingle, Big (Double_Uns (D (1))));
               Lemma_Ge_Mult (Big_2xxSingle * Big_2xxSingle, Big_2xxDouble,
                              Big_2xxSingle * Big (Double_Uns (D (1))),
                              Big_2xxDouble * Big_2xxSingle);
               pragma Assert (Mult >= Big_2xxDouble * Big_2xxSingle);
               Lemma_Ge_Commutation (2 ** Single_Size, Zu);
               Lemma_Ge_Mult (Big_2xxSingle, Big (Zu), Big_2xxDouble,
                              Big_2xxDouble * Big (Zu));
               pragma Assert (Mult >= Big_2xxDouble * Big (Zu));
            else
               Lemma_Ge_Commutation (Double_Uns (D (2)), Zu);
               pragma Assert (Mult >= Big_2xxDouble * Big (Zu));
            end if;

            Prove_Overflow;
            Raise_Error;

         --  Here we are dividing at most three digits by one digit

         else
            T1 := D (2) & D (3);
            T2 := Lo (T1 rem Zlo) & D (4);

            Qu := Lo (T1 / Zlo) & Lo (T2 / Zlo);
            Ru := T2 rem Zlo;

            Prove_Z_Low;
         end if;

      --  If divisor is double digit and dividend is too large, raise error

      elsif (D (1) & D (2)) >= Zu then
         Lemma_Hi_Lo (D (1) & D (2), D (1), D (2));
         Lemma_Ge_Commutation (D (1) & D (2), Zu);
         pragma Assert
           (Mult >= Big_2xxSingle * Big_2xxSingle * Big (D (1) & D (2)));
         Prove_Overflow;
         Raise_Error;

      --  This is the complex case where we definitely have a double digit
      --  divisor and a dividend of at least three digits. We use the classical
      --  multiple-precision division algorithm (see section (4.3.1) of Knuth's
      --  "The Art of Computer Programming", Vol. 2 for a description
      --  (algorithm D).

      else
         --  First normalize the divisor so that it has the leading bit on.
         --  We do this by finding the appropriate left shift amount.

         Lemma_Hi_Lo (D (1) & D (2), D (1), D (2));
         Lemma_Lt_Commutation (D (1) & D (2), Zu);
         pragma Assert
           (Mult < Big_2xxDouble * Big (Zu));

         Shift := Single_Size;
         Mask  := Single_Uns'Last;
         Scale := 0;

         Inter := 0;
         pragma Assert (Big_2xx (Scale) = 1);

         while Shift > 1 loop
            pragma Loop_Invariant (Scale <= Single_Size - Shift);
            pragma Loop_Invariant ((Hi (Zu) and Mask) /= 0);
            pragma Loop_Invariant
              (Mask = Shift_Left (Single_Uns'Last, Single_Size - Shift));
            pragma Loop_Invariant (Zu = Shift_Left (abs Z, Scale));
            pragma Loop_Invariant (Big (Zu) =
              Big (Double_Uns'(abs Z)) * Big_2xx (Scale));
            pragma Loop_Invariant (Inter in 0 .. Log_Single_Size - 1);
            pragma Loop_Invariant (Shift = 2 ** (Log_Single_Size - Inter));
            pragma Loop_Invariant (Shift mod 2 = 0);

            declare
               --  Local ghost variables

               Shift_Prev : constant Natural := Shift with Ghost;
               Mask_Prev  : constant Single_Uns := Mask with Ghost;
               Zu_Prev    : constant Double_Uns := Zu with Ghost;

               --  Local lemmas

               procedure Prove_Power
               with
                 Ghost,
                 Pre  => Inter in 0 .. Log_Single_Size - 1
                   and then Shift = 2 ** (Log_Single_Size - Inter),
                 Post => Shift / 2 = 2 ** (Log_Single_Size - (Inter + 1))
                   and then (Shift = 2 or (Shift / 2) mod 2 = 0);

               procedure Prove_Prev_And_Mask (Prev, Mask : Single_Uns)
               with
                 Ghost,
                 Pre  => Prev /= 0
                   and then (Prev and Mask) = 0,
                 Post => (Prev and not Mask) /= 0;

               procedure Prove_Shift_Progress
               with
                 Ghost,
                 Pre  => Shift <= Single_Size / 2
                   and then Shift_Prev = 2 * Shift
                   and then Mask_Prev =
                     Shift_Left (Single_Uns'Last, Single_Size - Shift_Prev)
                   and then Mask =
                     Shift_Left (Single_Uns'Last,
                                 Single_Size - Shift_Prev + Shift),
                 Post => Mask_Prev =
                     Shift_Left (Single_Uns'Last, Single_Size - 2 * Shift)
                   and then Mask =
                     Shift_Left (Single_Uns'Last, Single_Size - Shift);

               procedure Prove_Shifting
               with
                 Ghost,
                 Pre  => Shift <= Single_Size / 2
                   and then Zu = Shift_Left (Zu_Prev, Shift)
                   and then Mask_Prev =
                     Shift_Left (Single_Uns'Last, Single_Size - 2 * Shift)
                   and then Mask =
                     Shift_Left (Single_Uns'Last, Single_Size - Shift)
                   and then (Hi (Zu_Prev) and Mask_Prev and not Mask) /= 0,
                 Post => (Hi (Zu) and Mask) /= 0;

               -----------------------------
               -- Local lemma null bodies --
               -----------------------------

               procedure Prove_Prev_And_Mask (Prev, Mask : Single_Uns) is null;
               procedure Prove_Power is null;
               procedure Prove_Shifting is null;
               procedure Prove_Shift_Progress is null;

            begin
               pragma Assert (Mask = Shift_Left (Single_Uns'Last,
                              Single_Size - Shift_Prev));
               Prove_Power;

               Shift := Shift / 2;

               Inter := Inter + 1;
               pragma Assert (Shift_Prev = 2 * Shift);

               Mask := Shift_Left (Mask, Shift);

               Lemma_Double_Shift
                 (Single_Uns'Last, Single_Size - Shift_Prev, Shift);
               Prove_Shift_Progress;

               if (Hi (Zu) and Mask) = 0 then
                  Zu := Shift_Left (Zu, Shift);

                  pragma Assert ((Hi (Zu_Prev) and Mask_Prev) /= 0);
                  pragma Assert
                    (By ((Hi (Zu_Prev) and Mask_Prev and Mask) = 0,
                     (Hi (Zu_Prev) and Mask) = 0
                     and then
                     (Hi (Zu_Prev) and Mask_Prev and Mask)
                     = (Hi (Zu_Prev) and Mask and Mask_Prev)
                    ));
                  Prove_Prev_And_Mask (Hi (Zu_Prev) and Mask_Prev, Mask);
                  Prove_Shifting;
                  pragma Assert (Big (Zu_Prev) =
                    Big (Double_Uns'(abs Z)) * Big_2xx (Scale));
                  Lemma_Shift_Without_Drop (Zu_Prev, Zu, Mask, Shift);
                  Lemma_Substitution
                    (Big (Zu), Big_2xx (Shift),
                     Big (Zu_Prev), Big (Double_Uns'(abs Z)) * Big_2xx (Scale),
                     0);
                  Lemma_Powers_Of_2 (Shift, Scale);
                  Lemma_Substitution
                    (Big (Zu), Big (Double_Uns'(abs Z)),
                     Big_2xx (Shift) * Big_2xx (Scale),
                     Big_2xx (Shift + Scale), 0);
                  Lemma_Double_Shift (abs Z, Scale, Shift);

                  Scale := Scale + Shift;

                  pragma Assert (Zu = Shift_Left (abs Z, Scale));
                  pragma Assert
                    (Big (Zu) = Big (Double_Uns'(abs Z)) * Big_2xx (Scale));
               end if;

               pragma Assert
                 (Big (Zu) = Big (Double_Uns'(abs Z)) * Big_2xx (Scale));
            end;
         end loop;

         Zhi := Hi (Zu);
         Zlo := Lo (Zu);

         pragma Assert (Shift = 1);
         pragma Assert (Mask = Shift_Left (Single_Uns'Last, Single_Size - 1));
         pragma Assert ((Zhi and Mask) /= 0);
         pragma Assert (Zhi >= 2 ** (Single_Size - 1));
         pragma Assert (Big (Zu) = Big (Double_Uns'(abs Z)) * Big_2xx (Scale));
         --  We have Hi (Zu) /= 0 before normalization. The sequence of
         --  Shift_Left operations results in the leading bit of Zu being 1 by
         --  moving the leftmost 1-bit in Zu to leading position, thus
         --  Zhi = Hi (Zu) >= 2 ** (Single_Size - 1) here.

         --  Note that when we scale up the dividend, it still fits in four
         --  digits, since we already tested for overflow, and scaling does
         --  not change the invariant that (D (1) & D (2)) < Zu.

         Lemma_Lt_Commutation (D (1) & D (2), abs Z);
         Lemma_Big_Of_Double_Uns (Zu);
         Lemma_Lt_Mult (Big (D (1) & D (2)),
                        Big (Double_Uns'(abs Z)), Big_2xx (Scale),
                        Big_2xxDouble);

         T1 := Shift_Left (D (1) & D (2), Scale);
         T2 := Shift_Left (Double_Uns (D (3)), Scale);
         T3 := Shift_Left (Double_Uns (D (4)), Scale);

         Prove_Dividend_Scaling;

         D (1) := Hi (T1);
         D (2) := Lo (T1) or Hi (T2);
         D (3) := Lo (T2) or Hi (T3);
         D (4) := Lo (T3);

         pragma Assert (Big (Double_Uns (Hi (T1))) = Big (Double_Uns (D (1))));
         pragma Assert
           (Big_2xxSingle * Big_2xxSingle * Big_2xxSingle
            * Big (Double_Uns (Hi (T1)))
            = Big_2xxSingle * Big_2xxSingle * Big_2xxSingle
            * Big (Double_Uns (D (1))));
         Lemma_Substitution (Big_2xxDouble * Big (Zu), Big_2xxDouble, Big (Zu),
                             Big (Double_Uns'(abs Z)) * Big_2xx (Scale), 0);
         Lemma_Lt_Mult (Mult, Big_2xxDouble * Big (Double_Uns'(abs Z)),
                        Big_2xx (Scale), Big_2xxDouble * Big (Zu));
         pragma Assert (Mult >= Big_0);
         pragma Assert (Big_2xx (Scale) >= Big_0);
         Lemma_Mult_Non_Negative (Mult, Big_2xx (Scale));
         Lemma_Div_Lt (Mult * Big_2xx (Scale), Big (Zu), Big_2xxDouble);
         Lemma_Concat_Definition (D (1), D (2));
         Lemma_Double_Big_2xxSingle;
         Prove_Scaled_Mult_Decomposition_Regroup24
           (Big (Double_Uns (D (1))),
            Big (Double_Uns (D (2))),
            Big (Double_Uns (D (3))),
            Big (Double_Uns (D (4))));
         Lemma_Substitution
           (Mult * Big_2xx (Scale), Big_2xxSingle * Big_2xxSingle,
              Big_2xxSingle * Big (Double_Uns (D (1)))
                            + Big (Double_Uns (D (2))),
              Big (D (1) & D (2)),
              Big_2xxSingle * Big (Double_Uns (D (3)))
                            + Big (Double_Uns (D (4))));
         pragma Assert
           (By (Big (D (1) & D (2)) < Big (Zu),
            Big_2xxDouble * (Big (Zu) - Big (D (1) & D (2))) >
              Big_2xxSingle * Big (Double_Uns (D (3)))
            + Big (Double_Uns (D (4)))));

         --  Loop to compute quotient digits, runs twice for Qd (1) and Qd (2)

         declare
            --  Local lemmas

            procedure Prove_First_Iteration (X1, X2, X3, X4 : Single_Uns)
            with
              Ghost,
              Pre  => X1 = 0,
              Post =>
                Big_2xxSingle * Big3 (X1, X2, X3) + Big (Double_Uns (X4))
                  = Big3 (X2, X3, X4);

            ---------------------------
            -- Prove_First_Iteration --
            ---------------------------

            procedure Prove_First_Iteration (X1, X2, X3, X4 : Single_Uns) is
            null;

            --  Local ghost variables

            Qd1  : Single_Uns := 0 with Ghost;
            D234 : Big_Integer with Ghost, Relaxed_Initialization;
            D123 : constant Big_Integer := Big3 (D (1), D (2), D (3))
              with Ghost;
            D4   : constant Big_Integer := Big (Double_Uns (D (4)))
              with Ghost;

         begin
            Prove_Scaled_Mult_Decomposition_Regroup3
              (D (1), D (2), D (3), D (4));
            pragma Assert
              (By (Mult * Big_2xx (Scale) = Big_2xxSingle * D123 + D4,
               Is_Scaled_Mult_Decomposition (0, 0, D123, D4)));

            for J in 1 .. 2 loop
               Lemma_Hi_Lo (D (J) & D (J + 1), D (J), D (J + 1));
               pragma Assert (Big (D (J) & D (J + 1)) < Big (Zu));

               --  Compute next quotient digit. We have to divide three digits
               --  by two digits. We estimate the quotient by dividing the
               --  leading two digits by the leading digit. Given the scaling
               --  we did above which ensured the first bit of the divisor is
               --  set, this gives an estimate of the quotient that is at most
               --  two too high.

               if D (J) > Zhi then
                  Lemma_Lt_Commutation (Zu, D (J) & D (J + 1));
                  pragma Assert (False);

               elsif D (J) = Zhi then
                  Qd (J) := Single_Uns'Last;

                  Lemma_Concat_Definition (D (J), D (J + 1));
                  Lemma_Big_Of_Double_Uns_Of_Single_Uns (D (J + 2));
                  pragma Assert (Big_2xxSingle > Big (Double_Uns (D (J + 2))));
                  pragma Assert (Big3 (D (J), D (J + 1), 0) + Big_2xxSingle
                                 > Big3 (D (J), D (J + 1), D (J + 2)));
                  pragma Assert (Big (Double_Uns'(0)) = 0);
                  pragma Assert (Big (D (J) & D (J + 1)) * Big_2xxSingle =
                    Big_2xxSingle * (Big_2xxSingle * Big (Double_Uns (D (J)))
                                              + Big (Double_Uns (D (J + 1)))));
                  pragma Assert (Big (D (J) & D (J + 1)) * Big_2xxSingle =
                    Big_2xxSingle * Big_2xxSingle * Big (Double_Uns (D (J)))
                               + Big_2xxSingle * Big (Double_Uns (D (J + 1))));
                  pragma Assert (Big (D (J) & D (J + 1)) * Big_2xxSingle
                                 = Big3 (D (J), D (J + 1), 0));
                  pragma Assert ((Big (D (J) & D (J + 1)) + 1) * Big_2xxSingle
                                 = Big3 (D (J), D (J + 1), 0) + Big_2xxSingle);
                  Lemma_Gt_Mult (Big (Zu), Big (D (J) & D (J + 1)) + 1,
                                 Big_2xxSingle,
                                 Big3 (D (J), D (J + 1), D (J + 2)));
                  Lemma_Div_Lt
                    (Big3 (D (J), D (J + 1), D (J + 2)),
                     Big_2xxSingle, Big (Zu));
                  pragma Assert
                    (By (Big (Double_Uns (Qd (J))) >=
                       Big3 (D (J), D (J + 1), D (J + 2)) / Big (Zu),
                     Big (Double_Uns (Qd (J))) = Big_2xxSingle - 1));

               else
                  Qd (J) := Lo ((D (J) & D (J + 1)) / Zhi);

                  Prove_Qd_Calculation_Part_1 (J);
               end if;

               pragma Assert (for all K in 1 .. J => Qd (K)'Initialized);
               Lemma_Div_Mult (Big3 (D (J), D (J + 1), D (J + 2)), Big (Zu));
               Lemma_Gt_Mult
                 (Big (Double_Uns (Qd (J))),
                  Big3 (D (J), D (J + 1), D (J + 2)) / Big (Zu),
                  Big (Zu), Big3 (D (J), D (J + 1), D (J + 2)) - Big (Zu));

               --  Compute amount to subtract

               T1 := Qd (J) * Zlo;
               T2 := Qd (J) * Zhi;
               S3 := Lo (T1);
               T3 := Hi (T1) + Lo (T2);
               S2 := Lo (T3);
               S1 := Hi (T3) + Hi (T2);

               Prove_Multiplication (Qd (J));

               --  Adjust quotient digit if it was too high

               --  We use the version of the algorithm in the 2nd Edition
               --  of "The Art of Computer Programming". This had a bug not
               --  discovered till 1995, see Vol 2 errata:
               --     http://www-cs-faculty.stanford.edu/~uno/err2-2e.ps.gz.
               --  Under rare circumstances the expression in the test could
               --  overflow. This version was further corrected in 2005, see
               --  Vol 2 errata:
               --     http://www-cs-faculty.stanford.edu/~uno/all2-pre.ps.gz.
               --  This implementation is not impacted by these bugs, due
               --  to the use of a word-size comparison done in function Le3
               --  instead of a comparison on two-word integer quantities in
               --  the original algorithm.

               Lemma_Hi_Lo_3 (Zu, Zhi, Zlo);

               while not Le3 (S1, S2, S3, D (J), D (J + 1), D (J + 2)) loop
                  pragma Loop_Invariant
                    (for all K in 1 .. J => Qd (K)'Initialized);
                  pragma Loop_Invariant (if J = 2 then Qd (1) = Qd1);
                  pragma Loop_Invariant
                    (Big3 (S1, S2, S3) = Big (Double_Uns (Qd (J))) * Big (Zu));
                  pragma Loop_Invariant
                    (Big3 (S1, S2, S3) > Big3 (D (J), D (J + 1), D (J + 2)));
                  pragma Assert (Big3 (S1, S2, S3) > 0);
                  if Qd (J) = 0 then
                     pragma Assert (Big3 (S1, S2, S3) = 0);
                     pragma Assert (False);
                  end if;
                  Lemma_Ge_Commutation (Double_Uns (Qd (J)), 1);
                  Lemma_Ge_Mult
                    (Big (Double_Uns (Qd (J))), 1, Big (Zu), Big (Zu));

                  Sub3 (S1, S2, S3, 0, Zhi, Zlo);

                  pragma Assert
                    (Big3 (S1, S2, S3) >
                       Big3 (D (J), D (J + 1), D (J + 2)) - Big (Zu));
                  Lemma_Subtract_Commutation (Double_Uns (Qd (J)), 1);
                  pragma Assert (Double_Uns (Qd (J)) - Double_Uns'(1)
                                 = Double_Uns (Qd (J) - 1));
                  pragma Assert (Big (Double_Uns'(1)) = 1);
                  Lemma_Substitution (Big3 (S1, S2, S3), Big (Zu),
                                      Big (Double_Uns (Qd (J))) - 1,
                                      Big (Double_Uns (Qd (J) - 1)), 0);

                  declare
                     Prev : constant Single_Uns := Qd (J) - 1 with Ghost;
                  begin
                     Qd (J) := Qd (J) - 1;

                     pragma Assert (Qd (J) = Prev);
                     pragma Assert (Qd (J)'Initialized);
                     if J = 2 then
                        pragma Assert (Qd (J - 1)'Initialized);
                     end if;
                     pragma Assert (for all K in 1 .. J => Qd (K)'Initialized);
                  end;

                  pragma Assert
                    (Big3 (S1, S2, S3) = Big (Double_Uns (Qd (J))) * Big (Zu));
               end loop;

               --  Now subtract S1&S2&S3 from D1&D2&D3 ready for next step

               pragma Assert (for all K in 1 .. J => Qd (K)'Initialized);
               pragma Assert
                 (Big3 (S1, S2, S3) = Big (Double_Uns (Qd (J))) * Big (Zu));
               pragma Assert (Big3 (S1, S2, S3) >
                                Big3 (D (J), D (J + 1), D (J + 2)) - Big (Zu));
               Inline_Le3 (S1, S2, S3, D (J), D (J + 1), D (J + 2));

               Sub3 (D (J), D (J + 1), D (J + 2), S1, S2, S3);

               pragma Assert (Big3 (D (J), D (J + 1), D (J + 2)) < Big (Zu));
               if D (J) > 0 then
                  Lemma_Double_Big_2xxSingle;
                  pragma Assert (Big3 (D (J), D (J + 1), D (J + 2)) =
                    Big_2xxSingle
                      * Big_2xxSingle * Big (Double_Uns (D (J)))
                      + Big_2xxSingle * Big (Double_Uns (D (J + 1)))
                                      + Big (Double_Uns (D (J + 2))));
                  pragma Assert (Big_2xxSingle >= 0);
                  Lemma_Big_Of_Double_Uns_Of_Single_Uns (D (J + 1));
                  pragma Assert (Big (Double_Uns (D (J + 1))) >= 0);
                  Lemma_Mult_Non_Negative
                    (Big_2xxSingle, Big (Double_Uns (D (J + 1))));
                  pragma Assert
                    (Big3 (D (J), D (J + 1), D (J + 2)) >=
                       Big_2xxSingle * Big_2xxSingle
                       * Big (Double_Uns (D (J))));
                  Lemma_Ge_Commutation (Double_Uns (D (J)), Double_Uns'(1));
                  Lemma_Ge_Mult (Big (Double_Uns (D (J))),
                                 Big (Double_Uns'(1)),
                                 Big_2xxDouble,
                                 Big (Double_Uns'(1)) * Big_2xxDouble);
                  pragma Assert
                    (Big_2xxDouble * Big (Double_Uns'(1)) = Big_2xxDouble);
                  pragma Assert
                    (Big3 (D (J), D (J + 1), D (J + 2)) >= Big_2xxDouble);
                  pragma Assert (False);
               end if;

               if J = 1 then
                  Qd1 := Qd (1);
                  D234 := Big3 (D (2), D (3), D (4));
                  pragma Assert (D4 = Big (Double_Uns (D (4))));
                  Lemma_Substitution
                    (Mult * Big_2xx (Scale), Big_2xxSingle, D123,
                     Big3 (D (1), D (2), D (3)) + Big3 (S1, S2, S3),
                     Big (Double_Uns (D (4))));
                  Prove_First_Iteration (D (1), D (2), D (3), D (4));
                  Lemma_Substitution (Mult * Big_2xx (Scale), Big_2xxSingle,
                                      Big3 (S1, S2, S3),
                                      Big (Double_Uns (Qd1)) * Big (Zu),
                                      D234);
               else
                  pragma Assert (Qd1 = Qd (1));
                  pragma Assert
                    (By (Mult * Big_2xx (Scale) =
                       Big_2xxSingle * Big (Double_Uns (Qd (1))) * Big (Zu)
                                     + Big (Double_Uns (Qd (2))) * Big (Zu)
                     + Big_2xxSingle * Big (Double_Uns (D (3)))
                                     + Big (Double_Uns (D (4))),
                     By (Mult * Big_2xx (Scale) =
                       Big_2xxSingle * Big (Double_Uns (Qd (1))) * Big (Zu)
                       + Big3 (D (2), D (3), D (4)) + Big3 (S1, S2, S3),
                     Mult * Big_2xx (Scale) =
                       Big_2xxSingle * Big (Double_Uns (Qd (1))) * Big (Zu)
                       + D234)));

               end if;
            end loop;
         end;

         --  The two quotient digits are now set, and the remainder of the
         --  scaled division is in D3&D4. To get the remainder for the
         --  original unscaled division, we rescale this dividend.

         --  We rescale the divisor as well, to make the proper comparison
         --  for rounding below.

         pragma Assert (for all K in 1 .. 2 => Qd (K)'Initialized);
         Qu := Qd (1) & Qd (2);
         Ru := D (3) & D (4);

         pragma Assert
           (Mult * Big_2xx (Scale) =
              Big_2xxSingle * Big (Double_Uns (Qd (1))) * Big (Zu)
                            + Big (Double_Uns (Qd (2))) * Big (Zu)
            + Big_2xxSingle * Big (Double_Uns (D (3)))
                            + Big (Double_Uns (D (4))));
         Lemma_Hi_Lo (Qu, Qd (1), Qd (2));
         Lemma_Hi_Lo (Ru, D (3), D (4));
         Lemma_Substitution
           (Mult * Big_2xx (Scale), Big (Zu),
            Big_2xxSingle * Big (Double_Uns (Qd (1)))
              + Big (Double_Uns (Qd (2))),
            Big (Qu), Big (Ru));
         Prove_Rescaling;

         Ru := Shift_Right (Ru, Scale);

         declare
            --  Local lemma required to help automatic provers
            procedure Lemma_Div_Congruent
              (X, Y : Big_Natural;
               Z    : Big_Positive)
            with
              Ghost,
              Pre  => X = Y,
              Post => X / Z = Y / Z;

            procedure Lemma_Div_Congruent
              (X, Y : Big_Natural;
               Z    : Big_Positive)
            is null;

         begin
            Lemma_Shift_Right (Zu, Scale);
            Lemma_Div_Congruent (Big (Zu),
                                 Big (Double_Uns'(abs Z)) * Big_2xx (Scale),
                                 Big_2xx (Scale));

            Zu := Shift_Right (Zu, Scale);

            Lemma_Simplify (Big (Double_Uns'(abs Z)), Big_2xx (Scale));
            pragma Assert (Big (Zu) = Big (Double_Uns'(abs Z)));
         end;
      end if;

      pragma Assert (Big (Ru) = abs Big_R);
      pragma Assert (Big (Qu) = abs Quot);
      pragma Assert (Big (Zu) = Big (Double_Uns'(abs Z)));

      --  Deal with rounding case

      if Round then
         Prove_Rounding_Case;

         if Ru > (Zu - Double_Uns'(1)) / Double_Uns'(2) then
            pragma Assert (abs Big_Q = Big (Qu) + 1);

            --  Protect against wrapping around when rounding, by signaling
            --  an overflow when the quotient is too large.

            if Qu = Double_Uns'Last then
               Prove_Q_Too_Big;
               Raise_Error;
            end if;

            Lemma_Add_One (Qu);

            Qu := Qu + Double_Uns'(1);
         end if;
      end if;

      pragma Assert (Big (Qu) = abs Big_Q);

      --  Set final signs (RM 4.5.5(27-30))

      --  Case of dividend (X * Y) sign positive

      if (X >= 0 and then Y >= 0) or else (X < 0 and then Y < 0) then
         Prove_Positive_Dividend;

         R := To_Pos_Int (Ru);
         Q := (if Z > 0 then To_Pos_Int (Qu) else To_Neg_Int (Qu));

      --  Case of dividend (X * Y) sign negative

      else
         Prove_Negative_Dividend;

         R := To_Neg_Int (Ru);
         Q := (if Z > 0 then To_Neg_Int (Qu) else To_Pos_Int (Qu));
      end if;

      Prove_Sign_R;
      Prove_Signs;
   end Scaled_Divide;
   pragma Annotate (Gnatcheck, Exempt_Off, "Metrics_Cyclomatic_Complexity");

   ----------
   -- Sub3 --
   ----------

   procedure Sub3 (X1, X2, X3 : in out Single_Uns; Y1, Y2, Y3 : Single_Uns) is

      --  Local ghost variables

      XX1 : constant Single_Uns := X1 with Ghost;
      XX2 : constant Single_Uns := X2 with Ghost;
      XX3 : constant Single_Uns := X3 with Ghost;

      --  Local lemmas

      procedure Lemma_Add3_No_Carry (X1, X2, X3, Y1, Y2, Y3 : Single_Uns)
      with
        Ghost,
        Pre  => X1 <= Single_Uns'Last - Y1
          and then X2 <= Single_Uns'Last - Y2
          and then X3 <= Single_Uns'Last - Y3,
        Post => Big3 (X1 + Y1, X2 + Y2, X3 + Y3)
              = Big3 (X1, X2, X3) + Big3 (Y1, Y2, Y3);

      procedure Lemma_Ge_Expand (X1, X2, X3, Y1, Y2, Y3 : Single_Uns)
      with
        Ghost,
        Pre  => Big3 (X1, X2, X3) >= Big3 (Y1, Y2, Y3),
        Post => X1 > Y1
          or else (X1 = Y1 and then X2 > Y2)
          or else (X1 = Y1 and then X2 = Y2 and then X3 >= Y3);

      procedure Lemma_Sub3_No_Carry (X1, X2, X3, Y1, Y2, Y3 : Single_Uns)
      with
        Ghost,
        Pre  => X1 >= Y1 and then X2 >= Y2 and then X3 >= Y3,
        Post => Big3 (X1 - Y1, X2 - Y2, X3 - Y3)
              = Big3 (X1, X2, X3) - Big3 (Y1, Y2, Y3);

      procedure Lemma_Sub3_With_Carry2 (X1, X2, X3, Y2 : Single_Uns)
      with
        Ghost,
        Pre  => X2 < Y2,
        Post => Big3 (X1, X2 - Y2, X3)
          = Big3 (X1, X2, X3) + Big3 (Single_Uns'(1), 0, 0) - Big3 (0, Y2, 0);

      procedure Lemma_Sub3_With_Carry3 (X1, X2, X3, Y3 : Single_Uns)
      with
        Ghost,
        Pre  => X3 < Y3,
        Post => Big3 (X1, X2, X3 - Y3)
          = Big3 (X1, X2, X3) + Big3 (Single_Uns'(0), 1, 0) - Big3 (0, 0, Y3);

      -------------------------
      -- Lemma_Add3_No_Carry --
      -------------------------

      procedure Lemma_Add3_No_Carry (X1, X2, X3, Y1, Y2, Y3 : Single_Uns) is
      begin
         Lemma_Add_Commutation (Double_Uns (X1), Y1);
         Lemma_Add_Commutation (Double_Uns (X2), Y2);
         Lemma_Add_Commutation (Double_Uns (X3), Y3);
      end Lemma_Add3_No_Carry;

      ---------------------
      -- Lemma_Ge_Expand --
      ---------------------

      procedure Lemma_Ge_Expand (X1, X2, X3, Y1, Y2, Y3 : Single_Uns) is null;

      -------------------------
      -- Lemma_Sub3_No_Carry --
      -------------------------

      procedure Lemma_Sub3_No_Carry (X1, X2, X3, Y1, Y2, Y3 : Single_Uns) is
      begin
         Lemma_Subtract_Commutation (Double_Uns (X1), Double_Uns (Y1));
         Lemma_Subtract_Commutation (Double_Uns (X2), Double_Uns (Y2));
         Lemma_Subtract_Commutation (Double_Uns (X3), Double_Uns (Y3));
      end Lemma_Sub3_No_Carry;

      ----------------------------
      -- Lemma_Sub3_With_Carry2 --
      ----------------------------

      procedure Lemma_Sub3_With_Carry2 (X1, X2, X3, Y2 : Single_Uns) is
         pragma Unreferenced (X1, X3);
      begin
         Lemma_Add_Commutation
           (Double_Uns'(2 ** Single_Size) - Double_Uns (Y2), X2);
         Lemma_Subtract_Commutation
           (Double_Uns'(2 ** Single_Size), Double_Uns (Y2));
      end Lemma_Sub3_With_Carry2;

      ----------------------------
      -- Lemma_Sub3_With_Carry3 --
      ----------------------------

      procedure Lemma_Sub3_With_Carry3 (X1, X2, X3, Y3 : Single_Uns) is
         pragma Unreferenced (X1, X2);
      begin
         Lemma_Add_Commutation
           (Double_Uns'(2 ** Single_Size) - Double_Uns (Y3), X3);
         Lemma_Subtract_Commutation
           (Double_Uns'(2 ** Single_Size), Double_Uns (Y3));
      end Lemma_Sub3_With_Carry3;

   --  Start of processing for Sub3

   begin
      Lemma_Ge_Expand (X1, X2, X3, Y1, Y2, Y3);

      if Y3 > X3 then
         if X2 = 0 then
            pragma Assert (X1 >= 1);
            Lemma_Sub3_No_Carry (X1, X2, X3, 1, 0, 0);

            X1 := X1 - 1;

            pragma Assert
              (Big3 (X1, X2, X3) =
                 Big3 (XX1, XX2, XX3) - Big3 (Single_Uns'(1), 0, 0));
            pragma Assert
              (Big3 (X1, X2, X3) = Big3 (XX1, XX2, XX3)
               - Big3 (Single_Uns'(0), Single_Uns'Last, 0)
               - Big3 (Single_Uns'(0), 1, 0));
            Lemma_Add3_No_Carry (X1, X2, X3, 0, Single_Uns'Last, 0);
         else
            Lemma_Sub3_No_Carry (X1, X2, X3, 0, 1, 0);
         end if;

         X2 := X2 - 1;

         pragma Assert
           (Big3 (X1, X2, X3) =
              Big3 (XX1, XX2, XX3) - Big3 (Single_Uns'(0), 1, 0));
         Lemma_Sub3_With_Carry3 (X1, X2, X3, Y3);
      else
         Lemma_Sub3_No_Carry (X1, X2, X3, 0, 0, Y3);
      end if;

      X3 := X3 - Y3;

      pragma Assert
        (Big3 (X1, X2, X3) = Big3 (XX1, XX2, XX3) - Big3 (0, 0, Y3));

      if Y2 > X2 then
         pragma Assert (X1 >= 1);
         Lemma_Sub3_No_Carry (X1, X2, X3, 1, 0, 0);

         X1 := X1 - 1;

         pragma Assert
           (Big3 (X1, X2, X3) = Big3 (XX1, XX2, XX3)
            - Big3 (0, 0, Y3) - Big3 (Single_Uns'(1), 0, 0));
         Lemma_Sub3_With_Carry2 (X1, X2, X3, Y2);
      else
         Lemma_Sub3_No_Carry (X1, X2, X3, 0, Y2, 0);
      end if;

      X2 := X2 - Y2;

      pragma Assert
        (Big3 (X1, X2, X3) = Big3 (XX1, XX2, XX3) - Big3 (0, Y2, Y3));
      pragma Assert (X1 >= Y1);
      Lemma_Sub3_No_Carry (X1, Y2, X3, Y1, 0, 0);

      X1 := X1 - Y1;

      pragma Assert
        (Big3 (X1, X2, X3) = Big3 (XX1, XX2, XX3)
         - Big3 (0, Y2, Y3) - Big3 (Y1, 0, 0));
      Lemma_Add3_No_Carry (0, Y2, Y3, Y1, 0, 0);
      pragma Assert
        (Big3 (X1, X2, X3) = Big3 (XX1, XX2, XX3) - Big3 (Y1, Y2, Y3));
   end Sub3;

   -------------------------------
   -- Subtract_With_Ovflo_Check --
   -------------------------------

   function Subtract_With_Ovflo_Check (X, Y : Double_Int) return Double_Int is
      R : constant Double_Int := To_Int (To_Uns (X) - To_Uns (Y));

      --  Local lemmas

      procedure Prove_Negative_X
      with
        Ghost,
        Pre  => X < 0 and then (Y <= 0 or else R < 0),
        Post => R = X - Y;

      procedure Prove_Non_Negative_X
      with
        Ghost,
        Pre  => X >= 0 and then (Y > 0 or else R >= 0),
        Post => R = X - Y;

      procedure Prove_Overflow_Case
      with
        Ghost,
        Pre  =>
          (if X >= 0 then Y <= 0 and then R < 0
                     else Y > 0 and then R >= 0),
        Post => not In_Double_Int_Range (Big (X) - Big (Y));

      ----------------------
      -- Prove_Negative_X --
      ----------------------

      procedure Prove_Negative_X is
      begin
         if X = Double_Int'First then
            if Y = Double_Int'First or else Y > 0 then
               null;
            else
               pragma Assert
                 (To_Uns (X) - To_Uns (Y) =
                    2 ** (Double_Size - 1) + Double_Uns (-Y));
            end if;

         elsif Y >= 0 or else Y = Double_Int'First then
            null;

         else
            pragma Assert
              (To_Uns (X) - To_Uns (Y) = -Double_Uns (-X) + Double_Uns (-Y));
         end if;
      end Prove_Negative_X;

      --------------------------
      -- Prove_Non_Negative_X --
      --------------------------

      procedure Prove_Non_Negative_X is
      begin
         if Y > 0 then
            declare
               Ru : constant Double_Uns := To_Uns (X) - To_Uns (Y);
            begin
               pragma Assert (Ru = Double_Uns (X) - Double_Uns (Y));
               if Ru < 2 ** (Double_Size - 1) then  --  R >= 0
                  pragma Assert (To_Uns (Y) <= To_Uns (X));
                  Lemma_Subtract_Double_Uns (X => Y, Y => X);
                  pragma Assert (Ru = Double_Uns (X - Y));

               elsif Ru = 2 ** (Double_Size - 1) then
                  pragma Assert (Double_Uns (Y) < 2 ** (Double_Size - 1));
                  pragma Assert (False);

               else
                  pragma Assert
                    (R = -Double_Int (-(Double_Uns (X) - Double_Uns (Y))));
                  pragma Assert
                    (R = -Double_Int (-Double_Uns (X) + Double_Uns (Y)));
                  pragma Assert
                    (R = -Double_Int (Double_Uns (Y) - Double_Uns (X)));
               end if;
            end;

         elsif Y = Double_Int'First then
            pragma Assert
              (To_Uns (X) - To_Uns (Y) =
                 Double_Uns (X) - 2 ** (Double_Size - 1));
            pragma Assert (False);

         else
            pragma Assert
              (To_Uns (X) - To_Uns (Y) = Double_Uns (X) + Double_Uns (-Y));
         end if;
      end Prove_Non_Negative_X;

      -------------------------
      -- Prove_Overflow_Case --
      -------------------------

      procedure Prove_Overflow_Case is
      begin
         if X >= 0 and then Y /= Double_Int'First then
            pragma Assert
              (To_Uns (X) - To_Uns (Y) = Double_Uns (X) + Double_Uns (-Y));

         elsif X < 0 and then X /= Double_Int'First then
            pragma Assert
              (To_Uns (X) - To_Uns (Y) = -Double_Uns (-X) - Double_Uns (Y));
         end if;
      end Prove_Overflow_Case;

   --  Start of processing for Subtract_With_Ovflo_Check

   begin
      if X >= 0 then
         if Y > 0 or else R >= 0 then
            Prove_Non_Negative_X;
            return R;
         end if;

      else -- X < 0
         if Y <= 0 or else R < 0 then
            Prove_Negative_X;
            return R;
         end if;
      end if;

      Prove_Overflow_Case;
      Raise_Error;
   end Subtract_With_Ovflo_Check;

   ----------------
   -- To_Neg_Int --
   ----------------

   function To_Neg_Int (A : Double_Uns) return Double_Int is
      R : constant Double_Int :=
        (if A = 2 ** (Double_Size - 1) then Double_Int'First else -To_Int (A));
      --  Note that we can't just use the expression of the Else, because it
      --  overflows for A = 2 ** (Double_Size - 1).
   begin
      if R <= 0 then
         return R;
      else
         Raise_Error;
      end if;
   end To_Neg_Int;

   ----------------
   -- To_Pos_Int --
   ----------------

   function To_Pos_Int (A : Double_Uns) return Double_Int is
      R : constant Double_Int := To_Int (A);
   begin
      if R >= 0 then
         return R;
      else
         Raise_Error;
      end if;
   end To_Pos_Int;

   pragma Annotate (Gnatcheck, Exempt_Off, "Improper_Returns");
end System.Arith_Double;

pragma Annotate (Gnatcheck, Exempt_Off, "Metrics_LSLOC");
