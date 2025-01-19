------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . A R I T H _ 3 2                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2025, Free Software Foundation, Inc.       --
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

--  Preconditions, postconditions, ghost code, loop invariants and assertions
--  in this unit are meant for analysis only, not for run-time checking, as it
--  would be too costly otherwise. This is enforced by setting the assertion
--  policy to Ignore.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Ghost          => Ignore,
                         Loop_Invariant => Ignore,
                         Assert         => Ignore);

with Ada.Numerics.Big_Numbers.Big_Integers_Ghost;
use Ada.Numerics.Big_Numbers.Big_Integers_Ghost;
with Ada.Unchecked_Conversion;

package body System.Arith_32
  with SPARK_Mode
is

   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);

   subtype Uns32 is Interfaces.Unsigned_32;
   subtype Uns64 is Interfaces.Unsigned_64;

   use Interfaces;

   function To_Int is new Ada.Unchecked_Conversion (Uns32, Int32);

   package Unsigned_Conversion is new Unsigned_Conversions (Int => Uns32);

   function Big (Arg : Uns32) return Big_Integer is
     (Unsigned_Conversion.To_Big_Integer (Arg))
   with Ghost;

   package Unsigned_Conversion_64 is new Unsigned_Conversions (Int => Uns64);

   function Big (Arg : Uns64) return Big_Integer is
     (Unsigned_Conversion_64.To_Big_Integer (Arg))
   with Ghost;

   pragma Warnings
     (Off, "non-preelaborable call not allowed in preelaborated unit",
      Reason => "Ghost code is not compiled");
   Big_0 : constant Big_Integer :=
     Big (Uns32'(0))
   with Ghost;
   Big_2xx32 : constant Big_Integer :=
     Big (Uns32'(2 ** 32 - 1)) + 1
   with Ghost;
   Big_2xx64 : constant Big_Integer :=
     Big (Uns64'(2 ** 64 - 1)) + 1
   with Ghost;
   pragma Warnings
     (On, "non-preelaborable call not allowed in preelaborated unit");

   -----------------------
   -- Local Subprograms --
   -----------------------

   function "abs" (X : Int32) return Uns32 is
     (if X = Int32'First
      then Uns32'(2**31)
      else Uns32 (Int32'(abs X)));
   --  Convert absolute value of X to unsigned. Note that we can't just use
   --  the expression of the Else since it overflows for X = Int32'First.

   function Lo (A : Uns64) return Uns32 is (Uns32 (A and (2 ** 32 - 1)));
   --  Low order half of 64-bit value

   function Hi (A : Uns64) return Uns32 is (Uns32 (Shift_Right (A, 32)));
   --  High order half of 64-bit value

   function To_Neg_Int (A : Uns32) return Int32
   with
     Pre  => In_Int32_Range (-Big (A)),
     Post => Big (To_Neg_Int'Result) = -Big (A);
   --  Convert to negative integer equivalent. If the input is in the range
   --  0 .. 2**31, then the corresponding nonpositive signed integer (obtained
   --  by negating the given value) is returned, otherwise constraint error is
   --  raised.

   function To_Pos_Int (A : Uns32) return Int32
   with
     Pre  => In_Int32_Range (Big (A)),
     Post => Big (To_Pos_Int'Result) = Big (A);
   --  Convert to positive integer equivalent. If the input is in the range
   --  0 .. 2**31 - 1, then the corresponding nonnegative signed integer is
   --  returned, otherwise constraint error is raised.

   procedure Raise_Error with
     Always_Terminates,
     Exceptional_Cases => (Constraint_Error => True);
   pragma No_Return (Raise_Error);
   --  Raise constraint error with appropriate message

   ------------------
   -- Local Lemmas --
   ------------------

   procedure Lemma_Abs_Commutation (X : Int32)
   with
     Ghost,
     Post => abs Big (X) = Big (Uns32'(abs X));

   procedure Lemma_Abs_Div_Commutation (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => abs (X / Y) = abs X / abs Y;

   procedure Lemma_Abs_Mult_Commutation (X, Y : Big_Integer)
   with
     Ghost,
     Post => abs (X * Y) = abs X * abs Y;

   procedure Lemma_Abs_Rem_Commutation (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => abs (X rem Y) = (abs X) rem (abs Y);

   procedure Lemma_Div_Commutation (X, Y : Uns64)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => Big (X) / Big (Y) = Big (X / Y);

   procedure Lemma_Div_Ge (X, Y, Z : Big_Integer)
   with
     Ghost,
     Pre  => Z > 0 and then X >= Y * Z,
     Post => X / Z >= Y;

   procedure Lemma_Ge_Commutation (A, B : Uns32)
   with
     Ghost,
     Pre  => A >= B,
     Post => Big (A) >= Big (B);

   procedure Lemma_Hi_Lo (Xu : Uns64; Xhi, Xlo : Uns32)
   with
     Ghost,
     Pre  => Xhi = Hi (Xu) and Xlo = Lo (Xu),
     Post => Big (Xu) = Big_2xx32 * Big (Xhi) + Big (Xlo);

   procedure Lemma_Mult_Commutation (X, Y, Z : Uns64)
   with
     Ghost,
     Pre  => Big (X) * Big (Y) < Big_2xx64 and then Z = X * Y,
     Post => Big (X) * Big (Y) = Big (Z);

   procedure Lemma_Mult_Non_Negative (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => (X >= Big_0 and then Y >= Big_0)
       or else (X <= Big_0 and then Y <= Big_0),
     Post => X * Y >= Big_0;

   procedure Lemma_Mult_Non_Positive (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => (X <= Big_0 and then Y >= Big_0)
       or else (X >= Big_0 and then Y <= Big_0),
     Post => X * Y <= Big_0;

   procedure Lemma_Neg_Rem (X, Y : Big_Integer)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => X rem Y = X rem (-Y);

   procedure Lemma_Not_In_Range_Big2xx32
   with
     Post => not In_Int32_Range (Big_2xx32)
       and then not In_Int32_Range (-Big_2xx32);

   procedure Lemma_Rem_Commutation (X, Y : Uns64)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => Big (X) rem Big (Y) = Big (X rem Y);

   -----------------------------
   -- Local lemma null bodies --
   -----------------------------

   procedure Lemma_Abs_Commutation (X : Int32) is null;
   procedure Lemma_Abs_Div_Commutation (X, Y : Big_Integer) is null;
   procedure Lemma_Abs_Mult_Commutation (X, Y : Big_Integer) is null;
   procedure Lemma_Div_Commutation (X, Y : Uns64) is null;
   procedure Lemma_Div_Ge (X, Y, Z : Big_Integer) is null;
   procedure Lemma_Ge_Commutation (A, B : Uns32) is null;
   procedure Lemma_Mult_Commutation (X, Y, Z : Uns64) is null;
   procedure Lemma_Mult_Non_Negative (X, Y : Big_Integer) is null;
   procedure Lemma_Mult_Non_Positive (X, Y : Big_Integer) is null;
   procedure Lemma_Neg_Rem (X, Y : Big_Integer) is null;
   procedure Lemma_Not_In_Range_Big2xx32 is null;
   procedure Lemma_Rem_Commutation (X, Y : Uns64) is null;

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

   -----------------
   -- Lemma_Hi_Lo --
   -----------------

   procedure Lemma_Hi_Lo (Xu : Uns64; Xhi, Xlo : Uns32) is
   begin
      pragma Assert (Uns64 (Xhi) = Xu / Uns64'(2 ** 32));
      pragma Assert (Uns64 (Xlo) = Xu mod 2 ** 32);
   end Lemma_Hi_Lo;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error is
   begin
      raise Constraint_Error with "32-bit arithmetic overflow";
      pragma Annotate
        (GNATprove, Intentional, "exception might be raised",
         "Procedure Raise_Error is called to signal input errors");
   end Raise_Error;

   -------------------
   -- Scaled_Divide --
   -------------------

   procedure Scaled_Divide32
     (X, Y, Z : Int32;
      Q, R    : out Int32;
      Round   : Boolean)
   is
      Xu  : constant Uns32 := abs X;
      Yu  : constant Uns32 := abs Y;
      Zu  : constant Uns32 := abs Z;

      D   : Uns64;
      --  The dividend

      Qu : Uns32;
      Ru : Uns32;
      --  Unsigned quotient and remainder

      --  Local ghost variables

      Mult  : constant Big_Integer := abs (Big (X) * Big (Y)) with Ghost;
      Quot  : Big_Integer with Ghost;
      Big_R : Big_Integer with Ghost;
      Big_Q : Big_Integer with Ghost;

      --  Local lemmas

      procedure Prove_Negative_Dividend
      with
        Ghost,
        Pre  => Z /= 0
          and then ((X >= 0 and Y < 0) or (X < 0 and Y >= 0))
          and then Big_Q =
            (if Round then Round_Quotient (Big (X) * Big (Y), Big (Z),
                                           Big (X) * Big (Y) / Big (Z),
                                           Big (X) * Big (Y) rem Big (Z))
             else Big (X) * Big (Y) / Big (Z)),
         Post =>
           (if Z > 0 then Big_Q <= Big_0 else Big_Q >= Big_0);
      --  Proves the sign of rounded quotient when dividend is non-positive

      procedure Prove_Overflow
      with
        Ghost,
        Pre  => Z /= 0 and then Mult >= Big_2xx32 * Big (Uns32'(abs Z)),
        Post => not In_Int32_Range (Big (X) * Big (Y) / Big (Z))
          and then not In_Int32_Range
            (Round_Quotient (Big (X) * Big (Y), Big (Z),
                             Big (X) * Big (Y) / Big (Z),
                             Big (X) * Big (Y) rem Big (Z)));
      --  Proves overflow case

      procedure Prove_Positive_Dividend
      with
        Ghost,
        Pre  => Z /= 0
          and then ((X >= 0 and Y >= 0) or (X < 0 and Y < 0))
          and then Big_Q =
            (if Round then Round_Quotient (Big (X) * Big (Y), Big (Z),
                                           Big (X) * Big (Y) / Big (Z),
                                           Big (X) * Big (Y) rem Big (Z))
             else Big (X) * Big (Y) / Big (Z)),
         Post =>
           (if Z > 0 then Big_Q >= Big_0 else Big_Q <= Big_0);
      --  Proves the sign of rounded quotient when dividend is non-negative

      procedure Prove_Rounding_Case
      with
        Ghost,
        Pre  => Z /= 0
          and then Quot = Big (X) * Big (Y) / Big (Z)
          and then Big_R = Big (X) * Big (Y) rem Big (Z)
          and then Big_Q =
            Round_Quotient (Big (X) * Big (Y), Big (Z), Quot, Big_R)
          and then Big (Ru) = abs Big_R
          and then Big (Zu) = Big (Uns32'(abs Z)),
        Post => abs Big_Q =
          (if Ru > (Zu - Uns32'(1)) / Uns32'(2)
           then abs Quot + 1
           else abs Quot);
      --  Proves correctness of the rounding of the unsigned quotient

      procedure Prove_Sign_R
      with
        Ghost,
        Pre  => Z /= 0 and then Big_R = Big (X) * Big (Y) rem Big (Z),
        Post => In_Int32_Range (Big_R);

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
          and then In_Int32_Range (Big_Q)
          and then In_Int32_Range (Big_R)
          and then R =
            (if (X >= 0) = (Y >= 0) then To_Pos_Int (Ru) else To_Neg_Int (Ru))
          and then Q =
            (if ((X >= 0) = (Y >= 0)) = (Z >= 0) then To_Pos_Int (Qu)
             else To_Neg_Int (Qu)),  --  need to ensure To_Pos_Int precondition
        Post => Big (R) = Big_R and then Big (Q) = Big_Q;
      --  Proves final signs match the intended result after the unsigned
      --  division is done.

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
         Lemma_Div_Ge (Mult, Big_2xx32, Big (Uns32'(abs Z)));
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

      -------------------------
      -- Prove_Rounding_Case --
      -------------------------

      procedure Prove_Rounding_Case is
      begin
         if Same_Sign (Big (X) * Big (Y), Big (Z)) then
            pragma Assert
              (abs Big_Q =
                 (if Ru > (Zu - Uns32'(1)) / Uns32'(2)
                  then abs Quot + 1
                  else abs Quot));
         end if;
      end Prove_Rounding_Case;

      ------------------
      -- Prove_Sign_R --
      ------------------

      procedure Prove_Sign_R is
      begin
         pragma Assert (In_Int32_Range (Big (Z)));
      end Prove_Sign_R;

      -----------------
      -- Prove_Signs --
      -----------------

      procedure Prove_Signs is
      begin
         if (X >= 0) = (Y >= 0) then
            pragma Assert (Big (R) = Big_R and then Big (Q) = Big_Q);
         else
            pragma Assert (Big (R) = Big_R and then Big (Q) = Big_Q);
         end if;
      end Prove_Signs;

   --  Start of processing for Scaled_Divide32

   begin
      --  First do the 64-bit multiplication

      D := Uns64 (Xu) * Uns64 (Yu);

      Lemma_Abs_Mult_Commutation (Big (X), Big (Y));
      pragma Assert (Mult = Big (D));
      Lemma_Hi_Lo (D, Hi (D), Lo (D));
      pragma Assert (Mult = Big_2xx32 * Big (Hi (D)) + Big (Lo (D)));

      --  If divisor is zero, raise error

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

      --  If dividend is too large, raise error

      if Hi (D) >= Zu then
         Lemma_Ge_Commutation (Hi (D), Zu);
         pragma Assert (Mult >= Big_2xx32 * Big (Zu));
         Prove_Overflow;
         Raise_Error;
      end if;

      --  Then do the 64-bit division

      Qu := Uns32 (D / Uns64 (Zu));
      Ru := Uns32 (D rem Uns64 (Zu));

      Lemma_Abs_Div_Commutation (Big (X) * Big (Y), Big (Z));
      Lemma_Abs_Rem_Commutation (Big (X) * Big (Y), Big (Z));
      Lemma_Abs_Commutation (X);
      Lemma_Abs_Commutation (Y);
      Lemma_Abs_Commutation (Z);
      Lemma_Mult_Commutation (Uns64 (Xu), Uns64 (Yu), D);
      Lemma_Div_Commutation (D, Uns64 (Zu));
      Lemma_Rem_Commutation (D, Uns64 (Zu));

      pragma Assert (Uns64 (Qu) = D / Uns64 (Zu));
      pragma Assert (Uns64 (Ru) = D rem Uns64 (Zu));
      pragma Assert (Big (Ru) = abs Big_R);
      pragma Assert (Big (Qu) = abs Quot);
      pragma Assert (Big (Zu) = Big (Uns32'(abs Z)));

      --  Deal with rounding case

      if Round then
         Prove_Rounding_Case;

         if Ru > (Zu - Uns32'(1)) / Uns32'(2) then
            pragma Assert (abs Big_Q = Big (Qu) + 1);

            --  Protect against wrapping around when rounding, by signaling
            --  an overflow when the quotient is too large.

            if Qu = Uns32'Last then
               pragma Assert (abs Big_Q = Big_2xx32);
               Lemma_Not_In_Range_Big2xx32;
               Raise_Error;
            end if;

            Qu := Qu + Uns32'(1);
         end if;
      end if;

      pragma Assert (In_Int32_Range (Big_Q));
      pragma Assert (Big (Qu) = abs Big_Q);
      pragma Assert (Big (Ru) = abs Big_R);
      Prove_Sign_R;

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

      Prove_Signs;
   end Scaled_Divide32;

   ----------------
   -- To_Neg_Int --
   ----------------

   function To_Neg_Int (A : Uns32) return Int32 is
      R : constant Int32 :=
        (if A = 2**31 then Int32'First else -To_Int (A));
      --  Note that we can't just use the expression of the Else, because it
      --  overflows for A = 2**31.
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

   function To_Pos_Int (A : Uns32) return Int32 is
      R : constant Int32 := To_Int (A);
   begin
      if R >= 0 then
         return R;
      else
         Raise_Error;
      end if;
   end To_Pos_Int;

end System.Arith_32;
