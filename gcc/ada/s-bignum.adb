------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . B I G N U M S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2012-2015, Free Software Foundation, Inc.         --
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
--  pragma Overflow_Check (Eliminate) is in effect.

with System;                  use System;
with System.Secondary_Stack;  use System.Secondary_Stack;
with System.Storage_Elements; use System.Storage_Elements;

package body System.Bignums is

   use Interfaces;
   --  So that operations on Unsigned_32 are available

   type DD is mod Base ** 2;
   --  Double length digit used for intermediate computations

   function MSD (X : DD) return SD is (SD (X / Base));
   function LSD (X : DD) return SD is (SD (X mod Base));
   --  Most significant and least significant digit of double digit value

   function "&" (X, Y : SD) return DD is (DD (X) * Base + DD (Y));
   --  Compose double digit value from two single digit values

   subtype LLI is Long_Long_Integer;

   One_Data : constant Digit_Vector (1 .. 1) := (1 => 1);
   --  Constant one

   Zero_Data : constant Digit_Vector (1 .. 0) := (1 .. 0 => 0);
   --  Constant zero

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Add
     (X, Y  : Digit_Vector;
      X_Neg : Boolean;
      Y_Neg : Boolean) return Bignum
   with
     Pre => X'First = 1 and then Y'First = 1;
   --  This procedure adds two signed numbers returning the Sum, it is used
   --  for both addition and subtraction. The value computed is X + Y, with
   --  X_Neg and Y_Neg giving the signs of the operands.

   function Allocate_Bignum (Len : Length) return Bignum with
     Post => Allocate_Bignum'Result.Len = Len;
   --  Allocate Bignum value of indicated length on secondary stack. On return
   --  the Neg and D fields are left uninitialized.

   type Compare_Result is (LT, EQ, GT);
   --  Indicates result of comparison in following call

   function Compare
     (X, Y         : Digit_Vector;
      X_Neg, Y_Neg : Boolean) return Compare_Result
   with
     Pre => X'First = 1 and then Y'First = 1;
   --  Compare (X with sign X_Neg) with (Y with sign Y_Neg), and return the
   --  result of the signed comparison.

   procedure Div_Rem
     (X, Y              : Bignum;
      Quotient          : out Bignum;
      Remainder         : out Bignum;
      Discard_Quotient  : Boolean := False;
      Discard_Remainder : Boolean := False);
   --  Returns the Quotient and Remainder from dividing abs (X) by abs (Y). The
   --  values of X and Y are not modified. If Discard_Quotient is True, then
   --  Quotient is undefined on return, and if Discard_Remainder is True, then
   --  Remainder is undefined on return. Service routine for Big_Div/Rem/Mod.

   procedure Free_Bignum (X : Bignum) is null;
   --  Called to free a Bignum value used in intermediate computations. In
   --  this implementation using the secondary stack, it does nothing at all,
   --  because we rely on Mark/Release, but it may be of use for some
   --  alternative implementation.

   function Normalize
     (X   : Digit_Vector;
      Neg : Boolean := False) return Bignum;
   --  Given a digit vector and sign, allocate and construct a Bignum value.
   --  Note that X may have leading zeroes which must be removed, and if the
   --  result is zero, the sign is forced positive.

   ---------
   -- Add --
   ---------

   function Add
     (X, Y  : Digit_Vector;
      X_Neg : Boolean;
      Y_Neg : Boolean) return Bignum
   is
   begin
      --  If signs are the same, we are doing an addition, it is convenient to
      --  ensure that the first operand is the longer of the two.

      if X_Neg = Y_Neg then
         if X'Last < Y'Last then
            return Add (X => Y, Y => X, X_Neg => Y_Neg, Y_Neg => X_Neg);

         --  Here signs are the same, and the first operand is the longer

         else
            pragma Assert (X_Neg = Y_Neg and then X'Last >= Y'Last);

            --  Do addition, putting result in Sum (allowing for carry)

            declare
               Sum : Digit_Vector (0 .. X'Last);
               RD  : DD;

            begin
               RD := 0;
               for J in reverse 1 .. X'Last loop
                  RD := RD + DD (X (J));

                  if J >= 1 + (X'Last - Y'Last) then
                     RD := RD + DD (Y (J - (X'Last - Y'Last)));
                  end if;

                  Sum (J) := LSD (RD);
                  RD := RD / Base;
               end loop;

               Sum (0) := SD (RD);
               return Normalize (Sum, X_Neg);
            end;
         end if;

      --  Signs are different so really this is a subtraction, we want to make
      --  sure that the largest magnitude operand is the first one, and then
      --  the result will have the sign of the first operand.

      else
         declare
            CR : constant Compare_Result := Compare (X, Y, False, False);

         begin
            if CR = EQ then
               return Normalize (Zero_Data);

            elsif CR = LT then
               return Add (X => Y, Y => X, X_Neg => Y_Neg, Y_Neg => X_Neg);

            else
               pragma Assert (X_Neg /= Y_Neg and then CR = GT);

               --  Do subtraction, putting result in Diff

               declare
                  Diff : Digit_Vector (1 .. X'Length);
                  RD   : DD;

               begin
                  RD := 0;
                  for J in reverse 1 .. X'Last loop
                     RD := RD + DD (X (J));

                     if J >= 1 + (X'Last - Y'Last) then
                        RD := RD - DD (Y (J - (X'Last - Y'Last)));
                     end if;

                     Diff (J) := LSD (RD);
                     RD := (if RD < Base then 0 else -1);
                  end loop;

                  return Normalize (Diff, X_Neg);
               end;
            end if;
         end;
      end if;
   end Add;

   ---------------------
   -- Allocate_Bignum --
   ---------------------

   function Allocate_Bignum (Len : Length) return Bignum is
      Addr : Address;

   begin
      --  Change the if False here to if True to get allocation on the heap
      --  instead of the secondary stack, which is convenient for debugging
      --  System.Bignum itself.

      if False then
         declare
            B : Bignum;
         begin
            B := new Bignum_Data'(Len, False, (others => 0));
            return B;
         end;

      --  Normal case of allocation on the secondary stack

      else
         --  Note: The approach used here is designed to avoid strict aliasing
         --  warnings that appeared previously using unchecked conversion.

         SS_Allocate (Addr, Storage_Offset (4 + 4 * Len));

         declare
            B : Bignum;
            for B'Address use Addr'Address;
            pragma Import (Ada, B);

            BD : Bignum_Data (Len);
            for BD'Address use Addr;
            pragma Import (Ada, BD);

            --  Expose a writable view of discriminant BD.Len so that we can
            --  initialize it. We need to use the exact layout of the record
            --  to ensure that the Length field has 24 bits as expected.

            type Bignum_Data_Header is record
               Len : Length;
               Neg : Boolean;
            end record;

            for Bignum_Data_Header use record
               Len at 0 range 0 .. 23;
               Neg at 3 range 0 .. 7;
            end record;

            BDH : Bignum_Data_Header;
            for BDH'Address use BD'Address;
            pragma Import (Ada, BDH);

            pragma Assert (BDH.Len'Size = BD.Len'Size);

         begin
            BDH.Len := Len;
            return B;
         end;
      end if;
   end Allocate_Bignum;

   -------------
   -- Big_Abs --
   -------------

   function Big_Abs (X : Bignum) return Bignum is
   begin
      return Normalize (X.D);
   end Big_Abs;

   -------------
   -- Big_Add --
   -------------

   function Big_Add  (X, Y : Bignum) return Bignum is
   begin
      return Add (X.D, Y.D, X.Neg, Y.Neg);
   end Big_Add;

   -------------
   -- Big_Div --
   -------------

   --  This table is excerpted from RM 4.5.5(28-30) and shows how the result
   --  varies with the signs of the operands.

   --   A      B   A/B      A     B    A/B
   --
   --   10     5    2      -10    5    -2
   --   11     5    2      -11    5    -2
   --   12     5    2      -12    5    -2
   --   13     5    2      -13    5    -2
   --   14     5    2      -14    5    -2
   --
   --   A      B   A/B      A     B    A/B
   --
   --   10    -5   -2      -10   -5     2
   --   11    -5   -2      -11   -5     2
   --   12    -5   -2      -12   -5     2
   --   13    -5   -2      -13   -5     2
   --   14    -5   -2      -14   -5     2

   function Big_Div  (X, Y : Bignum) return Bignum is
      Q, R : Bignum;
   begin
      Div_Rem (X, Y, Q, R, Discard_Remainder => True);
      Q.Neg := Q.Len > 0 and then (X.Neg xor Y.Neg);
      return Q;
   end Big_Div;

   -------------
   -- Big_Exp --
   -------------

   function Big_Exp  (X, Y : Bignum) return Bignum is

      function "**" (X : Bignum; Y : SD) return Bignum;
      --  Internal routine where we know right operand is one word

      ----------
      -- "**" --
      ----------

      function "**" (X : Bignum; Y : SD) return Bignum is
      begin
         case Y is

            --  X ** 0 is 1

            when 0 =>
               return Normalize (One_Data);

            --  X ** 1 is X

            when 1 =>
               return Normalize (X.D);

            --  X ** 2 is X * X

            when 2 =>
               return Big_Mul (X, X);

            --  For X greater than 2, use the recursion

            --  X even, X ** Y = (X ** (Y/2)) ** 2;
            --  X odd,  X ** Y = (X ** (Y/2)) ** 2 * X;

            when others =>
               declare
                  XY2  : constant Bignum := X ** (Y / 2);
                  XY2S : constant Bignum := Big_Mul (XY2, XY2);
                  Res  : Bignum;

               begin
                  Free_Bignum (XY2);

                  --  Raise storage error if intermediate value is getting too
                  --  large, which we arbitrarily define as 200 words for now.

                  if XY2S.Len > 200 then
                     Free_Bignum (XY2S);
                     raise Storage_Error with
                       "exponentiation result is too large";
                  end if;

                  --  Otherwise take care of even/odd cases

                  if (Y and 1) = 0 then
                     return XY2S;

                  else
                     Res := Big_Mul (XY2S, X);
                     Free_Bignum (XY2S);
                     return Res;
                  end if;
               end;
         end case;
      end "**";

   --  Start of processing for Big_Exp

   begin
      --  Error if right operand negative

      if Y.Neg then
         raise Constraint_Error with "exponentiation to negative power";

      --  X ** 0 is always 1 (including 0 ** 0, so do this test first)

      elsif Y.Len = 0 then
         return Normalize (One_Data);

      --  0 ** X is always 0 (for X non-zero)

      elsif X.Len = 0 then
         return Normalize (Zero_Data);

      --  (+1) ** Y = 1
      --  (-1) ** Y = +/-1 depending on whether Y is even or odd

      elsif X.Len = 1 and then X.D (1) = 1 then
         return Normalize
           (X.D, Neg => X.Neg and then ((Y.D (Y.Len) and 1) = 1));

      --  If the absolute value of the base is greater than 1, then the
      --  exponent must not be bigger than one word, otherwise the result
      --  is ludicrously large, and we just signal Storage_Error right away.

      elsif Y.Len > 1 then
         raise Storage_Error with "exponentiation result is too large";

      --  Special case (+/-)2 ** K, where K is 1 .. 31 using a shift

      elsif X.Len = 1 and then X.D (1) = 2 and then Y.D (1) < 32 then
         declare
            D : constant Digit_Vector (1 .. 1) :=
                  (1 => Shift_Left (SD'(1), Natural (Y.D (1))));
         begin
            return Normalize (D, X.Neg);
         end;

      --  Remaining cases have right operand of one word

      else
         return X ** Y.D (1);
      end if;
   end Big_Exp;

   ------------
   -- Big_EQ --
   ------------

   function Big_EQ (X, Y : Bignum) return Boolean is
   begin
      return Compare (X.D, Y.D, X.Neg, Y.Neg) = EQ;
   end Big_EQ;

   ------------
   -- Big_GE --
   ------------

   function Big_GE (X, Y : Bignum) return Boolean is
   begin
      return Compare (X.D, Y.D, X.Neg, Y.Neg) /= LT;
   end Big_GE;

   ------------
   -- Big_GT --
   ------------

   function Big_GT (X, Y : Bignum) return Boolean is
   begin
      return Compare (X.D, Y.D, X.Neg, Y.Neg) = GT;
   end Big_GT;

   ------------
   -- Big_LE --
   ------------

   function Big_LE (X, Y : Bignum) return Boolean is
   begin
      return Compare (X.D, Y.D, X.Neg, Y.Neg) /= GT;
   end Big_LE;

   ------------
   -- Big_LT --
   ------------

   function Big_LT (X, Y : Bignum) return Boolean is
   begin
      return Compare (X.D, Y.D, X.Neg, Y.Neg) = LT;
   end Big_LT;

   -------------
   -- Big_Mod --
   -------------

   --  This table is excerpted from RM 4.5.5(28-30) and shows how the result
   --  of Rem and Mod vary with the signs of the operands.

   --   A      B    A mod B  A rem B     A     B    A mod B  A rem B

   --   10     5       0        0       -10    5       0        0
   --   11     5       1        1       -11    5       4       -1
   --   12     5       2        2       -12    5       3       -2
   --   13     5       3        3       -13    5       2       -3
   --   14     5       4        4       -14    5       1       -4

   --   A      B    A mod B  A rem B     A     B    A mod B  A rem B

   --   10    -5       0        0       -10   -5       0        0
   --   11    -5      -4        1       -11   -5      -1       -1
   --   12    -5      -3        2       -12   -5      -2       -2
   --   13    -5      -2        3       -13   -5      -3       -3
   --   14    -5      -1        4       -14   -5      -4       -4

   function Big_Mod (X, Y : Bignum) return Bignum is
      Q, R : Bignum;

   begin
      --  If signs are same, result is same as Rem

      if X.Neg = Y.Neg then
         return Big_Rem (X, Y);

      --  Case where Mod is different

      else
         --  Do division

         Div_Rem (X, Y, Q, R, Discard_Quotient => True);

         --  Zero result is unchanged

         if R.Len = 0 then
            return R;

         --  Otherwise adjust result

         else
            declare
               T1 : constant Bignum := Big_Sub (Y, R);
            begin
               T1.Neg := Y.Neg;
               Free_Bignum (R);
               return T1;
            end;
         end if;
      end if;
   end Big_Mod;

   -------------
   -- Big_Mul --
   -------------

   function Big_Mul (X, Y : Bignum) return Bignum is
      Result : Digit_Vector (1 .. X.Len + Y.Len) := (others => 0);
      --  Accumulate result (max length of result is sum of operand lengths)

      L : Length;
      --  Current result digit

      D : DD;
      --  Result digit

   begin
      for J in 1 .. X.Len loop
         for K in 1 .. Y.Len loop
            L := Result'Last - (X.Len - J) - (Y.Len - K);
            D := DD (X.D (J)) * DD (Y.D (K)) + DD (Result (L));
            Result (L) := LSD (D);
            D := D / Base;

            --  D is carry which must be propagated

            while D /= 0 and then L >= 1 loop
               L := L - 1;
               D := D + DD (Result (L));
               Result (L) := LSD (D);
               D := D / Base;
            end loop;

            --  Must not have a carry trying to extend max length

            pragma Assert (D = 0);
         end loop;
      end loop;

      --  Return result

      return Normalize (Result, X.Neg xor Y.Neg);
   end Big_Mul;

   ------------
   -- Big_NE --
   ------------

   function Big_NE (X, Y : Bignum) return Boolean is
   begin
      return Compare (X.D, Y.D, X.Neg, Y.Neg) /= EQ;
   end Big_NE;

   -------------
   -- Big_Neg --
   -------------

   function Big_Neg (X : Bignum) return Bignum is
   begin
      return Normalize (X.D, not X.Neg);
   end Big_Neg;

   -------------
   -- Big_Rem --
   -------------

   --  This table is excerpted from RM 4.5.5(28-30) and shows how the result
   --  varies with the signs of the operands.

   --   A      B   A rem B   A     B   A rem B

   --   10     5      0     -10    5      0
   --   11     5      1     -11    5     -1
   --   12     5      2     -12    5     -2
   --   13     5      3     -13    5     -3
   --   14     5      4     -14    5     -4

   --   A      B  A rem B    A     B   A rem B

   --   10    -5     0      -10   -5      0
   --   11    -5     1      -11   -5     -1
   --   12    -5     2      -12   -5     -2
   --   13    -5     3      -13   -5     -3
   --   14    -5     4      -14   -5     -4

   function Big_Rem (X, Y : Bignum) return Bignum is
      Q, R : Bignum;
   begin
      Div_Rem (X, Y, Q, R, Discard_Quotient => True);
      R.Neg := R.Len > 0 and then X.Neg;
      return R;
   end Big_Rem;

   -------------
   -- Big_Sub --
   -------------

   function Big_Sub (X, Y : Bignum) return Bignum is
   begin
      --  If right operand zero, return left operand (avoiding sharing)

      if Y.Len = 0 then
         return Normalize (X.D, X.Neg);

      --  Otherwise add negative of right operand

      else
         return Add (X.D, Y.D, X.Neg, not Y.Neg);
      end if;
   end Big_Sub;

   -------------
   -- Compare --
   -------------

   function Compare
     (X, Y         : Digit_Vector;
      X_Neg, Y_Neg : Boolean) return Compare_Result
   is
   begin
      --  Signs are different, that's decisive, since 0 is always plus

      if X_Neg /= Y_Neg then
         return (if X_Neg then LT else GT);

      --  Lengths are different, that's decisive since no leading zeroes

      elsif X'Last /= Y'Last then
         return (if (X'Last > Y'Last) xor X_Neg then GT else LT);

      --  Need to compare data

      else
         for J in X'Range loop
            if X (J) /= Y (J) then
               return (if (X (J) > Y (J)) xor X_Neg then GT else LT);
            end if;
         end loop;

         return EQ;
      end if;
   end Compare;

   -------------
   -- Div_Rem --
   -------------

   procedure Div_Rem
     (X, Y              : Bignum;
      Quotient          : out Bignum;
      Remainder         : out Bignum;
      Discard_Quotient  : Boolean := False;
      Discard_Remainder : Boolean := False)
   is
   begin
      --  Error if division by zero

      if Y.Len = 0 then
         raise Constraint_Error with "division by zero";
      end if;

      --  Handle simple cases with special tests

      --  If X < Y then quotient is zero and remainder is X

      if Compare (X.D, Y.D, False, False) = LT then
         Remainder := Normalize (X.D);
         Quotient  := Normalize (Zero_Data);
         return;

      --  If both X and Y are less than 2**63-1, we can use Long_Long_Integer
      --  arithmetic. Note it is good not to do an accurate range check against
      --  Long_Long_Integer since -2**63 / -1 overflows.

      elsif (X.Len <= 1 or else (X.Len = 2 and then X.D (1) < 2**31))
              and then
            (Y.Len <= 1 or else (Y.Len = 2 and then Y.D (1) < 2**31))
      then
         declare
            A : constant LLI := abs (From_Bignum (X));
            B : constant LLI := abs (From_Bignum (Y));
         begin
            Quotient  := To_Bignum (A / B);
            Remainder := To_Bignum (A rem B);
            return;
         end;

      --  Easy case if divisor is one digit

      elsif Y.Len = 1 then
         declare
            ND  : DD;
            Div : constant DD := DD (Y.D (1));

            Result : Digit_Vector (1 .. X.Len);
            Remdr  : Digit_Vector (1 .. 1);

         begin
            ND := 0;
            for J in 1 .. X.Len loop
               ND := Base * ND + DD (X.D (J));
               Result (J) := SD (ND / Div);
               ND := ND rem Div;
            end loop;

            Quotient  := Normalize (Result);
            Remdr (1) := SD (ND);
            Remainder := Normalize (Remdr);
            return;
         end;
      end if;

      --  The complex full multi-precision case. We will employ algorithm
      --  D defined in the section "The Classical Algorithms" (sec. 4.3.1)
      --  of Donald Knuth's "The Art of Computer Programming", Vol. 2, 2nd
      --  edition. The terminology is adjusted for this section to match that
      --  reference.

      --  We are dividing X.Len digits of X (called u here) by Y.Len digits
      --  of Y (called v here), developing the quotient and remainder. The
      --  numbers are represented using Base, which was chosen so that we have
      --  the operations of multiplying to single digits (SD) to form a double
      --  digit (DD), and dividing a double digit (DD) by a single digit (SD)
      --  to give a single digit quotient and a single digit remainder.

      --  Algorithm D from Knuth

      --  Comments here with square brackets are directly from Knuth

      Algorithm_D : declare

         --  The following lower case variables correspond exactly to the
         --  terminology used in algorithm D.

         m : constant Length := X.Len - Y.Len;
         n : constant Length := Y.Len;
         b : constant DD     := Base;

         u : Digit_Vector (0 .. m + n);
         v : Digit_Vector (1 .. n);
         q : Digit_Vector (0 .. m);
         r : Digit_Vector (1 .. n);

         u0 : SD renames u (0);
         v1 : SD renames v (1);
         v2 : SD renames v (2);

         d    : DD;
         j    : Length;
         qhat : DD;
         rhat : DD;
         temp : DD;

      begin
         --  Initialize data of left and right operands

         for J in 1 .. m + n loop
            u (J) := X.D (J);
         end loop;

         for J in 1 .. n loop
            v (J) := Y.D (J);
         end loop;

         --  [Division of nonnegative integers.] Given nonnegative integers u
         --  = (ul,u2..um+n) and v = (v1,v2..vn), where v1 /= 0 and n > 1, we
         --  form the quotient u / v = (q0,ql..qm) and the remainder u mod v =
         --  (r1,r2..rn).

         pragma Assert (v1 /= 0);
         pragma Assert (n > 1);

         --  Dl. [Normalize.] Set d = b/(vl + 1). Then set (u0,u1,u2..um+n)
         --  equal to (u1,u2..um+n) times d, and set (v1,v2..vn) equal to
         --  (v1,v2..vn) times d. Note the introduction of a new digit position
         --  u0 at the left of u1; if d = 1 all we need to do in this step is
         --  to set u0 = 0.

         d := b / (DD (v1) + 1);

         if d = 1 then
            u0 := 0;

         else
            declare
               Carry : DD;
               Tmp   : DD;

            begin
               --  Multiply Dividend (u) by d

               Carry := 0;
               for J in reverse 1 .. m + n loop
                  Tmp   := DD (u (J)) * d + Carry;
                  u (J) := LSD (Tmp);
                  Carry := Tmp / Base;
               end loop;

               u0 := SD (Carry);

               --  Multiply Divisor (v) by d

               Carry := 0;
               for J in reverse 1 .. n loop
                  Tmp   := DD (v (J)) * d + Carry;
                  v (J) := LSD (Tmp);
                  Carry := Tmp / Base;
               end loop;

               pragma Assert (Carry = 0);
            end;
         end if;

         --  D2. [Initialize j.] Set j = 0. The loop on j, steps D2 through D7,
         --  will be essentially a division of (uj, uj+1..uj+n) by (v1,v2..vn)
         --  to get a single quotient digit qj.

         j := 0;

         --  Loop through digits

         loop
            --  Note: In the original printing, step D3 was as follows:

            --  D3. [Calculate qhat.] If uj = v1, set qhat to b-l; otherwise
            --  set qhat to (uj,uj+1)/v1. Now test if v2 * qhat is greater than
            --  (uj*b + uj+1 - qhat*v1)*b + uj+2. If so, decrease qhat by 1 and
            --  repeat this test

            --  This had a bug not discovered till 1995, see Vol 2 errata:
            --  http://www-cs-faculty.stanford.edu/~uno/err2-2e.ps.gz. Under
            --  rare circumstances the expression in the test could overflow.
            --  This version was further corrected in 2005, see Vol 2 errata:
            --  http://www-cs-faculty.stanford.edu/~uno/all2-pre.ps.gz.
            --  The code below is the fixed version of this step.

            --  D3. [Calculate qhat.] Set qhat to (uj,uj+1)/v1 and rhat to
            --  to (uj,uj+1) mod v1.

            temp := u (j) & u (j + 1);
            qhat := temp / DD (v1);
            rhat := temp mod DD (v1);

            --  D3 (continued). Now test if qhat >= b or v2*qhat > (rhat,uj+2):
            --  if so, decrease qhat by 1, increase rhat by v1, and repeat this
            --  test if rhat < b. [The test on v2 determines at high speed
            --  most of the cases in which the trial value qhat is one too
            --  large, and eliminates all cases where qhat is two too large.]

            while qhat >= b
              or else DD (v2) * qhat > LSD (rhat) & u (j + 2)
            loop
               qhat := qhat - 1;
               rhat := rhat + DD (v1);
               exit when rhat >= b;
            end loop;

            --  D4. [Multiply and subtract.] Replace (uj,uj+1..uj+n) by
            --  (uj,uj+1..uj+n) minus qhat times (v1,v2..vn). This step
            --  consists of a simple multiplication by a one-place number,
            --  combined with a subtraction.

            --  The digits (uj,uj+1..uj+n) are always kept positive; if the
            --  result of this step is actually negative then (uj,uj+1..uj+n)
            --  is left as the true value plus b**(n+1), i.e. as the b's
            --  complement of the true value, and a "borrow" to the left is
            --  remembered.

            declare
               Borrow : SD;
               Carry  : DD;
               Temp   : DD;

               Negative : Boolean;
               --  Records if subtraction causes a negative result, requiring
               --  an add back (case where qhat turned out to be 1 too large).

            begin
               Borrow := 0;
               for K in reverse 1 .. n loop
                  Temp := qhat * DD (v (K)) + DD (Borrow);
                  Borrow := MSD (Temp);

                  if LSD (Temp) > u (j + K) then
                     Borrow := Borrow + 1;
                  end if;

                  u (j + K) := u (j + K) - LSD (Temp);
               end loop;

               Negative := u (j) < Borrow;
               u (j) := u (j) - Borrow;

               --  D5. [Test remainder.] Set qj = qhat. If the result of step
               --  D4 was negative, we will do the add back step (step D6).

               q (j) := LSD (qhat);

               if Negative then

                  --  D6. [Add back.] Decrease qj by 1, and add (0,v1,v2..vn)
                  --  to (uj,uj+1,uj+2..uj+n). (A carry will occur to the left
                  --  of uj, and it is be ignored since it cancels with the
                  --  borrow that occurred in D4.)

                  q (j) := q (j) - 1;

                  Carry := 0;
                  for K in reverse 1 .. n loop
                     Temp := DD (v (K)) + DD (u (j + K)) + Carry;
                     u (j + K) := LSD (Temp);
                     Carry := Temp / Base;
                  end loop;

                  u (j) := u (j) + SD (Carry);
               end if;
            end;

            --  D7. [Loop on j.] Increase j by one. Now if j <= m, go back to
            --  D3 (the start of the loop on j).

            j := j + 1;
            exit when not (j <= m);
         end loop;

         --  D8. [Unnormalize.] Now (qo,ql..qm) is the desired quotient, and
         --  the desired remainder may be obtained by dividing (um+1..um+n)
         --  by d.

         if not Discard_Quotient then
            Quotient := Normalize (q);
         end if;

         if not Discard_Remainder then
            declare
               Remdr : DD;

            begin
               Remdr := 0;
               for K in 1 .. n loop
                  Remdr := Base * Remdr + DD (u (m + K));
                  r (K) := SD (Remdr / d);
                  Remdr := Remdr rem d;
               end loop;

               pragma Assert (Remdr = 0);
            end;

            Remainder := Normalize (r);
         end if;
      end Algorithm_D;
   end Div_Rem;

   -----------------
   -- From_Bignum --
   -----------------

   function From_Bignum (X : Bignum) return Long_Long_Integer is
   begin
      if X.Len = 0 then
         return 0;

      elsif X.Len = 1 then
         return (if X.Neg then -LLI (X.D (1)) else LLI (X.D (1)));

      elsif X.Len = 2 then
         declare
            Mag : constant DD := X.D (1) & X.D (2);
         begin
            if X.Neg and then Mag <= 2 ** 63 then
               return -LLI (Mag);
            elsif Mag < 2 ** 63 then
               return LLI (Mag);
            end if;
         end;
      end if;

      raise Constraint_Error with "expression value out of range";
   end From_Bignum;

   -------------------------
   -- Bignum_In_LLI_Range --
   -------------------------

   function Bignum_In_LLI_Range (X : Bignum) return Boolean is
   begin
      --  If length is 0 or 1, definitely fits

      if X.Len <= 1 then
         return True;

      --  If length is greater than 2, definitely does not fit

      elsif X.Len > 2 then
         return False;

      --  Length is 2, more tests needed

      else
         declare
            Mag : constant DD := X.D (1) & X.D (2);
         begin
            return Mag < 2 ** 63 or else (X.Neg and then Mag = 2 ** 63);
         end;
      end if;
   end Bignum_In_LLI_Range;

   ---------------
   -- Normalize --
   ---------------

   function Normalize
     (X   : Digit_Vector;
      Neg : Boolean := False) return Bignum
   is
      B : Bignum;
      J : Length;

   begin
      J := X'First;
      while J <= X'Last and then X (J) = 0 loop
         J := J + 1;
      end loop;

      B := Allocate_Bignum (X'Last - J + 1);
      B.Neg := B.Len > 0 and then Neg;
      B.D := X (J .. X'Last);
      return B;
   end Normalize;

   ---------------
   -- To_Bignum --
   ---------------

   function To_Bignum (X : Long_Long_Integer) return Bignum is
      R : Bignum;

   begin
      if X = 0 then
         R := Allocate_Bignum (0);

      --  One word result

      elsif X in -(2 ** 32 - 1) .. +(2 ** 32 - 1) then
         R := Allocate_Bignum (1);
         R.D (1) := SD (abs (X));

      --  Largest negative number annoyance

      elsif X = Long_Long_Integer'First then
         R := Allocate_Bignum (2);
         R.D (1) := 2 ** 31;
         R.D (2) := 0;

      --  Normal two word case

      else
         R := Allocate_Bignum (2);
         R.D (2) := SD (abs (X) mod Base);
         R.D (1) := SD (abs (X) / Base);
      end if;

      R.Neg := X < 0;
      return R;
   end To_Bignum;

end System.Bignums;
