------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . A R I T H _ D O U B L E                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;

package body System.Arith_Double is

   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);

   function To_Uns is new Ada.Unchecked_Conversion (Double_Int, Double_Uns);
   function To_Int is new Ada.Unchecked_Conversion (Double_Uns, Double_Int);

   Double_Size : constant Natural := Double_Int'Size;
   Single_Size : constant Natural := Double_Int'Size / 2;

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
     (A / Double_Uns (B));
   --  Length doubling division

   function "&" (Hi, Lo : Single_Uns) return Double_Uns is
     (Shift_Left (Double_Uns (Hi), Single_Size) or Double_Uns (Lo));
   --  Concatenate hi, lo values to form double result

   function "abs" (X : Double_Int) return Double_Uns is
     (if X = Double_Int'First
      then 2 ** (Double_Size - 1)
      else Double_Uns (Double_Int'(abs X)));
   --  Convert absolute value of X to unsigned. Note that we can't just use
   --  the expression of the Else since it overflows for X = Double_Int'First.

   function "rem" (A : Double_Uns; B : Single_Uns) return Double_Uns is
     (A rem Double_Uns (B));
   --  Length doubling remainder

   function Le3 (X1, X2, X3, Y1, Y2, Y3 : Single_Uns) return Boolean;
   --  Determines if (3 * Single_Size)-bit value X1&X2&X3 <= Y1&Y2&Y3

   function Lo (A : Double_Uns) return Single_Uns is
     (Single_Uns (A and (2 ** Single_Size - 1)));
   --  Low order half of double value

   function Hi (A : Double_Uns) return Single_Uns is
     (Single_Uns (Shift_Right (A, Single_Size)));
   --  High order half of double value

   procedure Sub3 (X1, X2, X3 : in out Single_Uns; Y1, Y2, Y3 : Single_Uns);
   --  Computes X1&X2&X3 := X1&X2&X3 - Y1&Y1&Y3 mod 2 ** (3 * Single_Size)

   function To_Neg_Int (A : Double_Uns) return Double_Int;
   --  Convert to negative integer equivalent. If the input is in the range
   --  0 .. 2 ** (Double_Size - 1), then the corresponding nonpositive signed
   --  integer (obtained by negating the given value) is returned, otherwise
   --  constraint error is raised.

   function To_Pos_Int (A : Double_Uns) return Double_Int;
   --  Convert to positive integer equivalent. If the input is in the range
   --  0 .. 2 ** (Double_Size - 1) - 1, then the corresponding non-negative
   --  signed integer is returned, otherwise constraint error is raised.

   procedure Raise_Error;
   pragma No_Return (Raise_Error);
   --  Raise constraint error with appropriate message

   --------------------------
   -- Add_With_Ovflo_Check --
   --------------------------

   function Add_With_Ovflo_Check (X, Y : Double_Int) return Double_Int is
      R : constant Double_Int := To_Int (To_Uns (X) + To_Uns (Y));

   begin
      if X >= 0 then
         if Y < 0 or else R >= 0 then
            return R;
         end if;

      else -- X < 0
         if Y > 0 or else R < 0 then
            return R;
         end if;
      end if;

      Raise_Error;
   end Add_With_Ovflo_Check;

   -------------------
   -- Double_Divide --
   -------------------

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
      Den_Pos    : Boolean;

   begin
      if Yu = 0 or else Zu = 0 then
         Raise_Error;
      end if;

      --  Set final signs (RM 4.5.5(27-30))

      Den_Pos := (Y < 0) = (Z < 0);

      --  Compute Y * Z. Note that if the result overflows Double_Uns, then
      --  the rounded result is zero, except for the very special case where
      --  X = -2 ** (Double_Size - 1) and abs(Y*Z) = 2 ** Double_Size, when
      --  Round is True.

      if Yhi /= 0 then
         if Zhi /= 0 then

            --  Handle the special case when Round is True

            if Yhi = 1
              and then Zhi = 1
              and then Ylo = 0
              and then Zlo = 0
              and then X = Double_Int'First
              and then Round
            then
               Q := (if Den_Pos then -1 else 1);
            else
               Q := 0;
            end if;

            R := X;
            return;
         else
            T2 := Yhi * Zlo;
         end if;

      else
         T2 := Ylo * Zhi;
      end if;

      T1 := Ylo * Zlo;
      T2 := T2 + Hi (T1);

      if Hi (T2) /= 0 then

         --  Handle the special case when Round is True

         if Hi (T2) = 1
           and then Lo (T2) = 0
           and then Lo (T1) = 0
           and then X = Double_Int'First
           and then Round
         then
            Q := (if Den_Pos then -1 else 1);
         else
            Q := 0;
         end if;

         R := X;
         return;
      end if;

      Du := Lo (T2) & Lo (T1);

      --  Check overflow case of largest negative number divided by -1

      if X = Double_Int'First and then Du = 1 and then not Den_Pos then
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

      --  Deal with rounding case

      if Round and then Ru > (Du - Double_Uns'(1)) / Double_Uns'(2) then
         Qu := Qu + Double_Uns'(1);
      end if;

      --  Case of dividend (X) sign positive

      if X >= 0 then
         R := To_Int (Ru);
         Q := (if Den_Pos then To_Int (Qu) else -To_Int (Qu));

      --  Case of dividend (X) sign negative

      --  We perform the unary minus operation on the unsigned value
      --  before conversion to signed, to avoid a possible overflow
      --  for value -2 ** (Double_Size - 1), both for computing R and Q.

      else
         R := To_Int (-Ru);
         Q := (if Den_Pos then To_Int (-Qu) else To_Int (Qu));
      end if;
   end Double_Divide;

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

   begin
      if Xhi /= 0 then
         if Yhi /= 0 then
            Raise_Error;
         else
            T2 := Xhi * Ylo;
         end if;

      elsif Yhi /= 0 then
         T2 := Xlo * Yhi;

      else -- Yhi = Xhi = 0
         T2 := 0;
      end if;

      --  Here we have T2 set to the contribution to the upper half of the
      --  result from the upper halves of the input values.

      T1 := Xlo * Ylo;
      T2 := T2 + Hi (T1);

      if Hi (T2) /= 0 then
         Raise_Error;
      end if;

      T2 := Lo (T2) & Lo (T1);

      if X >= 0 then
         if Y >= 0 then
            return To_Pos_Int (T2);
            pragma Annotate (CodePeer, Intentional, "precondition",
                             "Intentional Unsigned->Signed conversion");
         else
            return To_Neg_Int (T2);
         end if;
      else -- X < 0
         if Y < 0 then
            return To_Pos_Int (T2);
            pragma Annotate (CodePeer, Intentional, "precondition",
                             "Intentional Unsigned->Signed conversion");
         else
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
   end Raise_Error;

   -------------------
   -- Scaled_Divide --
   -------------------

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

      D : array (1 .. 4) of Single_Uns;
      --  The dividend, four digits (D(1) is high order)

      Qd : array (1 .. 2) of Single_Uns;
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

   begin
      --  First do the multiplication, giving the four digit dividend

      T1 := Xlo * Ylo;
      D (4) := Lo (T1);
      D (3) := Hi (T1);

      if Yhi /= 0 then
         T1 := Xlo * Yhi;
         T2 := D (3) + Lo (T1);
         D (3) := Lo (T2);
         D (2) := Hi (T1) + Hi (T2);

         if Xhi /= 0 then
            T1 := Xhi * Ylo;
            T2 := D (3) + Lo (T1);
            D (3) := Lo (T2);
            T3 := D (2) + Hi (T1);
            T3 := T3 + Hi (T2);
            D (2) := Lo (T3);
            D (1) := Hi (T3);

            T1 := (D (1) & D (2)) + Double_Uns'(Xhi * Yhi);
            D (1) := Hi (T1);
            D (2) := Lo (T1);

         else
            D (1) := 0;
         end if;

      else
         if Xhi /= 0 then
            T1 := Xhi * Ylo;
            T2 := D (3) + Lo (T1);
            D (3) := Lo (T2);
            D (2) := Hi (T1) + Hi (T2);

         else
            D (2) := 0;
         end if;

         D (1) := 0;
      end if;

      --  Now it is time for the dreaded multiple precision division. First an
      --  easy case, check for the simple case of a one digit divisor.

      if Zhi = 0 then
         if D (1) /= 0 or else D (2) >= Zlo then
            Raise_Error;

         --  Here we are dividing at most three digits by one digit

         else
            T1 := D (2) & D (3);
            T2 := Lo (T1 rem Zlo) & D (4);

            Qu := Lo (T1 / Zlo) & Lo (T2 / Zlo);
            Ru := T2 rem Zlo;
         end if;

      --  If divisor is double digit and dividend is too large, raise error

      elsif (D (1) & D (2)) >= Zu then
         Raise_Error;

      --  This is the complex case where we definitely have a double digit
      --  divisor and a dividend of at least three digits. We use the classical
      --  multiple-precision division algorithm (see section (4.3.1) of Knuth's
      --  "The Art of Computer Programming", Vol. 2 for a description
      --  (algorithm D).

      else
         --  First normalize the divisor so that it has the leading bit on.
         --  We do this by finding the appropriate left shift amount.

         Shift := Single_Size / 2;
         Mask  := Shift_Left (2 ** (Single_Size / 2) - 1, Shift);
         Scale := 0;

         while Shift /= 0 loop
            if (Hi (Zu) and Mask) = 0 then
               Scale := Scale + Shift;
               Zu := Shift_Left (Zu, Shift);
            end if;

            Shift := Shift / 2;
            Mask := Shift_Left (Mask, Shift);
         end loop;

         Zhi := Hi (Zu);
         Zlo := Lo (Zu);

         pragma Assert (Zhi /= 0);
         --  We have Hi(Zu)/=0 before normalization. The sequence of Shift_Left
         --  operations results in the leading bit of Zu being 1 by moving the
         --  leftmost 1-bit in Zu to leading position, thus Zhi=Hi(Zu)/=0 here.

         --  Note that when we scale up the dividend, it still fits in four
         --  digits, since we already tested for overflow, and scaling does
         --  not change the invariant that (D (1) & D (2)) < Zu.

         T1 := Shift_Left (D (1) & D (2), Scale);
         D (1) := Hi (T1);
         T2 := Shift_Left (0 & D (3), Scale);
         D (2) := Lo (T1) or Hi (T2);
         T3 := Shift_Left (0 & D (4), Scale);
         D (3) := Lo (T2) or Hi (T3);
         D (4) := Lo (T3);

         --  Loop to compute quotient digits, runs twice for Qd(1) and Qd(2)

         for J in 0 .. 1 loop

            --  Compute next quotient digit. We have to divide three digits by
            --  two digits. We estimate the quotient by dividing the leading
            --  two digits by the leading digit. Given the scaling we did above
            --  which ensured the first bit of the divisor is set, this gives
            --  an estimate of the quotient that is at most two too high.

            Qd (J + 1) := (if D (J + 1) = Zhi
                           then 2 ** Single_Size - 1
                           else Lo ((D (J + 1) & D (J + 2)) / Zhi));

            --  Compute amount to subtract

            T1 := Qd (J + 1) * Zlo;
            T2 := Qd (J + 1) * Zhi;
            S3 := Lo (T1);
            T1 := Hi (T1) + Lo (T2);
            S2 := Lo (T1);
            S1 := Hi (T1) + Hi (T2);

            --  Adjust quotient digit if it was too high

            --  We use the version of the algorithm in the 2nd Edition of
            --  "The Art of Computer Programming". This had a bug not
            --  discovered till 1995, see Vol 2 errata:
            --     http://www-cs-faculty.stanford.edu/~uno/err2-2e.ps.gz.
            --  Under rare circumstances the expression in the test could
            --  overflow. This version was further corrected in 2005, see
            --  Vol 2 errata:
            --     http://www-cs-faculty.stanford.edu/~uno/all2-pre.ps.gz.
            --  This implementation is not impacted by these bugs, due to the
            --  use of a word-size comparison done in function Le3 instead of
            --  a comparison on two-word integer quantities in the original
            --  algorithm.

            loop
               exit when Le3 (S1, S2, S3, D (J + 1), D (J + 2), D (J + 3));
               Qd (J + 1) := Qd (J + 1) - 1;
               Sub3 (S1, S2, S3, 0, Zhi, Zlo);
            end loop;

            --  Now subtract S1&S2&S3 from D1&D2&D3 ready for next step

            Sub3 (D (J + 1), D (J + 2), D (J + 3), S1, S2, S3);
         end loop;

         --  The two quotient digits are now set, and the remainder of the
         --  scaled division is in D3&D4. To get the remainder for the
         --  original unscaled division, we rescale this dividend.

         --  We rescale the divisor as well, to make the proper comparison
         --  for rounding below.

         Qu := Qd (1) & Qd (2);
         Ru := Shift_Right (D (3) & D (4), Scale);
         Zu := Shift_Right (Zu, Scale);
      end if;

      --  Deal with rounding case

      if Round and then Ru > (Zu - Double_Uns'(1)) / Double_Uns'(2) then

         --  Protect against wrapping around when rounding, by signaling
         --  an overflow when the quotient is too large.

         if Qu = Double_Uns'Last then
            Raise_Error;
         end if;

         Qu := Qu + Double_Uns'(1);
      end if;

      --  Set final signs (RM 4.5.5(27-30))

      --  Case of dividend (X * Y) sign positive

      if (X >= 0 and then Y >= 0) or else (X < 0 and then Y < 0) then
         R := To_Pos_Int (Ru);
         Q := (if Z > 0 then To_Pos_Int (Qu) else To_Neg_Int (Qu));

      --  Case of dividend (X * Y) sign negative

      else
         R := To_Neg_Int (Ru);
         Q := (if Z > 0 then To_Neg_Int (Qu) else To_Pos_Int (Qu));
      end if;
   end Scaled_Divide;

   ----------
   -- Sub3 --
   ----------

   procedure Sub3 (X1, X2, X3 : in out Single_Uns; Y1, Y2, Y3 : Single_Uns) is
   begin
      if Y3 > X3 then
         if X2 = 0 then
            X1 := X1 - 1;
         end if;

         X2 := X2 - 1;
      end if;

      X3 := X3 - Y3;

      if Y2 > X2 then
         X1 := X1 - 1;
      end if;

      X2 := X2 - Y2;
      X1 := X1 - Y1;
   end Sub3;

   -------------------------------
   -- Subtract_With_Ovflo_Check --
   -------------------------------

   function Subtract_With_Ovflo_Check (X, Y : Double_Int) return Double_Int is
      R : constant Double_Int := To_Int (To_Uns (X) - To_Uns (Y));

   begin
      if X >= 0 then
         if Y > 0 or else R >= 0 then
            return R;
         end if;

      else -- X < 0
         if Y <= 0 or else R < 0 then
            return R;
         end if;
      end if;

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

end System.Arith_Double;
