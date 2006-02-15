------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . A R I T H _ 6 4                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Pure_Exceptions; use System.Pure_Exceptions;

with Interfaces; use Interfaces;
with Unchecked_Conversion;

package body System.Arith_64 is

   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);

   subtype Uns64 is Unsigned_64;
   function To_Uns is new Unchecked_Conversion (Int64, Uns64);
   function To_Int is new Unchecked_Conversion (Uns64, Int64);

   subtype Uns32 is Unsigned_32;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function "+" (A, B : Uns32) return Uns64;
   function "+" (A : Uns64; B : Uns32) return Uns64;
   pragma Inline ("+");
   --  Length doubling additions

   function "*" (A, B : Uns32) return Uns64;
   pragma Inline ("*");
   --  Length doubling multiplication

   function "/" (A : Uns64; B : Uns32) return Uns64;
   pragma Inline ("/");
   --  Length doubling division

   function "rem" (A : Uns64; B : Uns32) return Uns64;
   pragma Inline ("rem");
   --  Length doubling remainder

   function "&" (Hi, Lo : Uns32) return Uns64;
   pragma Inline ("&");
   --  Concatenate hi, lo values to form 64-bit result

   function Le3 (X1, X2, X3 : Uns32; Y1, Y2, Y3 : Uns32) return Boolean;
   --  Determines if 96 bit value X1&X2&X3 <= Y1&Y2&Y3

   function Lo (A : Uns64) return Uns32;
   pragma Inline (Lo);
   --  Low order half of 64-bit value

   function Hi (A : Uns64) return Uns32;
   pragma Inline (Hi);
   --  High order half of 64-bit value

   procedure Sub3 (X1, X2, X3 : in out Uns32; Y1, Y2, Y3 : Uns32);
   --  Computes X1&X2&X3 := X1&X2&X3 - Y1&Y1&Y3 with mod 2**96 wrap

   function To_Neg_Int (A : Uns64) return Int64;
   --  Convert to negative integer equivalent. If the input is in the range
   --  0 .. 2 ** 63, then the corresponding negative signed integer (obtained
   --  by negating the given value) is returned, otherwise constraint error
   --  is raised.

   function To_Pos_Int (A : Uns64) return Int64;
   --  Convert to positive integer equivalent. If the input is in the range
   --  0 .. 2 ** 63-1, then the corresponding non-negative signed integer is
   --  returned, otherwise constraint error is raised.

   procedure Raise_Error;
   pragma No_Return (Raise_Error);
   --  Raise constraint error with appropriate message

   ---------
   -- "&" --
   ---------

   function "&" (Hi, Lo : Uns32) return Uns64 is
   begin
      return Shift_Left (Uns64 (Hi), 32) or Uns64 (Lo);
   end "&";

   ---------
   -- "*" --
   ---------

   function "*" (A, B : Uns32) return Uns64 is
   begin
      return Uns64 (A) * Uns64 (B);
   end "*";

   ---------
   -- "+" --
   ---------

   function "+" (A, B : Uns32) return Uns64 is
   begin
      return Uns64 (A) + Uns64 (B);
   end "+";

   function "+" (A : Uns64; B : Uns32) return Uns64 is
   begin
      return A + Uns64 (B);
   end "+";

   ---------
   -- "/" --
   ---------

   function "/" (A : Uns64; B : Uns32) return Uns64 is
   begin
      return A / Uns64 (B);
   end "/";

   -----------
   -- "rem" --
   -----------

   function "rem" (A : Uns64; B : Uns32) return Uns64 is
   begin
      return A rem Uns64 (B);
   end "rem";

   --------------------------
   -- Add_With_Ovflo_Check --
   --------------------------

   function Add_With_Ovflo_Check (X, Y : Int64) return Int64 is
      R : constant Int64 := To_Int (To_Uns (X) + To_Uns (Y));

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
     (X, Y, Z : Int64;
      Q, R    : out Int64;
      Round   : Boolean)
   is
      Xu  : constant Uns64 := To_Uns (abs X);
      Yu  : constant Uns64 := To_Uns (abs Y);

      Yhi : constant Uns32 := Hi (Yu);
      Ylo : constant Uns32 := Lo (Yu);

      Zu  : constant Uns64 := To_Uns (abs Z);
      Zhi : constant Uns32 := Hi (Zu);
      Zlo : constant Uns32 := Lo (Zu);

      T1, T2     : Uns64;
      Du, Qu, Ru : Uns64;
      Den_Pos    : Boolean;

   begin
      if Yu = 0 or else Zu = 0 then
         Raise_Error;
      end if;

      --  Compute Y * Z. Note that if the result overflows 64 bits unsigned,
      --  then the rounded result is clearly zero (since the dividend is at
      --  most 2**63 - 1, the extra bit of precision is nice here!)

      if Yhi /= 0 then
         if Zhi /= 0 then
            Q := 0;
            R := X;
            return;
         else
            T2 := Yhi * Zlo;
         end if;

      else
         if Zhi /= 0 then
            T2 := Ylo * Zhi;
         else
            T2 := 0;
         end if;
      end if;

      T1 := Ylo * Zlo;
      T2 := T2 + Hi (T1);

      if Hi (T2) /= 0 then
         Q := 0;
         R := X;
         return;
      end if;

      Du := Lo (T2) & Lo (T1);

      --  Set final signs (RM 4.5.5(27-30))

      Den_Pos := (Y < 0) = (Z < 0);

      --  Check overflow case of largest negative number divided by 1

      if X = Int64'First and then Du = 1 and then not Den_Pos then
         Raise_Error;
      end if;

      --  Perform the actual division

      Qu := Xu / Du;
      Ru := Xu rem Du;

      --  Deal with rounding case

      if Round and then Ru > (Du - Uns64'(1)) / Uns64'(2) then
         Qu := Qu + Uns64'(1);
      end if;

      --  Case of dividend (X) sign positive

      if X >= 0 then
         R := To_Int (Ru);

         if Den_Pos then
            Q := To_Int (Qu);
         else
            Q := -To_Int (Qu);
         end if;

      --  Case of dividend (X) sign negative

      else
         R := -To_Int (Ru);

         if Den_Pos then
            Q := -To_Int (Qu);
         else
            Q := To_Int (Qu);
         end if;
      end if;
   end Double_Divide;

   --------
   -- Hi --
   --------

   function Hi (A : Uns64) return Uns32 is
   begin
      return Uns32 (Shift_Right (A, 32));
   end Hi;

   ---------
   -- Le3 --
   ---------

   function Le3 (X1, X2, X3 : Uns32; Y1, Y2, Y3 : Uns32) return Boolean is
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

   --------
   -- Lo --
   --------

   function Lo (A : Uns64) return Uns32 is
   begin
      return Uns32 (A and 16#FFFF_FFFF#);
   end Lo;

   -------------------------------
   -- Multiply_With_Ovflo_Check --
   -------------------------------

   function Multiply_With_Ovflo_Check (X, Y : Int64) return Int64 is
      Xu  : constant Uns64 := To_Uns (abs X);
      Xhi : constant Uns32 := Hi (Xu);
      Xlo : constant Uns32 := Lo (Xu);

      Yu  : constant Uns64 := To_Uns (abs Y);
      Yhi : constant Uns32 := Hi (Yu);
      Ylo : constant Uns32 := Lo (Yu);

      T1, T2 : Uns64;

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

      --  Here we have T2 set to the contribution to the upper half
      --  of the result from the upper halves of the input values.

      T1 := Xlo * Ylo;
      T2 := T2 + Hi (T1);

      if Hi (T2) /= 0 then
         Raise_Error;
      end if;

      T2 := Lo (T2) & Lo (T1);

      if X >= 0 then
         if Y >= 0 then
            return To_Pos_Int (T2);
         else
            return To_Neg_Int (T2);
         end if;
      else -- X < 0
         if Y < 0 then
            return To_Pos_Int (T2);
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
      Raise_Exception (CE, "64-bit arithmetic overflow");
   end Raise_Error;

   -------------------
   -- Scaled_Divide --
   -------------------

   procedure Scaled_Divide
     (X, Y, Z : Int64;
      Q, R    : out Int64;
      Round   : Boolean)
   is
      Xu  : constant Uns64 := To_Uns (abs X);
      Xhi : constant Uns32 := Hi (Xu);
      Xlo : constant Uns32 := Lo (Xu);

      Yu  : constant Uns64 := To_Uns (abs Y);
      Yhi : constant Uns32 := Hi (Yu);
      Ylo : constant Uns32 := Lo (Yu);

      Zu  : Uns64 := To_Uns (abs Z);
      Zhi : Uns32 := Hi (Zu);
      Zlo : Uns32 := Lo (Zu);

      D : array (1 .. 4) of Uns32;
      --  The dividend, four digits (D(1) is high order)

      Qd : array (1 .. 2) of Uns32;
      --  The quotient digits, two digits (Qd(1) is high order)

      S1, S2, S3 : Uns32;
      --  Value to subtract, three digits (S1 is high order)

      Qu : Uns64;
      Ru : Uns64;
      --  Unsigned quotient and remainder

      Scale : Natural;
      --  Scaling factor used for multiple-precision divide. Dividend and
      --  Divisor are multiplied by 2 ** Scale, and the final remainder
      --  is divided by the scaling factor. The reason for this scaling
      --  is to allow more accurate estimation of quotient digits.

      T1, T2, T3 : Uns64;
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

            T1 := (D (1) & D (2)) + Uns64'(Xhi * Yhi);
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

      --  Now it is time for the dreaded multiple precision division. First
      --  an easy case, check for the simple case of a one digit divisor.

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

      --  If divisor is double digit and too large, raise error

      elsif (D (1) & D (2)) >= Zu then
         Raise_Error;

      --  This is the complex case where we definitely have a double digit
      --  divisor and a dividend of at least three digits. We use the classical
      --  multiple division algorithm (see section (4.3.1) of Knuth's "The Art
      --  of Computer Programming", Vol. 2 for a description (algorithm D).

      else
         --  First normalize the divisor so that it has the leading bit on.
         --  We do this by finding the appropriate left shift amount.

         Scale := 0;

         if (Zhi and 16#FFFF0000#) = 0 then
            Scale := 16;
            Zu := Shift_Left (Zu, 16);
         end if;

         if (Hi (Zu) and 16#FF00_0000#) = 0 then
            Scale := Scale + 8;
            Zu := Shift_Left (Zu, 8);
         end if;

         if (Hi (Zu) and 16#F000_0000#) = 0 then
            Scale := Scale + 4;
            Zu := Shift_Left (Zu, 4);
         end if;

         if (Hi (Zu) and 16#C000_0000#) = 0 then
            Scale := Scale + 2;
            Zu := Shift_Left (Zu, 2);
         end if;

         if (Hi (Zu) and 16#8000_0000#) = 0 then
            Scale := Scale + 1;
            Zu := Shift_Left (Zu, 1);
         end if;

         Zhi := Hi (Zu);
         Zlo := Lo (Zu);

         --  Note that when we scale up the dividend, it still fits in four
         --  digits, since we already tested for overflow, and scaling does
         --  not change the invariant that (D (1) & D (2)) >= Zu.

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

            if D (J + 1) = Zhi then
               Qd (J + 1) := 2 ** 32 - 1;
            else
               Qd (J + 1) := Lo ((D (J + 1) & D (J + 2)) / Zhi);
            end if;

            --  Compute amount to subtract

            T1 := Qd (J + 1) * Zlo;
            T2 := Qd (J + 1) * Zhi;
            S3 := Lo (T1);
            T1 := Hi (T1) + Lo (T2);
            S2 := Lo (T1);
            S1 := Hi (T1) + Hi (T2);

            --  Adjust quotient digit if it was too high

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

      if Round and then Ru > (Zu - Uns64'(1)) / Uns64'(2) then
         Qu := Qu + Uns64 (1);
      end if;

      --  Set final signs (RM 4.5.5(27-30))

      --  Case of dividend (X * Y) sign positive

      if (X >= 0 and then Y >= 0)
        or else (X < 0 and then Y < 0)
      then
         R := To_Pos_Int (Ru);

         if Z > 0 then
            Q := To_Pos_Int (Qu);
         else
            Q := To_Neg_Int (Qu);
         end if;

      --  Case of dividend (X * Y) sign negative

      else
         R := To_Neg_Int (Ru);

         if Z > 0 then
            Q := To_Neg_Int (Qu);
         else
            Q := To_Pos_Int (Qu);
         end if;
      end if;
   end Scaled_Divide;

   ----------
   -- Sub3 --
   ----------

   procedure Sub3 (X1, X2, X3 : in out Uns32; Y1, Y2, Y3 : Uns32) is
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

   function Subtract_With_Ovflo_Check (X, Y : Int64) return Int64 is
      R : constant Int64 := To_Int (To_Uns (X) - To_Uns (Y));

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

   function To_Neg_Int (A : Uns64) return Int64 is
      R : constant Int64 := -To_Int (A);

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

   function To_Pos_Int (A : Uns64) return Int64 is
      R : constant Int64 := To_Int (A);

   begin
      if R >= 0 then
         return R;
      else
         Raise_Error;
      end if;
   end To_Pos_Int;

end System.Arith_64;
