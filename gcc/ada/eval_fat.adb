------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E V A L _ F A T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Einfo;          use Einfo;
with Einfo.Utils;    use Einfo.Utils;
with Errout;         use Errout;
with Opt;            use Opt;
with Sem_Util;       use Sem_Util;

package body Eval_Fat is

   Radix : constant Int := 2;
   --  This code is currently only correct for the radix 2 case. We use the
   --  symbolic value Radix where possible to help in the unlikely case of
   --  anyone ever having to adjust this code for another value, and for
   --  documentation purposes.

   --  Another assumption is that the range of the floating-point type is
   --  symmetric around zero.

   type Radix_Power_Table is array (Int range 1 .. 4) of Int;

   Radix_Powers : constant Radix_Power_Table :=
     (Radix ** 1, Radix ** 2, Radix ** 3, Radix ** 4);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Decompose
     (RT       : R;
      X        : T;
      Fraction : out T;
      Exponent : out UI;
      Mode     : Rounding_Mode := Round);
   --  Decomposes a non-zero floating-point number into fraction and exponent
   --  parts. The fraction is in the interval 1.0 / Radix .. T'Pred (1.0) and
   --  uses Rbase = Radix. The result is rounded to a nearest machine number.

   --------------
   -- Adjacent --
   --------------

   function Adjacent (RT : R; X, Towards : T) return T is
   begin
      if Towards = X then
         return X;
      elsif Towards > X then
         return Succ (RT, X);
      else
         return Pred (RT, X);
      end if;
   end Adjacent;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (RT : R; X : T) return T is
      XT : constant T := Truncation (RT, X);
   begin
      if UR_Is_Negative (X) then
         return XT;
      elsif X = XT then
         return X;
      else
         return XT + Ureal_1;
      end if;
   end Ceiling;

   -------------
   -- Compose --
   -------------

   function Compose (RT : R; Fraction : T; Exponent : UI) return T is
      Arg_Frac : T;
      Arg_Exp  : UI;
      pragma Warnings (Off, Arg_Exp);
   begin
      Decompose (RT, Fraction, Arg_Frac, Arg_Exp);
      return Scaling (RT, Arg_Frac, Exponent);
   end Compose;

   ---------------
   -- Copy_Sign --
   ---------------

   function Copy_Sign (RT : R; Value, Sign : T) return T is
      pragma Warnings (Off, RT);
      Result : T;

   begin
      Result := abs Value;

      if UR_Is_Negative (Sign) then
         return -Result;
      else
         return Result;
      end if;
   end Copy_Sign;

   ---------------
   -- Decompose --
   ---------------

   procedure Decompose
     (RT       : R;
      X        : T;
      Fraction : out T;
      Exponent : out UI;
      Mode     : Rounding_Mode := Round)
   is
      Int_F : UI;

   begin
      Decompose_Int (RT, abs X, Int_F, Exponent, Mode);

      Fraction := UR_From_Components
       (Num      => Int_F,
        Den      => Machine_Mantissa_Value (RT),
        Rbase    => Radix,
        Negative => False);

      if UR_Is_Negative (X) then
         Fraction := -Fraction;
      end if;

      return;
   end Decompose;

   -------------------
   -- Decompose_Int --
   -------------------

   --  This procedure should be modified with care, as there are many non-
   --  obvious details that may cause problems that are hard to detect. For
   --  zero arguments, Fraction and Exponent are set to zero. Note that sign
   --  of zero cannot be preserved.

   procedure Decompose_Int
     (RT       : R;
      X        : T;
      Fraction : out UI;
      Exponent : out UI;
      Mode     : Rounding_Mode)
   is
      Base : Int := Rbase (X);
      N    : UI  := abs Numerator (X);
      D    : UI  := Denominator (X);

      N_Times_Radix : UI;

      Even : Boolean;
      --  True iff Fraction is even

      Most_Significant_Digit : constant UI :=
        Radix ** (Machine_Mantissa_Value (RT) - 1);

      Uintp_Mark : Uintp.Save_Mark;
      --  The code is divided into blocks that systematically release
      --  intermediate values (this routine generates lots of junk).

   begin
      if N = Uint_0 then
         Fraction := Uint_0;
         Exponent := Uint_0;
         return;
      end if;

      Calculate_D_And_Exponent_1 : begin
         Uintp_Mark := Mark;
         Exponent := Uint_0;

         --  In cases where Base > 1, the actual denominator is Base**D. For
         --  cases where Base is a power of Radix, use the value 1 for the
         --  Denominator and adjust the exponent.

         --  Note: Exponent has different sign from D, because D is a divisor

         for Power in 1 .. Radix_Powers'Last loop
            if Base = Radix_Powers (Power) then
               Exponent := -D * Power;
               Base := 0;
               D := Uint_1;
               exit;
            end if;
         end loop;

         Release_And_Save (Uintp_Mark, D, Exponent);
      end Calculate_D_And_Exponent_1;

      if Base > 0 then
         Calculate_Exponent : begin
            Uintp_Mark := Mark;

            --  For bases that are a multiple of the Radix, divide the base by
            --  Radix and adjust the Exponent. This will help because D will be
            --  much smaller and faster to process.

            --  This occurs for decimal bases on machines with binary floating-
            --  point for example. When calculating 1E40, with Radix = 2, N
            --  will be 93 bits instead of 133.

            --        N            E
            --      ------  * Radix
            --           D
            --       Base

            --                  N                        E
            --    =  --------------------------  *  Radix
            --                     D        D
            --         (Base/Radix)  * Radix

            --             N                  E-D
            --    =  ---------------  *  Radix
            --                    D
            --        (Base/Radix)

            --  This code is commented out, because it causes numerous
            --  failures in the regression suite. To be studied ???

            while False and then Base > 0 and then Base mod Radix = 0 loop
               Base := Base / Radix;
               Exponent := Exponent + D;
            end loop;

            Release_And_Save (Uintp_Mark, Exponent);
         end Calculate_Exponent;

         --  For remaining bases we must actually compute the exponentiation

         --  Because the exponentiation can be negative, and D must be integer,
         --  the numerator is corrected instead.

         Calculate_N_And_D : begin
            Uintp_Mark := Mark;

            if D < 0 then
               N := N * Base ** (-D);
               D := Uint_1;
            else
               D := Base ** D;
            end if;

            Release_And_Save (Uintp_Mark, N, D);
         end Calculate_N_And_D;

         Base := 0;
      end if;

      --  Now scale N and D so that N / D is a value in the interval [1.0 /
      --  Radix, 1.0) and adjust Exponent accordingly, so the value N / D *
      --  Radix ** Exponent remains unchanged.

      --  Step 1 - Adjust N so N / D >= 1 / Radix, or N = 0

      --  N and D are positive, so N / D >= 1 / Radix implies N * Radix >= D.
      --  As this scaling is not possible for N is Uint_0, zero is handled
      --  explicitly at the start of this subprogram.

      Calculate_N_And_Exponent : begin
         Uintp_Mark := Mark;

         N_Times_Radix := N * Radix;
         while not (N_Times_Radix >= D) loop
            N := N_Times_Radix;
            Exponent := Exponent - 1;
            N_Times_Radix := N * Radix;
         end loop;

         Release_And_Save (Uintp_Mark, N, Exponent);
      end Calculate_N_And_Exponent;

      --  Step 2 - Adjust D so N / D < 1

      --  Scale up D so N / D < 1, so N < D

      Calculate_D_And_Exponent_2 : begin
         Uintp_Mark := Mark;

         while not (N < D) loop

            --  As N / D >= 1, N / (D * Radix) will be at least 1 / Radix, so
            --  the result of Step 1 stays valid

            D := D * Radix;
            Exponent := Exponent + 1;
         end loop;

         Release_And_Save (Uintp_Mark, D, Exponent);
      end Calculate_D_And_Exponent_2;

      --  Here the value N / D is in the range [1.0 / Radix .. 1.0)

      --  Now find the fraction by doing a very simple-minded division until
      --  enough digits have been computed.

      --  This division works for all radices, but is only efficient for a
      --  binary radix. It is just like a manual division algorithm, but
      --  instead of moving the denominator one digit right, we move the
      --  numerator one digit left so the numerator and denominator remain
      --  integral.

      Fraction := Uint_0;
      Even := True;

      Calculate_Fraction_And_N : begin
         Uintp_Mark := Mark;

         loop
            while N >= D loop
               N := N - D;
               Fraction := Fraction + 1;
               Even := not Even;
            end loop;

            --  Stop when the result is in [1.0 / Radix, 1.0)

            exit when Fraction >= Most_Significant_Digit;

            N := N * Radix;
            Fraction := Fraction * Radix;
            Even := True;
         end loop;

         Release_And_Save (Uintp_Mark, Fraction, N);
      end Calculate_Fraction_And_N;

      Calculate_Fraction_And_Exponent : begin
         Uintp_Mark := Mark;

         --  Determine correct rounding based on the remainder which is in
         --  N and the divisor D. The rounding is performed on the absolute
         --  value of X, so Ceiling and Floor need to check for the sign of
         --  X explicitly.

         case Mode is
            when Round_Even =>

               --  This rounding mode corresponds to the unbiased rounding
               --  method that is used at run time. When the real value is
               --  exactly between two machine numbers, choose the machine
               --  number with its least significant bit equal to zero.

               --  The recommendation advice in RM 4.9(38) is that static
               --  expressions are rounded to machine numbers in the same
               --  way as the target machine does.

               if (Even and then N * 2 > D)
                     or else
                  (not Even and then N * 2 >= D)
               then
                  Fraction := Fraction + 1;
               end if;

            when Round =>

               --  Do not round to even as is done with IEEE arithmetic, but
               --  instead round away from zero when the result is exactly
               --  between two machine numbers. This biased rounding method
               --  should not be used to convert static expressions to
               --  machine numbers, see AI95-268.

               if N * 2 >= D then
                  Fraction := Fraction + 1;
               end if;

            when Ceiling =>
               if N > Uint_0 and then not UR_Is_Negative (X) then
                  Fraction := Fraction + 1;
               end if;

            when Floor =>
               if N > Uint_0 and then UR_Is_Negative (X) then
                  Fraction := Fraction + 1;
               end if;
         end case;

         --  The result must be normalized to [1.0/Radix, 1.0), so adjust if
         --  the result is 1.0 because of rounding.

         if Fraction = Most_Significant_Digit * Radix then
            Fraction := Most_Significant_Digit;
            Exponent := Exponent + 1;
         end if;

         --  Put back sign after applying the rounding

         if UR_Is_Negative (X) then
            Fraction := -Fraction;
         end if;

         Release_And_Save (Uintp_Mark, Fraction, Exponent);
      end Calculate_Fraction_And_Exponent;
   end Decompose_Int;

   --------------
   -- Exponent --
   --------------

   function Exponent (RT : R; X : T) return UI is
      X_Frac : UI;
      X_Exp  : UI;
      pragma Warnings (Off, X_Frac);
   begin
      Decompose_Int (RT, X, X_Frac, X_Exp, Round_Even);
      return X_Exp;
   end Exponent;

   -----------
   -- Floor --
   -----------

   function Floor (RT : R; X : T) return T is
      XT : constant T := Truncation (RT, X);

   begin
      if UR_Is_Positive (X) then
         return XT;

      elsif XT = X then
         return X;

      else
         return XT - Ureal_1;
      end if;
   end Floor;

   --------------
   -- Fraction --
   --------------

   function Fraction (RT : R; X : T) return T is
      X_Frac : T;
      X_Exp  : UI;
      pragma Warnings (Off, X_Exp);
   begin
      Decompose (RT, X, X_Frac, X_Exp);
      return X_Frac;
   end Fraction;

   ------------------
   -- Leading_Part --
   ------------------

   function Leading_Part (RT : R; X : T; Radix_Digits : UI) return T is
      RD : constant UI := UI_Min (Radix_Digits, Machine_Mantissa_Value (RT));
      L  : UI;
      Y  : T;
   begin
      L := Exponent (RT, X) - RD;
      Y := UR_From_Uint (UR_Trunc (Scaling (RT, X, -L)));
      return Scaling (RT, Y, L);
   end Leading_Part;

   -------------
   -- Machine --
   -------------

   function Machine
     (RT    : R;
      X     : T;
      Mode  : Rounding_Mode;
      Enode : Node_Id) return T
   is
      X_Frac : T;
      X_Exp  : UI;
      Emin   : constant UI := Machine_Emin_Value (RT);

   begin
      Decompose (RT, X, X_Frac, X_Exp, Mode);

      --  Case of denormalized number or (gradual) underflow

      --  A denormalized number is one with the minimum exponent Emin, but that
      --  breaks the assumption that the first digit of the mantissa is a one.
      --  This allows the first non-zero digit to be in any of the remaining
      --  Mant - 1 spots. The gap between subsequent denormalized numbers is
      --  the same as for the smallest normalized numbers. However, the number
      --  of significant digits left decreases as a result of the mantissa now
      --  having leading seros.

      if X_Exp < Emin then
         declare
            Emin_Den : constant UI := Machine_Emin_Value (RT) -
                                        Machine_Mantissa_Value (RT) + Uint_1;

         begin
            --  Do not issue warnings about underflows in GNATprove mode,
            --  as calling Machine as part of interval checking may lead
            --  to spurious warnings.

            if X_Exp < Emin_Den or not Has_Denormals (RT) then
               if Has_Signed_Zeros (RT) and then UR_Is_Negative (X) then
                  if not GNATprove_Mode then
                     Error_Msg_N
                       ("floating-point value underflows to -0.0??", Enode);
                  end if;

                  return Ureal_M_0;

               else
                  if not GNATprove_Mode then
                     Error_Msg_N
                       ("floating-point value underflows to 0.0??", Enode);
                  end if;

                  return Ureal_0;
               end if;

            elsif Has_Denormals (RT) then

               --  Emin - Mant <= X_Exp < Emin, so result is denormal. Handle
               --  gradual underflow by first computing the number of
               --  significant bits still available for the mantissa and
               --  then truncating the fraction to this number of bits.

               --  If this value is different from the original fraction,
               --  precision is lost due to gradual underflow.

               --  We probably should round here and prevent double rounding as
               --  a result of first rounding to a model number and then to a
               --  machine number. However, this is an extremely rare case that
               --  is not worth the extra complexity. In any case, a warning is
               --  issued in cases where gradual underflow occurs.

               declare
                  Denorm_Sig_Bits : constant UI := X_Exp - Emin_Den + 1;

                  X_Frac_Denorm   : constant T := UR_From_Components
                    (UR_Trunc (Scaling (RT, abs X_Frac, Denorm_Sig_Bits)),
                     Denorm_Sig_Bits,
                     Radix,
                     UR_Is_Negative (X));

               begin
                  --  Do not issue warnings about loss of precision in
                  --  GNATprove mode, as calling Machine as part of interval
                  --  checking may lead to spurious warnings.

                  if X_Frac_Denorm /= X_Frac then
                     if not GNATprove_Mode then
                        Error_Msg_N
                          ("gradual underflow causes loss of precision??",
                           Enode);
                     end if;
                     X_Frac := X_Frac_Denorm;
                  end if;
               end;
            end if;
         end;
      end if;

      return Scaling (RT, X_Frac, X_Exp);
   end Machine;

   -----------
   -- Model --
   -----------

   function Model (RT : R; X : T) return T is
      X_Frac : T;
      X_Exp  : UI;
   begin
      Decompose (RT, X, X_Frac, X_Exp);
      return Compose (RT, X_Frac, X_Exp);
   end Model;

   ----------
   -- Pred --
   ----------

   function Pred (RT : R; X : T) return T is
   begin
      return -Succ (RT, -X);
   end Pred;

   ---------------
   -- Remainder --
   ---------------

   function Remainder (RT : R; X, Y : T) return T is
      A        : T;
      B        : T;
      Arg      : T;
      P        : T;
      Arg_Frac : T;
      P_Frac   : T;
      Sign_X   : T;
      IEEE_Rem : T;
      Arg_Exp  : UI;
      P_Exp    : UI;
      K        : UI;
      P_Even   : Boolean;

      pragma Warnings (Off, Arg_Frac);

   begin
      if UR_Is_Positive (X) then
         Sign_X :=  Ureal_1;
      else
         Sign_X := -Ureal_1;
      end if;

      Arg := abs X;
      P   := abs Y;

      if Arg < P then
         P_Even := True;
         IEEE_Rem := Arg;
         P_Exp := Exponent (RT, P);

      else
         --  ??? what about zero cases?
         Decompose (RT, Arg, Arg_Frac, Arg_Exp);
         Decompose (RT, P,   P_Frac,   P_Exp);

         P := Compose (RT, P_Frac, Arg_Exp);
         K := Arg_Exp - P_Exp;
         P_Even := True;
         IEEE_Rem := Arg;

         for Cnt in reverse 0 .. UI_To_Int (K) loop
            if IEEE_Rem >= P then
               P_Even := False;
               IEEE_Rem := IEEE_Rem - P;
            else
               P_Even := True;
            end if;

            P := P * Ureal_Half;
         end loop;
      end if;

      --  That completes the calculation of modulus remainder. The final step
      --  is get the IEEE remainder. Here we compare Rem with (abs Y) / 2.

      if P_Exp >= 0 then
         A := IEEE_Rem;
         B := abs Y * Ureal_Half;

      else
         A := IEEE_Rem * Ureal_2;
         B := abs Y;
      end if;

      if A > B or else (A = B and then not P_Even) then
         IEEE_Rem := IEEE_Rem - abs Y;
      end if;

      return Sign_X * IEEE_Rem;
   end Remainder;

   --------------
   -- Rounding --
   --------------

   function Rounding (RT : R; X : T) return T is
      Result : T;
      Tail   : T;

   begin
      Result := Truncation (RT, abs X);
      Tail   := abs X - Result;

      if Tail >= Ureal_Half then
         Result := Result + Ureal_1;
      end if;

      if UR_Is_Negative (X) then
         return -Result;
      else
         return Result;
      end if;
   end Rounding;

   -------------
   -- Scaling --
   -------------

   function Scaling (RT : R; X : T; Adjustment : UI) return T is
      pragma Warnings (Off, RT);

   begin
      if Rbase (X) = Radix then
         return UR_From_Components
           (Num      => Numerator (X),
            Den      => Denominator (X) - Adjustment,
            Rbase    => Radix,
            Negative => UR_Is_Negative (X));

      elsif Adjustment >= 0 then
         return X * Radix ** Adjustment;
      else
         return X / Radix ** (-Adjustment);
      end if;
   end Scaling;

   ----------
   -- Succ --
   ----------

   function Succ (RT : R; X : T) return T is
      Emin     : constant UI := Machine_Emin_Value (RT);
      Mantissa : constant UI := Machine_Mantissa_Value (RT);
      Exp      : UI := UI_Max (Emin, Exponent (RT, X));
      Frac     : T;
      New_Frac : T;

   begin
      --  Treat zero as a regular denormalized number if they are supported,
      --  otherwise return the smallest normalized number.

      if UR_Is_Zero (X) then
         if Has_Denormals (RT) then
            Exp := Emin;
         else
            return Scaling (RT, Ureal_Half, Emin);
         end if;
      end if;

      --  Multiply the number by 2.0**(Mantissa-Exp) so that the radix point
      --  will be directly following the mantissa after scaling.

      Exp := Exp - Mantissa;
      Frac := Scaling (RT, X, -Exp);

      --  Round to the neareast integer towards +Inf

      New_Frac := Ceiling (RT, Frac);

      --  If the rounding was a NOP, add one, except for -2.0**(Mantissa-1)
      --  because the exponent is going to be reduced.

      if New_Frac = Frac then
         if New_Frac = Scaling (RT, -Ureal_1, Mantissa - 1) then
            New_Frac := New_Frac + Ureal_Half;
         else
            New_Frac := New_Frac + Ureal_1;
         end if;
      end if;

      --  Divide back by 2.0**(Mantissa-Exp) to get the final result

      return Scaling (RT, New_Frac, Exp);
   end Succ;

   ----------------
   -- Truncation --
   ----------------

   function Truncation (RT : R; X : T) return T is
      pragma Warnings (Off, RT);
   begin
      return UR_From_Uint (UR_Trunc (X));
   end Truncation;

   -----------------------
   -- Unbiased_Rounding --
   -----------------------

   function Unbiased_Rounding (RT : R; X : T) return T is
      Abs_X  : constant T := abs X;
      Result : T;
      Tail   : T;

   begin
      Result := Truncation (RT, Abs_X);
      Tail   := Abs_X - Result;

      if Tail > Ureal_Half then
         Result := Result + Ureal_1;

      elsif Tail = Ureal_Half then
         Result := Ureal_2 *
                     Truncation (RT, (Result / Ureal_2) + Ureal_Half);
      end if;

      if UR_Is_Negative (X) then
         return -Result;
      elsif UR_Is_Positive (X) then
         return Result;

      --  For zero case, make sure sign of zero is preserved

      else
         return X;
      end if;
   end Unbiased_Rounding;

end Eval_Fat;
