------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ R E A L                       --
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

with System.Double_Real;
with System.Float_Control;
with System.Unsigned_Types; use System.Unsigned_Types;
with System.Val_Util;       use System.Val_Util;
with System.Value_R;

pragma Warnings (Off, "non-static constant in preelaborated unit");
--  Every constant is static given our instantiation model

package body System.Val_Real is

   pragma Assert (Num'Machine_Mantissa <= Uns'Size);
   --  We need an unsigned type large enough to represent the mantissa

   Need_Extra : constant Boolean := Num'Machine_Mantissa > Uns'Size - 4;
   --  If the mantissa of the floating-point type is almost as large as the
   --  unsigned type, we do not have enough space for an extra digit in the
   --  unsigned type so we handle the extra digit separately, at the cost of
   --  a bit more work in Integer_to_Real.

   Precision_Limit : constant Uns :=
     (if Need_Extra then 2**Num'Machine_Mantissa - 1 else 2**Uns'Size - 1);
   --  If we handle the extra digit separately, we use the precision of the
   --  floating-point type so that the conversion is exact.

   package Impl is new Value_R (Uns, Precision_Limit, Round => Need_Extra);

   subtype Base_T is Unsigned range 2 .. 16;

   --  The following tables compute the maximum exponent of the base that can
   --  fit in the given floating-point format, that is to say the element at
   --  index N is the largest K such that N**K <= Num'Last.

   Maxexp32 : constant array (Base_T) of Positive :=
     (2  => 127, 3 => 80,  4 => 63,  5 => 55,  6 => 49,
      7  => 45,  8 => 42,  9 => 40, 10 => 38, 11 => 37,
      12 => 35, 13 => 34, 14 => 33, 15 => 32, 16 => 31);

   Maxexp64 : constant array (Base_T) of Positive :=
     (2  => 1023, 3 => 646,  4 => 511,  5 => 441,  6 => 396,
      7  => 364,  8 => 341,  9 => 323, 10 => 308, 11 => 296,
      12 => 285, 13 => 276, 14 => 268, 15 => 262, 16 => 255);

   Maxexp80 : constant array (Base_T) of Positive :=
     (2  => 16383, 3 => 10337, 4 => 8191,  5 => 7056,  6 => 6338,
      7  => 5836,  8 => 5461,  9 => 5168, 10 => 4932, 11 => 4736,
      12 => 4570, 13 => 4427, 14 => 4303, 15 => 4193, 16 => 4095);

   package Double_Real is new System.Double_Real (Num);
   use type Double_Real.Double_T;

   subtype Double_T is Double_Real.Double_T;
   --  The double floating-point type

   function Integer_to_Real
     (Str   : String;
      Val   : Uns;
      Base  : Unsigned;
      Scale : Integer;
      Extra : Unsigned;
      Minus : Boolean) return Num;
   --  Convert the real value from integer to real representation

   function Large_Powten (Exp : Natural) return Double_T;
   --  Return 10.0**Exp as a double number, where Exp > Maxpow

   ---------------------
   -- Integer_to_Real --
   ---------------------

   function Integer_to_Real
     (Str   : String;
      Val   : Uns;
      Base  : Unsigned;
      Scale : Integer;
      Extra : Unsigned;
      Minus : Boolean) return Num
   is
      pragma Assert (Base in 2 .. 16);

      pragma Assert (Num'Machine_Radix = 2);

      pragma Unsuppress (Range_Check);

      Maxexp : constant Positive :=
                 (if    Num'Size = 32             then Maxexp32 (Base)
                  elsif Num'Size = 64             then Maxexp64 (Base)
                  elsif Num'Machine_Mantissa = 64 then Maxexp80 (Base)
                  else  raise Program_Error);
      --  Maximum exponent of the base that can fit in Num

      R_Val : Num;
      D_Val : Double_T;
      S     : Integer := Scale;

   begin
      --  We call the floating-point processor reset routine so we can be sure
      --  that the x87 FPU is properly set for conversions. This is especially
      --  needed on Windows, where calls to the operating system randomly reset
      --  the processor into 64-bit mode.

      if Num'Machine_Mantissa = 64 then
         System.Float_Control.Reset;
      end if;

      --  Take into account the extra digit, i.e. do the two computations

      --    (1)  R_Val := R_Val * Num (B) + Num (Extra)
      --    (2)  S := S - 1

      --  In the first, the three operands are exact, so using an FMA would
      --  be ideal, but we are most likely running on the x87 FPU, hence we
      --  may not have one. That is why we turn the multiplication into an
      --  iterated addition with exact error handling, so that we can do a
      --  single rounding at the end.

      if Need_Extra and then Extra > 0 then
         declare
            B   : Unsigned := Base;
            Acc : Num      := 0.0;
            Err : Num      := 0.0;
            Fac : Num      := Num (Val);
            DS  : Double_T;

         begin
            loop
               --  If B is odd, add one factor. Note that the accumulator is
               --  never larger than the factor at this point (it is in fact
               --  never larger than the factor minus the initial value).

               if B rem 2 /= 0 then
                  if Acc = 0.0 then
                     Acc := Fac;
                  else
                     DS  := Double_Real.Quick_Two_Sum (Fac, Acc);
                     Acc := DS.Hi;
                     Err := Err + DS.Lo;
                  end if;
                  exit when B = 1;
               end if;

               --  Now B is (morally) even, halve it and double the factor,
               --  which is always an exact operation.

               B := B / 2;
               Fac := Fac * 2.0;
            end loop;

            --  Add Extra to the error, which are both small integers

            D_Val := Double_Real.Quick_Two_Sum (Acc, Err + Num (Extra));

            S := S - 1;
         end;

      --  Or else, if the Extra digit is zero, do the exact conversion

      elsif Need_Extra then
         D_Val := Double_Real.To_Double (Num (Val));

      --  Otherwise, the value contains more bits than the mantissa so do the
      --  conversion in two steps.

      else
         declare
            Mask : constant Uns := 2**(Uns'Size - Num'Machine_Mantissa) - 1;
            Hi   : constant Uns := Val and not Mask;
            Lo   : constant Uns := Val and Mask;

         begin
            if Hi = 0 then
               D_Val := Double_Real.To_Double (Num (Lo));
            else
               D_Val := Double_Real.Quick_Two_Sum (Num (Hi), Num (Lo));
            end if;
         end;
      end if;

      --  Compute the final value by applying the scaling, if any

      if Val = 0 or else S = 0 then
         R_Val := Double_Real.To_Single (D_Val);

      else
         case Base is
            --  If the base is a power of two, we use the efficient Scaling
            --  attribute with an overflow check, if it is not 2, to catch
            --  ludicrous exponents that would result in an infinity or zero.

            when 2 =>
               R_Val := Num'Scaling (Double_Real.To_Single (D_Val), S);

            when 4 =>
               if Integer'First / 2 <= S and then S <= Integer'Last / 2 then
                  S := S * 2;
               end if;

               R_Val := Num'Scaling (Double_Real.To_Single (D_Val), S);

            when 8 =>
               if Integer'First / 3 <= S and then S <= Integer'Last / 3 then
                  S := S * 3;
               end if;

               R_Val := Num'Scaling (Double_Real.To_Single (D_Val), S);

            when 16 =>
               if Integer'First / 4 <= S and then S <= Integer'Last / 4 then
                  S := S * 4;
               end if;

               R_Val := Num'Scaling (Double_Real.To_Single (D_Val), S);

            --  If the base is 10, use a double implementation for the sake
            --  of accuracy, to be removed when exponentiation is improved.

            --  When the exponent is positive, we can do the computation
            --  directly because, if the exponentiation overflows, then
            --  the final value overflows as well. But when the exponent
            --  is negative, we may need to do it in two steps to avoid
            --  an artificial underflow.

            when 10 =>
               declare
                  Powten : constant array (0 .. Maxpow) of Double_T;
                  pragma Import (Ada, Powten);
                  for Powten'Address use Powten_Address;

               begin
                  if S > 0 then
                     if S <= Maxpow then
                        D_Val := D_Val * Powten (S);
                     else
                        D_Val := D_Val * Large_Powten (S);
                     end if;

                  else
                     if S < -Maxexp then
                        D_Val := D_Val / Large_Powten (Maxexp);
                        S := S + Maxexp;
                     end if;

                     if S >= -Maxpow then
                        D_Val := D_Val / Powten (-S);
                     else
                        D_Val := D_Val / Large_Powten (-S);
                     end if;
                  end if;

                  R_Val := Double_Real.To_Single (D_Val);
               end;

            --  Implementation for other bases with exponentiation

            --  When the exponent is positive, we can do the computation
            --  directly because, if the exponentiation overflows, then
            --  the final value overflows as well. But when the exponent
            --  is negative, we may need to do it in two steps to avoid
            --  an artificial underflow.

            when others =>
               declare
                  B : constant Num := Num (Base);

               begin
                  R_Val := Double_Real.To_Single (D_Val);

                  if S > 0 then
                     R_Val := R_Val * B ** S;

                  else
                     if S < -Maxexp then
                        R_Val := R_Val / B ** Maxexp;
                        S := S + Maxexp;
                     end if;

                     R_Val := R_Val / B ** (-S);
                  end if;
               end;
         end case;
      end if;

      --  Finally deal with initial minus sign, note that this processing is
      --  done even if Uval is zero, so that -0.0 is correctly interpreted.

      return (if Minus then -R_Val else R_Val);

   exception
      when Constraint_Error => Bad_Value (Str);
   end Integer_to_Real;

   ------------------
   -- Large_Powten --
   ------------------

   function Large_Powten (Exp : Natural) return Double_T is
      Powten : constant array (0 .. Maxpow) of Double_T;
      pragma Import (Ada, Powten);
      for Powten'Address use Powten_Address;

      R : Double_T;
      E : Natural;

   begin
      pragma Assert (Exp > Maxpow);

      R := Powten (Maxpow);
      E := Exp - Maxpow;

      while E > Maxpow loop
         R := R * Powten (Maxpow);
         E := E - Maxpow;
      end loop;

      R := R * Powten (E);

      return R;
   end Large_Powten;

   ---------------
   -- Scan_Real --
   ---------------

   function Scan_Real
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Num
   is
      Base  : Unsigned;
      Scale : Integer;
      Extra : Unsigned;
      Minus : Boolean;
      Val   : Uns;

   begin
      Val := Impl.Scan_Raw_Real (Str, Ptr, Max, Base, Scale, Extra, Minus);

      return Integer_to_Real (Str, Val, Base, Scale, Extra, Minus);
   end Scan_Real;

   ----------------
   -- Value_Real --
   ----------------

   function Value_Real (Str : String) return Num is
      Base  : Unsigned;
      Scale : Integer;
      Extra : Unsigned;
      Minus : Boolean;
      Val   : Uns;

   begin
      Val := Impl.Value_Raw_Real (Str, Base, Scale, Extra, Minus);

      return Integer_to_Real (Str, Val, Base, Scale, Extra, Minus);
   end Value_Real;

end System.Val_Real;
