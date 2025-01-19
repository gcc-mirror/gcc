------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ R E A L                       --
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

   Is_Large_Type : constant Boolean := Num'Machine_Mantissa >= 53;
   --  True if the floating-point type is at least IEEE Double

   Precision_Limit : constant Uns := 2**Num'Machine_Mantissa - 1;
   --  See below for the rationale

   package Impl is new Value_R (Uns, 2, Precision_Limit, Round => False);

   subtype Base_T is Unsigned range 2 .. 16;

   --  The following tables compute the maximum exponent of the base that can
   --  fit in the given floating-point format, that is to say the element at
   --  index N is the largest K such that N**K <= Num'Last.

   Maxexp32 : constant array (Base_T) of Positive :=
     [2  => 127, 3 => 80,  4 => 63,  5 => 55,  6 => 49,
      7  => 45,  8 => 42,  9 => 40, 10 => 55, 11 => 37,
      12 => 35, 13 => 34, 14 => 33, 15 => 32, 16 => 31];
   --  The actual value for 10 is 38 but we also use scaling for 10

   Maxexp64 : constant array (Base_T) of Positive :=
     [2  => 1023, 3 => 646,  4 => 511,  5 => 441,  6 => 396,
      7  => 364,  8 => 341,  9 => 323, 10 => 441, 11 => 296,
      12 => 285, 13 => 276, 14 => 268, 15 => 262, 16 => 255];
   --  The actual value for 10 is 308 but we also use scaling for 10

   Maxexp80 : constant array (Base_T) of Positive :=
     [2  => 16383, 3 => 10337, 4 => 8191,  5 => 7056,  6 => 6338,
      7  => 5836,  8 => 5461,  9 => 5168, 10 => 7056, 11 => 4736,
      12 => 4570, 13 => 4427, 14 => 4303, 15 => 4193, 16 => 4095];
   --  The actual value for 10 is 4932 but we also use scaling for 10

   package Double_Real is new System.Double_Real (Num);
   use type Double_Real.Double_T;

   subtype Double_T is Double_Real.Double_T;
   --  The double floating-point type

   function Exact_Log2 (N : Unsigned) return Positive is
     (case N is
        when  2     => 1,
        when  4     => 2,
        when  8     => 3,
        when 16     => 4,
        when others => raise Program_Error);
   --  Return the exponent of a power of 2

   function Integer_to_Real
     (Str   : String;
      Val   : Impl.Value_Array;
      Base  : Unsigned;
      Scale : Impl.Scale_Array;
      Minus : Boolean) return Num;
   --  Convert the real value from integer to real representation

   function Large_Powfive (Exp : Natural) return Double_T;
   --  Return 5.0**Exp as a double number, where Exp > Maxpow

   function Large_Powfive (Exp : Natural; S : out Natural) return Double_T;
   --  Return Num'Scaling (5.0**Exp, -S) as a double number where Exp > Maxexp

   ---------------------
   -- Integer_to_Real --
   ---------------------

   function Integer_to_Real
     (Str   : String;
      Val   : Impl.Value_Array;
      Base  : Unsigned;
      Scale : Impl.Scale_Array;
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

      D_Val : Double_T;
      R_Val : Num;
      S     : Integer;

   begin
      --  We call the floating-point processor reset routine so we can be sure
      --  that the x87 FPU is properly set for conversions. This is especially
      --  needed on Windows, where calls to the operating system randomly reset
      --  the processor into 64-bit mode.

      if Num'Machine_Mantissa = 64 then
         System.Float_Control.Reset;
      end if;

      --  First convert the integer mantissa into a double real. The conversion
      --  of each part is exact, given the precision limit we used above. Then,
      --  if the contribution of the low part might be nonnull, scale the high
      --  part appropriately and add the low part to the result.

      if Val (2) = 0 then
         D_Val := Double_Real.To_Double (Num (Val (1)));
         S := Scale (1);

      else
         declare
            V1 : constant Num := Num (Val (1));
            V2 : constant Num := Num (Val (2));

            DS : Positive;

         begin
            DS := Scale (1) - Scale (2);

            case Base is
               --  If the base is a power of two, we use the efficient Scaling
               --  attribute up to an amount worth a double mantissa.

               when 2 | 4 | 8 | 16 =>
                  declare
                     L : constant Positive := Exact_Log2 (Base);

                  begin
                     if DS <= 2 * Num'Machine_Mantissa / L then
                        DS := DS * L;
                        D_Val :=
                          Double_Real.Quick_Two_Sum (Num'Scaling (V1, DS), V2);
                        S := Scale (2);

                     else
                        D_Val := Double_Real.To_Double (V1);
                        S := Scale (1);
                     end if;
                  end;

               --  If the base is 10, we also scale up to an amount worth a
               --  double mantissa.

               when 10 =>
                  declare
                     Powfive : constant array (0 .. Maxpow) of Double_T;
                     pragma Import (Ada, Powfive);
                     for Powfive'Address use Powfive_Address;

                  begin
                     if DS <= Maxpow then
                        D_Val := Powfive (DS) * Num'Scaling (V1, DS) + V2;
                        S := Scale (2);

                     else
                        D_Val := Double_Real.To_Double (V1);
                        S := Scale (1);
                     end if;
                  end;

               --  Inaccurate implementation for other bases

               when others =>
                  D_Val := Double_Real.To_Double (V1);
                  S := Scale (1);
            end case;
         end;
      end if;

      --  Compute the final value by applying the scaling, if any

      if (Val (1) = 0 and then Val (2) = 0) or else S = 0 then
         R_Val := Double_Real.To_Single (D_Val);

      else
         case Base is
            --  If the base is a power of two, we use the efficient Scaling
            --  attribute with an overflow check, if it is not 2, to catch
            --  ludicrous exponents that would result in an infinity or zero.

            when 2 | 4 | 8 | 16 =>
               declare
                  L : constant Positive := Exact_Log2 (Base);

               begin
                  if Integer'First / L <= S and then S <= Integer'Last / L then
                     S := S * L;
                  end if;

                  R_Val := Num'Scaling (Double_Real.To_Single (D_Val), S);
               end;

            --  If the base is 10, we use a double implementation for the sake
            --  of accuracy combining powers of 5 and scaling attribute. Using
            --  this combination is better than using powers of 10 only because
            --  the Large_Powfive function may overflow only if the final value
            --  will also either overflow or underflow, thus making it possible
            --  to use a single division for the case of negative powers of 10.

            when 10 =>
               declare
                  Powfive : constant array (0 .. Maxpow) of Double_T;
                  pragma Import (Ada, Powfive);
                  for Powfive'Address use Powfive_Address;

                  RS : Natural;

               begin
                  if S > 0 then
                     if S <= Maxpow then
                        D_Val := D_Val * Powfive (S);
                     else
                        D_Val := D_Val * Large_Powfive (S);
                     end if;

                  else
                     if S >= -Maxpow then
                        D_Val := D_Val / Powfive (-S);

                     --  For small types, typically IEEE Single, the trick
                     --  described above does not fully work.

                     elsif not Is_Large_Type and then S < -Maxexp then
                        D_Val := D_Val / Large_Powfive (-S, RS);
                        S := S - RS;

                     else
                        D_Val := D_Val / Large_Powfive (-S);
                     end if;
                  end if;

                  R_Val := Num'Scaling (Double_Real.To_Single (D_Val), S);
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

   -------------------
   -- Large_Powfive --
   -------------------

   function Large_Powfive (Exp : Natural) return Double_T is
      Powfive : constant array (0 .. Maxpow) of Double_T;
      pragma Import (Ada, Powfive);
      for Powfive'Address use Powfive_Address;

      Powfive_100 : constant Double_T;
      pragma Import (Ada, Powfive_100);
      for Powfive_100'Address use Powfive_100_Address;

      Powfive_200 : constant Double_T;
      pragma Import (Ada, Powfive_200);
      for Powfive_200'Address use Powfive_200_Address;

      Powfive_300 : constant Double_T;
      pragma Import (Ada, Powfive_300);
      for Powfive_300'Address use Powfive_300_Address;

      H : Double_T;
      R : Double_T;
      E : Natural;

   begin
      pragma Assert (Exp > Maxpow);

      if Is_Large_Type and then Exp >= 300 then
         R := Powfive_300;
         E := Exp - 300;

      elsif Is_Large_Type and then Exp >= 200 then
         R := Powfive_200;
         E := Exp - 200;

      elsif Is_Large_Type and then Exp >= 100 then
         R := Powfive_100;
         E := Exp - 100;

      else
         R := Powfive (Maxpow);
         E := Exp - Maxpow;
      end if;

      --  Accumulate 5**Maxpow into R until E <= Maxpow or R saturates to +Inf

      while E > Maxpow loop
         H := R;
         R := R * Powfive (Maxpow);
         if R = H then
            E := Maxpow;
            exit;
         end if;
         E := E - Maxpow;
      end loop;

      R := R * Powfive (E);

      return R;
   end Large_Powfive;

   function Large_Powfive (Exp : Natural; S : out Natural) return Double_T is
      Maxexp : constant Positive :=
        (if    Num'Size = 32             then Maxexp32 (5)
         elsif Num'Size = 64             then Maxexp64 (5)
         elsif Num'Machine_Mantissa = 64 then Maxexp80 (5)
         else  raise Program_Error);
      --  Maximum exponent of 5 that can fit in Num

      Powfive : constant array (0 .. Maxpow) of Double_T;
      pragma Import (Ada, Powfive);
      for Powfive'Address use Powfive_Address;

      H : Double_T;
      R : Double_T;
      E : Natural;

   begin
      pragma Assert (Exp > Maxexp);

      --  This routine supports any type but it is not necessary to invoke it
      --  for large types because the above one is sufficient for them.

      pragma Warnings (Off, "-gnatw.a");
      pragma Assert (not Is_Large_Type);
      pragma Warnings (On, "-gnatw.a");

      R := Powfive (Maxpow);
      E := Exp - Maxpow;

      --  If the exponent is not too large, then scale down the result so that
      --  its final value does not overflow but, if it's too large, then do not
      --  bother doing it since overflow is just fine. The scaling factor is -3
      --  for every power of 5 above the maximum, in other words division by 8.
      --  Note that Maxpow is an upper bound of the span of exponents for which
      --  scaling is needed, but it's OK to apply it even if it is not needed.

      if Exp - Maxexp <= Maxpow then
         S := 3 * (Exp - Maxexp);
         R.Hi := Num'Scaling (R.Hi, -S);
         R.Lo := Num'Scaling (R.Lo, -S);
      else
         S := 0;
      end if;

      --  Accumulate 5**Maxpow into R until E <= Maxpow or R saturates to +Inf

      while E > Maxpow loop
         H := R;
         R := R * Powfive (Maxpow);
         if R = H then
            E := Maxpow;
            exit;
         end if;
         E := E - Maxpow;
      end loop;

      R := R * Powfive (E);

      return R;
   end Large_Powfive;

   ---------------
   -- Scan_Real --
   ---------------

   function Scan_Real
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Num
   is
      Base  : Unsigned;
      Scale : Impl.Scale_Array;
      Extra : Unsigned;
      Minus : Boolean;
      Val   : Impl.Value_Array;

   begin
      Val := Impl.Scan_Raw_Real (Str, Ptr, Max, Base, Scale, Extra, Minus);

      return Integer_to_Real (Str, Val, Base, Scale, Minus);
   end Scan_Real;

   ----------------
   -- Value_Real --
   ----------------

   function Value_Real (Str : String) return Num is
      Base  : Unsigned;
      Scale : Impl.Scale_Array;
      Extra : Unsigned;
      Minus : Boolean;
      Val   : Impl.Value_Array;

   begin
      Val := Impl.Value_Raw_Real (Str, Base, Scale, Extra, Minus);

      return Integer_to_Real (Str, Val, Base, Scale, Minus);
   end Value_Real;

end System.Val_Real;
