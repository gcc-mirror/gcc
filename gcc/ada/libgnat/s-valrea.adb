------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ R E A L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
   --  a potential roundoff error.

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

   function Integer_to_Real
     (Str   : String;
      Val   : Uns;
      Base  : Unsigned;
      Scale : Integer;
      Extra : Unsigned;
      Minus : Boolean) return Num;
   --  Convert the real value from integer to real representation

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

      B : constant Num := Num (Base);

      R_Val : Num;
      S     : Integer := Scale;

   begin
      --  We call the floating-point processor reset routine so we can be sure
      --  that the x87 FPU is properly set for conversions. This is especially
      --  needed on Windows, where calls to the operating system randomly reset
      --  the processor into 64-bit mode.

      if Num'Machine_Mantissa = 64 then
         System.Float_Control.Reset;
      end if;

      --  Do the conversion

      R_Val := Num (Val);

      --  Take into account the extra digit, if need be. In this case, the
      --  three operands are exact, so using an FMA would be ideal.

      if Need_Extra and then Extra > 0 then
         R_Val := R_Val * B + Num (Extra);
         S := S - 1;
      end if;

      --  Compute the final value

      if R_Val /= 0.0 and then S /= 0 then
         case Base is
            --  If the base is a power of two, we use the efficient Scaling
            --  attribute with an overflow check, if it is not 2, to catch
            --  ludicrous exponents that would result in an infinity or zero.

            when 2 =>
               R_Val := Num'Scaling (R_Val, S);

            when 4 =>
               if Integer'First / 2 <= S and then S <= Integer'Last / 2 then
                  S := S * 2;
               end if;

               R_Val := Num'Scaling (R_Val, S);

            when 8 =>
               if Integer'First / 3 <= S and then S <= Integer'Last / 3 then
                  S := S * 3;
               end if;

               R_Val := Num'Scaling (R_Val, S);

            when 16 =>
               if Integer'First / 4 <= S and then S <= Integer'Last / 4 then
                  S := S * 4;
               end if;

               R_Val := Num'Scaling (R_Val, S);

            --  If the base is 10, we use a table of powers for accuracy's sake

            when 10 =>
               declare
                  Powten : constant array (0 .. Maxpow) of Num;
                  pragma Import (Ada, Powten);
                  for Powten'Address use Powten_Address;

               begin
                  if S > 0 then
                     while S > Maxpow loop
                        R_Val := R_Val * Powten (Maxpow);
                        S := S - Maxpow;
                     end loop;

                     R_Val := R_Val * Powten (S);

                  else
                     while S < -Maxpow loop
                        R_Val := R_Val / Powten (Maxpow);
                        S := S + Maxpow;
                     end loop;

                     R_Val := R_Val / Powten (-S);
                  end if;
               end;

            --  Implementation for other bases with exponentiation

            --  When the exponent is positive, we can do the computation
            --  directly because, if the exponentiation overflows, then
            --  the final value overflows as well. But when the exponent
            --  is negative, we may need to do it in two steps to avoid
            --  an artificial underflow.

            when others =>
               if S > 0 then
                  R_Val := R_Val * B ** S;

               else
                  if S < -Maxexp then
                     R_Val := R_Val / B ** Maxexp;
                     S := S + Maxexp;
                  end if;

                  R_Val := R_Val / B ** (-S);
               end if;
         end case;
      end if;

      --  Finally deal with initial minus sign, note that this processing is
      --  done even if Uval is zero, so that -0.0 is correctly interpreted.

      return (if Minus then -R_Val else R_Val);

   exception
      when Constraint_Error => Bad_Value (Str);
   end Integer_to_Real;

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
