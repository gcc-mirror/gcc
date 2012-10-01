------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                ADA.NUMERICS.GENERIC_ELEMENTARY_FUNCTIONS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

--  This body is specifically for using an Ada interface to C math.h to get
--  the computation engine. Many special cases are handled locally to avoid
--  unnecessary calls or to meet Annex G strict mode requirements.

--  Uses functions sqrt, exp, log, pow, sin, asin, cos, acos, tan, atan, sinh,
--  cosh, tanh from C library via math.h

with Ada.Numerics.Aux;

package body Ada.Numerics.Generic_Elementary_Functions is

   use type Ada.Numerics.Aux.Double;

   Sqrt_Two : constant := 1.41421_35623_73095_04880_16887_24209_69807_85696;
   Log_Two  : constant := 0.69314_71805_59945_30941_72321_21458_17656_80755;

   Half_Log_Two : constant := Log_Two / 2;

   subtype T is Float_Type'Base;
   subtype Double is Aux.Double;

   Two_Pi  : constant T := 2.0 * Pi;
   Half_Pi : constant T := Pi / 2.0;

   Half_Log_Epsilon    : constant T := T (1 - T'Model_Mantissa) * Half_Log_Two;
   Log_Inverse_Epsilon : constant T := T (T'Model_Mantissa - 1) * Log_Two;
   Sqrt_Epsilon        : constant T := Sqrt_Two ** (1 - T'Model_Mantissa);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Exp_Strict (X : Float_Type'Base) return Float_Type'Base;
   --  Cody/Waite routine, supposedly more precise than the library version.
   --  Currently only needed for Sinh/Cosh on X86 with the largest FP type.

   function Local_Atan
     (Y : Float_Type'Base;
      X : Float_Type'Base := 1.0) return Float_Type'Base;
   --  Common code for arc tangent after cycle reduction

   ----------
   -- "**" --
   ----------

   function "**" (Left, Right : Float_Type'Base) return Float_Type'Base is
      A_Right  : Float_Type'Base;
      Int_Part : Integer;
      Result   : Float_Type'Base;
      R1       : Float_Type'Base;
      Rest     : Float_Type'Base;

   begin
      if Left = 0.0
        and then Right = 0.0
      then
         raise Argument_Error;

      elsif Left < 0.0 then
         raise Argument_Error;

      elsif Right = 0.0 then
         return 1.0;

      elsif Left = 0.0 then
         if Right < 0.0 then
            raise Constraint_Error;
         else
            return 0.0;
         end if;

      elsif Left = 1.0 then
         return 1.0;

      elsif Right = 1.0 then
         return Left;

      else
         begin
            if Right = 2.0 then
               return Left * Left;

            elsif Right = 0.5 then
               return Sqrt (Left);

            else
               A_Right := abs (Right);

               --  If exponent is larger than one, compute integer exponen-
               --  tiation if possible, and evaluate fractional part with more
               --  precision. The relative error is now proportional to the
               --  fractional part of the exponent only.

               if A_Right > 1.0
                 and then A_Right < Float_Type'Base (Integer'Last)
               then
                  Int_Part := Integer (Float_Type'Base'Truncation (A_Right));
                  Result := Left ** Int_Part;
                  Rest :=  A_Right - Float_Type'Base (Int_Part);

                  --  Compute with two leading bits of the mantissa using
                  --  square roots. Bound  to be better than logarithms, and
                  --  easily extended to greater precision.

                  if Rest >= 0.5 then
                     R1 := Sqrt (Left);
                     Result := Result * R1;
                     Rest := Rest - 0.5;

                     if Rest >= 0.25 then
                        Result := Result * Sqrt (R1);
                        Rest := Rest - 0.25;
                     end if;

                  elsif Rest >= 0.25 then
                     Result := Result * Sqrt (Sqrt (Left));
                     Rest := Rest - 0.25;
                  end if;

                  Result :=  Result *
                    Float_Type'Base (Aux.Pow (Double (Left), Double (Rest)));

                  if Right >= 0.0 then
                     return Result;
                  else
                     return (1.0 / Result);
                  end if;
               else
                  return
                    Float_Type'Base (Aux.Pow (Double (Left), Double (Right)));
               end if;
            end if;

         exception
            when others =>
               raise Constraint_Error;
         end;
      end if;
   end "**";

   ------------
   -- Arccos --
   ------------

   --  Natural cycle

   function Arccos (X : Float_Type'Base) return Float_Type'Base is
      Temp : Float_Type'Base;

   begin
      if abs X > 1.0 then
         raise Argument_Error;

      elsif abs X < Sqrt_Epsilon then
         return Pi / 2.0 - X;

      elsif X = 1.0 then
         return 0.0;

      elsif X = -1.0 then
         return Pi;
      end if;

      Temp := Float_Type'Base (Aux.Acos (Double (X)));

      if Temp < 0.0 then
         Temp := Pi + Temp;
      end if;

      return Temp;
   end Arccos;

   --  Arbitrary cycle

   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base is
      Temp : Float_Type'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif abs X > 1.0 then
         raise Argument_Error;

      elsif abs X < Sqrt_Epsilon then
         return Cycle / 4.0;

      elsif X = 1.0 then
         return 0.0;

      elsif X = -1.0 then
         return Cycle / 2.0;
      end if;

      Temp := Arctan (Sqrt ((1.0 - X) * (1.0 + X)) / X, 1.0, Cycle);

      if Temp < 0.0 then
         Temp := Cycle / 2.0 + Temp;
      end if;

      return Temp;
   end Arccos;

   -------------
   -- Arccosh --
   -------------

   function Arccosh (X : Float_Type'Base) return Float_Type'Base is
   begin
      --  Return positive branch of Log (X - Sqrt (X * X - 1.0)), or the proper
      --  approximation for X close to 1 or >> 1.

      if X < 1.0 then
         raise Argument_Error;

      elsif X < 1.0 + Sqrt_Epsilon then
         return Sqrt (2.0 * (X - 1.0));

      elsif  X > 1.0 / Sqrt_Epsilon then
         return Log (X) + Log_Two;

      else
         return Log (X + Sqrt ((X - 1.0) * (X + 1.0)));
      end if;
   end Arccosh;

   ------------
   -- Arccot --
   ------------

   --  Natural cycle

   function Arccot
     (X    : Float_Type'Base;
      Y    : Float_Type'Base := 1.0)
      return Float_Type'Base
   is
   begin
      --  Just reverse arguments

      return Arctan (Y, X);
   end Arccot;

   --  Arbitrary cycle

   function Arccot
     (X     : Float_Type'Base;
      Y     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return  Float_Type'Base
   is
   begin
      --  Just reverse arguments

      return Arctan (Y, X, Cycle);
   end Arccot;

   -------------
   -- Arccoth --
   -------------

   function Arccoth (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X > 2.0 then
         return Arctanh (1.0 / X);

      elsif abs X = 1.0 then
         raise Constraint_Error;

      elsif abs X < 1.0 then
         raise Argument_Error;

      else
         --  1.0 < abs X <= 2.0. One of X + 1.0 and X - 1.0 is exact, the other
         --  has error 0 or Epsilon.

         return 0.5 * (Log (abs (X + 1.0)) - Log (abs (X - 1.0)));
      end if;
   end Arccoth;

   ------------
   -- Arcsin --
   ------------

   --  Natural cycle

   function Arcsin (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X > 1.0 then
         raise Argument_Error;

      elsif abs X < Sqrt_Epsilon then
         return X;

      elsif X = 1.0 then
         return Pi / 2.0;

      elsif X = -1.0 then
         return -(Pi / 2.0);
      end if;

      return Float_Type'Base (Aux.Asin (Double (X)));
   end Arcsin;

   --  Arbitrary cycle

   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif abs X > 1.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         return X;

      elsif X = 1.0 then
         return Cycle / 4.0;

      elsif X = -1.0 then
         return -(Cycle / 4.0);
      end if;

      return Arctan (X / Sqrt ((1.0 - X) * (1.0 + X)), 1.0, Cycle);
   end Arcsin;

   -------------
   -- Arcsinh --
   -------------

   function Arcsinh (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X < Sqrt_Epsilon then
         return X;

      elsif X > 1.0 / Sqrt_Epsilon then
         return Log (X) + Log_Two;

      elsif X < -(1.0 / Sqrt_Epsilon) then
         return -(Log (-X) + Log_Two);

      elsif X < 0.0 then
         return -Log (abs X + Sqrt (X * X + 1.0));

      else
         return Log (X + Sqrt (X * X + 1.0));
      end if;
   end Arcsinh;

   ------------
   -- Arctan --
   ------------

   --  Natural cycle

   function Arctan
     (Y    : Float_Type'Base;
      X    : Float_Type'Base := 1.0)
      return Float_Type'Base
   is
   begin
      if X = 0.0 and then Y = 0.0 then
         raise Argument_Error;

      elsif Y = 0.0 then
         if X > 0.0 then
            return 0.0;
         else -- X < 0.0
            return Pi * Float_Type'Copy_Sign (1.0, Y);
         end if;

      elsif X = 0.0 then
         return Float_Type'Copy_Sign (Half_Pi, Y);

      else
         return Local_Atan (Y, X);
      end if;
   end Arctan;

   --  Arbitrary cycle

   function Arctan
     (Y     : Float_Type'Base;
      X     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return  Float_Type'Base
   is
   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif X = 0.0 and then Y = 0.0 then
         raise Argument_Error;

      elsif Y = 0.0 then
         if X > 0.0 then
            return 0.0;
         else -- X < 0.0
            return Cycle / 2.0 * Float_Type'Copy_Sign (1.0, Y);
         end if;

      elsif X = 0.0 then
         return Float_Type'Copy_Sign (Cycle / 4.0, Y);

      else
         return Local_Atan (Y, X) *  Cycle / Two_Pi;
      end if;
   end Arctan;

   -------------
   -- Arctanh --
   -------------

   function Arctanh (X : Float_Type'Base) return Float_Type'Base is
      A, B, D, A_Plus_1, A_From_1 : Float_Type'Base;

      Mantissa : constant Integer := Float_Type'Base'Machine_Mantissa;

   begin
      --  The naive formula:

      --     Arctanh (X) := (1/2) * Log  (1 + X) / (1 - X)

      --   is not well-behaved numerically when X < 0.5 and when X is close
      --   to one. The following is accurate but probably not optimal.

      if abs X = 1.0 then
         raise Constraint_Error;

      elsif abs X >= 1.0 - 2.0 ** (-Mantissa) then

         if abs X >= 1.0 then
            raise Argument_Error;
         else

            --  The one case that overflows if put through the method below:
            --  abs X = 1.0 - Epsilon.  In this case (1/2) log (2/Epsilon) is
            --  accurate. This simplifies to:

            return Float_Type'Copy_Sign (
               Half_Log_Two * Float_Type'Base (Mantissa + 1), X);
         end if;

      --  elsif abs X <= 0.5 then
      --  why is above line commented out ???

      else
         --  Use several piecewise linear approximations. A is close to X,
         --  chosen so 1.0 + A, 1.0 - A, and X - A are exact. The two scalings
         --  remove the low-order bits of X.

         A := Float_Type'Base'Scaling (
             Float_Type'Base (Long_Long_Integer
               (Float_Type'Base'Scaling (X, Mantissa - 1))), 1 - Mantissa);

         B := X - A;                --  This is exact; abs B <= 2**(-Mantissa).
         A_Plus_1 := 1.0 + A;       --  This is exact.
         A_From_1 := 1.0 - A;       --  Ditto.
         D := A_Plus_1 * A_From_1;  --  1 - A*A.

         --  use one term of the series expansion:

         --    f (x + e) = f(x) + e * f'(x) + ..

         --  The derivative of Arctanh at A is 1/(1-A*A). Next term is
         --  A*(B/D)**2 (if a quadratic approximation is ever needed).

         return 0.5 * (Log (A_Plus_1) - Log (A_From_1)) + B / D;
      end if;
   end Arctanh;

   ---------
   -- Cos --
   ---------

   --  Natural cycle

   function Cos (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X = 0.0 then
         return 1.0;

      elsif abs X < Sqrt_Epsilon then
         return 1.0;

      end if;

      return Float_Type'Base (Aux.Cos (Double (X)));
   end Cos;

   --  Arbitrary cycle

   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      --  Just reuse the code for Sin. The potential small loss of speed is
      --  negligible with proper (front-end) inlining.

      return -Sin (abs X - Cycle * 0.25, Cycle);
   end Cos;

   ----------
   -- Cosh --
   ----------

   function Cosh (X : Float_Type'Base) return Float_Type'Base is
      Lnv      : constant Float_Type'Base := 8#0.542714#;
      V2minus1 : constant Float_Type'Base := 0.13830_27787_96019_02638E-4;
      Y        : constant Float_Type'Base := abs X;
      Z        : Float_Type'Base;

   begin
      if Y < Sqrt_Epsilon then
         return 1.0;

      elsif  Y > Log_Inverse_Epsilon then
         Z := Exp_Strict (Y - Lnv);
         return (Z + V2minus1 * Z);

      else
         Z := Exp_Strict (Y);
         return 0.5 * (Z + 1.0 / Z);
      end if;

   end Cosh;

   ---------
   -- Cot --
   ---------

   --  Natural cycle

   function Cot (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X = 0.0 then
         raise Constraint_Error;

      elsif abs X < Sqrt_Epsilon then
         return 1.0 / X;
      end if;

      return 1.0 / Float_Type'Base (Aux.Tan (Double (X)));
   end Cot;

   --  Arbitrary cycle

   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base is
      T : Float_Type'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;
      end if;

      T := Float_Type'Base'Remainder (X, Cycle);

      if T = 0.0 or else abs T = 0.5 * Cycle then
         raise Constraint_Error;

      elsif abs T < Sqrt_Epsilon then
         return 1.0 / T;

      elsif abs T = 0.25 * Cycle then
         return 0.0;

      else
         T := T / Cycle * Two_Pi;
         return Cos (T) / Sin (T);
      end if;
   end Cot;

   ----------
   -- Coth --
   ----------

   function Coth (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X = 0.0 then
         raise Constraint_Error;

      elsif X < Half_Log_Epsilon then
         return -1.0;

      elsif X > -Half_Log_Epsilon then
         return 1.0;

      elsif abs X < Sqrt_Epsilon then
         return 1.0 / X;
      end if;

      return 1.0 / Float_Type'Base (Aux.Tanh (Double (X)));
   end Coth;

   ---------
   -- Exp --
   ---------

   function Exp (X : Float_Type'Base) return Float_Type'Base is
      Result : Float_Type'Base;

   begin
      if X = 0.0 then
         return 1.0;
      end if;

      Result := Float_Type'Base (Aux.Exp (Double (X)));

      --  Deal with case of Exp returning IEEE infinity. If Machine_Overflows
      --  is False, then we can just leave it as an infinity (and indeed we
      --  prefer to do so). But if Machine_Overflows is True, then we have
      --  to raise a Constraint_Error exception as required by the RM.

      if Float_Type'Machine_Overflows and then not Result'Valid then
         raise Constraint_Error;
      end if;

      return Result;
   end Exp;

   ----------------
   -- Exp_Strict --
   ----------------

   function Exp_Strict (X : Float_Type'Base) return Float_Type'Base is
      G : Float_Type'Base;
      Z : Float_Type'Base;

      P0 : constant := 0.25000_00000_00000_00000;
      P1 : constant := 0.75753_18015_94227_76666E-2;
      P2 : constant := 0.31555_19276_56846_46356E-4;

      Q0 : constant := 0.5;
      Q1 : constant := 0.56817_30269_85512_21787E-1;
      Q2 : constant := 0.63121_89437_43985_02557E-3;
      Q3 : constant := 0.75104_02839_98700_46114E-6;

      C1 : constant := 8#0.543#;
      C2 : constant := -2.1219_44400_54690_58277E-4;
      Le : constant := 1.4426_95040_88896_34074;

      XN : Float_Type'Base;
      P, Q, R : Float_Type'Base;

   begin
      if X = 0.0 then
         return 1.0;
      end if;

      XN := Float_Type'Base'Rounding (X * Le);
      G := (X - XN * C1) - XN * C2;
      Z := G * G;
      P := G * ((P2 * Z + P1) * Z + P0);
      Q := ((Q3 * Z + Q2) * Z + Q1) * Z + Q0;
      R := 0.5 + P / (Q - P);

      R := Float_Type'Base'Scaling (R, Integer (XN) + 1);

      --  Deal with case of Exp returning IEEE infinity. If Machine_Overflows
      --  is False, then we can just leave it as an infinity (and indeed we
      --  prefer to do so). But if Machine_Overflows is True, then we have to
      --  raise a Constraint_Error exception as required by the RM.

      if Float_Type'Machine_Overflows and then not R'Valid then
         raise Constraint_Error;
      else
         return R;
      end if;

   end Exp_Strict;

   ----------------
   -- Local_Atan --
   ----------------

   function Local_Atan
     (Y : Float_Type'Base;
      X : Float_Type'Base := 1.0) return Float_Type'Base
   is
      Z        : Float_Type'Base;
      Raw_Atan : Float_Type'Base;

   begin
      Z := (if abs Y > abs X then abs (X / Y) else abs (Y / X));

      Raw_Atan :=
        (if Z < Sqrt_Epsilon then Z
         elsif Z = 1.0 then Pi / 4.0
         else Float_Type'Base (Aux.Atan (Double (Z))));

      if abs Y > abs X then
         Raw_Atan := Half_Pi - Raw_Atan;
      end if;

      if X > 0.0 then
         return Float_Type'Copy_Sign (Raw_Atan, Y);
      else
         return Float_Type'Copy_Sign (Pi - Raw_Atan, Y);
      end if;
   end Local_Atan;

   ---------
   -- Log --
   ---------

   --  Natural base

   function Log (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X < 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         raise Constraint_Error;

      elsif X = 1.0 then
         return 0.0;
      end if;

      return Float_Type'Base (Aux.Log (Double (X)));
   end Log;

   --  Arbitrary base

   function Log (X, Base : Float_Type'Base) return Float_Type'Base is
   begin
      if X < 0.0 then
         raise Argument_Error;

      elsif Base <= 0.0 or else Base = 1.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         raise Constraint_Error;

      elsif X = 1.0 then
         return 0.0;
      end if;

      return Float_Type'Base (Aux.Log (Double (X)) / Aux.Log (Double (Base)));
   end Log;

   ---------
   -- Sin --
   ---------

   --  Natural cycle

   function Sin (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X < Sqrt_Epsilon then
         return X;
      end if;

      return Float_Type'Base (Aux.Sin (Double (X)));
   end Sin;

   --  Arbitrary cycle

   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base is
      T : Float_Type'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      --  If X is zero, return it as the result, preserving the argument sign.
      --  Is this test really needed on any machine ???

      elsif X = 0.0 then
         return X;
      end if;

      T := Float_Type'Base'Remainder (X, Cycle);

      --  The following two reductions reduce the argument to the interval
      --  [-0.25 * Cycle, 0.25 * Cycle]. This reduction is exact and is needed
      --  to prevent inaccuracy that may result if the sine function uses a
      --  different (more accurate) value of Pi in its reduction than is used
      --  in the multiplication with Two_Pi.

      if abs T > 0.25 * Cycle then
         T := 0.5 * Float_Type'Copy_Sign (Cycle, T) - T;
      end if;

      --  Could test for 12.0 * abs T = Cycle, and return an exact value in
      --  those cases. It is not clear this is worth the extra test though.

      return Float_Type'Base (Aux.Sin (Double (T / Cycle * Two_Pi)));
   end Sin;

   ----------
   -- Sinh --
   ----------

   function Sinh (X : Float_Type'Base) return Float_Type'Base is
      Lnv      : constant Float_Type'Base := 8#0.542714#;
      V2minus1 : constant Float_Type'Base := 0.13830_27787_96019_02638E-4;
      Y        : constant Float_Type'Base := abs X;
      F        : constant Float_Type'Base := Y * Y;
      Z        : Float_Type'Base;

      Float_Digits_1_6 : constant Boolean := Float_Type'Digits < 7;

   begin
      if Y < Sqrt_Epsilon then
         return X;

      elsif  Y > Log_Inverse_Epsilon then
         Z := Exp_Strict (Y - Lnv);
         Z := Z + V2minus1 * Z;

      elsif Y < 1.0 then

         if Float_Digits_1_6 then

            --  Use expansion provided by Cody and Waite, p. 226. Note that
            --  leading term of the polynomial in Q is exactly 1.0.

            declare
               P0 : constant := -0.71379_3159E+1;
               P1 : constant := -0.19033_3399E+0;
               Q0 : constant := -0.42827_7109E+2;

            begin
               Z := Y + Y * F * (P1 * F + P0) / (F + Q0);
            end;

         else
            declare
               P0 : constant := -0.35181_28343_01771_17881E+6;
               P1 : constant := -0.11563_52119_68517_68270E+5;
               P2 : constant := -0.16375_79820_26307_51372E+3;
               P3 : constant := -0.78966_12741_73570_99479E+0;
               Q0 : constant := -0.21108_77005_81062_71242E+7;
               Q1 : constant :=  0.36162_72310_94218_36460E+5;
               Q2 : constant := -0.27773_52311_96507_01667E+3;

            begin
               Z := Y + Y * F * (((P3 * F + P2) * F + P1) * F + P0)
                              / (((F + Q2) * F + Q1) * F + Q0);
            end;
         end if;

      else
         Z := Exp_Strict (Y);
         Z := 0.5 * (Z - 1.0 / Z);
      end if;

      if X > 0.0 then
         return Z;
      else
         return -Z;
      end if;
   end Sinh;

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X < 0.0 then
         raise Argument_Error;

      --  Special case Sqrt (0.0) to preserve possible minus sign per IEEE

      elsif X = 0.0 then
         return X;
      end if;

      return Float_Type'Base (Aux.Sqrt (Double (X)));
   end Sqrt;

   ---------
   -- Tan --
   ---------

   --  Natural cycle

   function Tan (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X < Sqrt_Epsilon then
         return X;
      end if;

      --  Note: if X is exactly pi/2, then we should raise an exception, since
      --  the result would overflow. But for all floating-point formats we deal
      --  with, it is impossible for X to be exactly pi/2, and the result is
      --  always in range.

      return Float_Type'Base (Aux.Tan (Double (X)));
   end Tan;

   --  Arbitrary cycle

   function Tan (X, Cycle : Float_Type'Base) return Float_Type'Base is
      T : Float_Type'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         return X;
      end if;

      T := Float_Type'Base'Remainder (X, Cycle);

      if abs T = 0.25 * Cycle then
         raise Constraint_Error;

      elsif abs T = 0.5 * Cycle then
         return 0.0;

      else
         T := T / Cycle * Two_Pi;
         return Sin (T) / Cos (T);
      end if;

   end Tan;

   ----------
   -- Tanh --
   ----------

   function Tanh (X : Float_Type'Base) return Float_Type'Base is
      P0 : constant Float_Type'Base := -0.16134_11902_39962_28053E+4;
      P1 : constant Float_Type'Base := -0.99225_92967_22360_83313E+2;
      P2 : constant Float_Type'Base := -0.96437_49277_72254_69787E+0;

      Q0 : constant Float_Type'Base :=  0.48402_35707_19886_88686E+4;
      Q1 : constant Float_Type'Base :=  0.22337_72071_89623_12926E+4;
      Q2 : constant Float_Type'Base :=  0.11274_47438_05349_49335E+3;
      Q3 : constant Float_Type'Base :=  0.10000_00000_00000_00000E+1;

      Half_Ln3 : constant Float_Type'Base := 0.54930_61443_34054_84570;

      P, Q, R : Float_Type'Base;
      Y : constant Float_Type'Base := abs X;
      G : constant Float_Type'Base := Y * Y;

      Float_Type_Digits_15_Or_More : constant Boolean :=
        Float_Type'Digits > 14;

   begin
      if X < Half_Log_Epsilon then
         return -1.0;

      elsif X > -Half_Log_Epsilon then
         return 1.0;

      elsif Y < Sqrt_Epsilon then
         return X;

      elsif Y < Half_Ln3
        and then Float_Type_Digits_15_Or_More
      then
         P := (P2 * G + P1) * G + P0;
         Q := ((Q3 * G + Q2) * G + Q1) * G + Q0;
         R := G * (P / Q);
         return X + X * R;

      else
         return Float_Type'Base (Aux.Tanh (Double (X)));
      end if;
   end Tanh;

end Ada.Numerics.Generic_Elementary_Functions;
