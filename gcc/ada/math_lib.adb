------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             M A T H _ L I B                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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
--  unnecessary calls. This is not a "strict" implementation, but takes full
--  advantage of the C functions, e.g. in providing interface to hardware
--  provided versions of the elementary functions.

--  A known weakness is that on the x86, all computation is done in Double,
--  which means that a lot of accuracy is lost for the Long_Long_Float case.

--  Uses functions sqrt, exp, log, pow, sin, asin, cos, acos, tan, atan,
--  sinh, cosh, tanh from C library via math.h

--  This is an adaptation of Ada.Numerics.Generic_Elementary_Functions that
--  provides a compatible body for the DEC Math_Lib package.

with Ada.Numerics.Aux;
use type Ada.Numerics.Aux.Double;
with Ada.Numerics; use Ada.Numerics;

package body Math_Lib is

   Log_Two : constant := 0.69314_71805_59945_30941_72321_21458_17656_80755;

   Two_Pi     : constant Real'Base := 2.0 * Pi;
   Half_Pi    : constant Real'Base := Pi / 2.0;
   Fourth_Pi  : constant Real'Base := Pi / 4.0;
   Epsilon    : constant Real'Base := Real'Base'Epsilon;
   IEpsilon   : constant Real'Base := 1.0 / Epsilon;

   subtype Double is Aux.Double;

   DEpsilon    : constant Double := Double (Epsilon);
   DIEpsilon   : constant Double := Double (IEpsilon);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Arctan
     (Y    : Real;
      A    : Real := 1.0)
      return Real;

   function Arctan
     (Y     : Real;
      A     : Real := 1.0;
      Cycle : Real)
      return  Real;

   function Exact_Remainder
     (A    : Real;
      Y    : Real)
      return Real;
   --  Computes exact remainder of A divided by Y

   function Half_Log_Epsilon return Real;
   --  Function to provide constant: 0.5 * Log (Epsilon)

   function Local_Atan
     (Y    : Real;
      A    : Real := 1.0)
      return Real;
   --  Common code for arc tangent after cycle reduction

   function Log_Inverse_Epsilon return Real;
   --  Function to provide constant: Log (1.0 / Epsilon)

   function Square_Root_Epsilon return Real;
   --  Function to provide constant: Sqrt (Epsilon)

   ----------
   -- "**" --
   ----------

   function "**" (A1, A2 : Real) return Real is

   begin
      if A1 = 0.0
        and then A2 = 0.0
      then
         raise Argument_Error;

      elsif A1 < 0.0 then
         raise Argument_Error;

      elsif A2 = 0.0 then
         return 1.0;

      elsif A1 = 0.0 then
         if A2 < 0.0 then
            raise Constraint_Error;
         else
            return 0.0;
         end if;

      elsif A1 = 1.0 then
         return 1.0;

      elsif A2 = 1.0 then
         return A1;

      else
         begin
            if A2 = 2.0 then
               return A1 * A1;
            else
               return
                 Real (Aux.pow (Double (A1), Double (A2)));
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

   function Arccos (A : Real) return Real is
      Temp : Real'Base;

   begin
      if abs A > 1.0 then
         raise Argument_Error;

      elsif abs A < Square_Root_Epsilon then
         return Pi / 2.0 - A;

      elsif A = 1.0 then
         return 0.0;

      elsif A = -1.0 then
         return Pi;
      end if;

      Temp := Real (Aux.acos (Double (A)));

      if Temp < 0.0 then
         Temp := Pi + Temp;
      end if;

      return Temp;
   end Arccos;

   --  Arbitrary cycle

   function Arccos (A, Cycle : Real) return Real is
      Temp : Real'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif abs A > 1.0 then
         raise Argument_Error;

      elsif abs A < Square_Root_Epsilon then
         return Cycle / 4.0;

      elsif A = 1.0 then
         return 0.0;

      elsif A = -1.0 then
         return Cycle / 2.0;
      end if;

      Temp := Arctan (Sqrt (1.0 - A * A) / A, 1.0, Cycle);

      if Temp < 0.0 then
         Temp := Cycle / 2.0 + Temp;
      end if;

      return Temp;
   end Arccos;

   -------------
   -- Arccosh --
   -------------

   function Arccosh (A : Real) return Real is
   begin
      --  Return Log (A - Sqrt (A * A - 1.0));  double valued,
      --    only positive value returned
      --  What is this comment ???

      if A < 1.0 then
         raise Argument_Error;

      elsif A < 1.0 + Square_Root_Epsilon then
         return A - 1.0;

      elsif abs A > 1.0 / Square_Root_Epsilon then
         return Log (A) + Log_Two;

      else
         return Log (A + Sqrt (A * A - 1.0));
      end if;
   end Arccosh;

   ------------
   -- Arccot --
   ------------

   --  Natural cycle

   function Arccot
     (A    : Real;
      Y    : Real := 1.0)
      return Real
   is
   begin
      --  Just reverse arguments

      return Arctan (Y, A);
   end Arccot;

   --  Arbitrary cycle

   function Arccot
     (A     : Real;
      Y     : Real := 1.0;
      Cycle : Real)
      return  Real
   is
   begin
      --  Just reverse arguments

      return Arctan (Y, A, Cycle);
   end Arccot;

   -------------
   -- Arccoth --
   -------------

   function Arccoth (A : Real) return Real is
   begin
      if abs A = 1.0 then
         raise Constraint_Error;

      elsif abs A < 1.0 then
         raise Argument_Error;

      elsif abs A > 1.0 / Epsilon then
         return 0.0;

      else
         return 0.5 * Log ((1.0 + A) / (A - 1.0));
      end if;
   end Arccoth;

   ------------
   -- Arcsin --
   ------------

   --  Natural cycle

   function Arcsin (A : Real) return Real is
   begin
      if abs A > 1.0 then
         raise Argument_Error;

      elsif abs A < Square_Root_Epsilon then
         return A;

      elsif A = 1.0 then
         return Pi / 2.0;

      elsif A = -1.0 then
         return -Pi / 2.0;
      end if;

      return Real (Aux.asin (Double (A)));
   end Arcsin;

   --  Arbitrary cycle

   function Arcsin (A, Cycle : Real) return Real is
   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif abs A > 1.0 then
         raise Argument_Error;

      elsif A = 0.0 then
         return A;

      elsif A = 1.0 then
         return Cycle / 4.0;

      elsif A = -1.0 then
         return -Cycle / 4.0;
      end if;

      return Arctan (A / Sqrt (1.0 - A * A), 1.0, Cycle);
   end Arcsin;

   -------------
   -- Arcsinh --
   -------------

   function Arcsinh (A : Real) return Real is
   begin
      if abs A < Square_Root_Epsilon then
         return A;

      elsif A > 1.0 / Square_Root_Epsilon then
         return Log (A) + Log_Two;

      elsif A < -1.0 / Square_Root_Epsilon then
         return -(Log (-A) + Log_Two);

      elsif A < 0.0 then
         return -Log (abs A + Sqrt (A * A + 1.0));

      else
         return Log (A + Sqrt (A * A + 1.0));
      end if;
   end Arcsinh;

   ------------
   -- Arctan --
   ------------

   --  Natural cycle

   function Arctan
     (Y    : Real;
      A    : Real := 1.0)
      return Real
   is
   begin
      if A = 0.0
        and then Y = 0.0
      then
         raise Argument_Error;

      elsif Y = 0.0 then
         if A > 0.0 then
            return 0.0;
         else -- A < 0.0
            return Pi;
         end if;

      elsif A = 0.0 then
         if Y > 0.0 then
            return Half_Pi;
         else -- Y < 0.0
            return -Half_Pi;
         end if;

      else
         return Local_Atan (Y, A);
      end if;
   end Arctan;

   --  Arbitrary cycle

   function Arctan
     (Y     : Real;
      A     : Real := 1.0;
      Cycle : Real)
      return  Real
   is
   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif A = 0.0
        and then Y = 0.0
      then
         raise Argument_Error;

      elsif Y = 0.0 then
         if A > 0.0 then
            return 0.0;
         else -- A < 0.0
            return Cycle / 2.0;
         end if;

      elsif A = 0.0 then
         if Y > 0.0 then
            return Cycle / 4.0;
         else -- Y < 0.0
            return -Cycle / 4.0;
         end if;

      else
         return Local_Atan (Y, A) *  Cycle / Two_Pi;
      end if;
   end Arctan;

   -------------
   -- Arctanh --
   -------------

   function Arctanh (A : Real) return Real is
   begin
      if abs A = 1.0 then
         raise Constraint_Error;

      elsif abs A > 1.0 then
         raise Argument_Error;

      elsif abs A < Square_Root_Epsilon then
         return A;

      else
         return 0.5 * Log ((1.0 + A) / (1.0 - A));
      end if;
   end Arctanh;

   ---------
   -- Cos --
   ---------

   --  Natural cycle

   function Cos (A : Real) return Real is
   begin
      if A = 0.0 then
         return 1.0;

      elsif abs A < Square_Root_Epsilon then
         return 1.0;

      end if;

      return Real (Aux.Cos (Double (A)));
   end Cos;

   --  Arbitrary cycle

   function Cos (A, Cycle : Real) return Real is
      T : Real'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif A = 0.0 then
         return 1.0;
      end if;

      T := Exact_Remainder (abs (A), Cycle) / Cycle;

      if T = 0.25
        or else T = 0.75
        or else T = -0.25
        or else T = -0.75
      then
         return 0.0;

      elsif T = 0.5 or T = -0.5 then
         return -1.0;
      end if;

      return Real (Aux.Cos (Double (T * Two_Pi)));
   end Cos;

   ----------
   -- Cosh --
   ----------

   function Cosh (A : Real) return Real is
   begin
      if abs A < Square_Root_Epsilon then
         return 1.0;

      elsif abs A > Log_Inverse_Epsilon then
         return Exp ((abs A) - Log_Two);
      end if;

      return Real (Aux.cosh (Double (A)));

   exception
      when others =>
         raise Constraint_Error;
   end Cosh;

   ---------
   -- Cot --
   ---------

   --  Natural cycle

   function Cot (A : Real) return Real is
   begin
      if A = 0.0 then
         raise Constraint_Error;

      elsif abs A < Square_Root_Epsilon then
         return 1.0 / A;
      end if;

      return Real (1.0 / Real'Base (Aux.tan (Double (A))));
   end Cot;

   --  Arbitrary cycle

   function Cot (A, Cycle : Real) return Real is
      T : Real'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif A = 0.0 then
         raise Constraint_Error;

      elsif abs A < Square_Root_Epsilon then
         return 1.0 / A;
      end if;

      T := Exact_Remainder (A, Cycle) / Cycle;

      if T = 0.0 or T = 0.5 or T = -0.5 then
         raise Constraint_Error;
      else
         return  Cos (T * Two_Pi) / Sin (T * Two_Pi);
      end if;
   end Cot;

   ----------
   -- Coth --
   ----------

   function Coth (A : Real) return Real is
   begin
      if A = 0.0 then
         raise Constraint_Error;

      elsif A < Half_Log_Epsilon then
         return -1.0;

      elsif A > -Half_Log_Epsilon then
         return 1.0;

      elsif abs A < Square_Root_Epsilon then
         return 1.0 / A;
      end if;

      return Real (1.0 / Real'Base (Aux.tanh (Double (A))));
   end Coth;

   ---------------------
   -- Exact_Remainder --
   ---------------------

   function Exact_Remainder
     (A    : Real;
      Y    : Real)
      return Real
   is
      Denominator : Real'Base := abs A;
      Divisor     : Real'Base := abs Y;
      Reducer     : Real'Base;
      Sign        : Real'Base := 1.0;

   begin
      if Y = 0.0 then
         raise Constraint_Error;

      elsif A = 0.0 then
         return 0.0;

      elsif A = Y then
         return 0.0;

      elsif Denominator < Divisor then
         return A;
      end if;

      while Denominator >= Divisor loop

         --  Put divisors mantissa with denominators exponent to make reducer

         Reducer := Divisor;

         begin
            while Reducer * 1_048_576.0 < Denominator loop
               Reducer := Reducer * 1_048_576.0;
            end loop;

         exception
            when others => null;
         end;

         begin
            while Reducer * 1_024.0 < Denominator loop
               Reducer := Reducer * 1_024.0;
            end loop;

         exception
            when others => null;
         end;

         begin
            while Reducer * 2.0 < Denominator loop
               Reducer := Reducer * 2.0;
            end loop;

         exception
            when others => null;
         end;

         Denominator := Denominator - Reducer;
      end loop;

      if A < 0.0 then
         return -Denominator;
      else
         return Denominator;
      end if;
   end Exact_Remainder;

   ---------
   -- Exp --
   ---------

   function Exp (A : Real) return Real is
      Result : Real'Base;

   begin
      if A = 0.0 then
         return 1.0;

      else
         Result := Real (Aux.Exp (Double (A)));

         --  The check here catches the case of Exp returning IEEE infinity

         if Result > Real'Last then
            raise Constraint_Error;
         else
            return Result;
         end if;
      end if;
   end Exp;

   ----------------------
   -- Half_Log_Epsilon --
   ----------------------

   --  Cannot precompute this constant, because this is required to be a
   --  pure package, which allows no state. A pity, but no way around it!

   function Half_Log_Epsilon return Real is
   begin
      return Real (0.5 * Real'Base (Aux.Log (DEpsilon)));
   end Half_Log_Epsilon;

   ----------------
   -- Local_Atan --
   ----------------

   function Local_Atan
     (Y    : Real;
      A    : Real := 1.0)
      return Real
   is
      Z        : Real'Base;
      Raw_Atan : Real'Base;

   begin
      if abs Y > abs A then
         Z := abs (A / Y);
      else
         Z := abs (Y / A);
      end if;

      if Z < Square_Root_Epsilon then
         Raw_Atan := Z;

      elsif Z = 1.0 then
         Raw_Atan := Pi / 4.0;

      elsif Z < Square_Root_Epsilon then
         Raw_Atan := Z;

      else
         Raw_Atan := Real'Base (Aux.Atan (Double (Z)));
      end if;

      if abs Y > abs A then
         Raw_Atan := Half_Pi - Raw_Atan;
      end if;

      if A > 0.0 then
         if Y > 0.0 then
            return Raw_Atan;
         else                 --  Y < 0.0
            return -Raw_Atan;
         end if;

      else                    --  A < 0.0
         if Y > 0.0 then
            return Pi - Raw_Atan;
         else                  --  Y < 0.0
            return -(Pi - Raw_Atan);
         end if;
      end if;
   end Local_Atan;

   ---------
   -- Log --
   ---------

   --  Natural base

   function Log (A : Real) return Real is
   begin
      if A < 0.0 then
         raise Argument_Error;

      elsif A = 0.0 then
         raise Constraint_Error;

      elsif A = 1.0 then
         return 0.0;
      end if;

      return Real (Aux.Log (Double (A)));
   end Log;

   --  Arbitrary base

   function Log (A, Base : Real) return Real is
   begin
      if A < 0.0 then
         raise Argument_Error;

      elsif Base <= 0.0 or else Base = 1.0 then
         raise Argument_Error;

      elsif A = 0.0 then
         raise Constraint_Error;

      elsif A = 1.0 then
         return 0.0;
      end if;

      return Real (Aux.Log (Double (A)) / Aux.Log (Double (Base)));
   end Log;

   -------------------------
   -- Log_Inverse_Epsilon --
   -------------------------

   --  Cannot precompute this constant, because this is required to be a
   --  pure package, which allows no state. A pity, but no way around it!

   function Log_Inverse_Epsilon return Real is
   begin
      return Real (Aux.Log (DIEpsilon));
   end Log_Inverse_Epsilon;

   ---------
   -- Sin --
   ---------

   --  Natural cycle

   function Sin (A : Real) return Real is
   begin
      if abs A < Square_Root_Epsilon then
         return A;
      end if;

      return Real (Aux.Sin (Double (A)));
   end Sin;

   --  Arbitrary cycle

   function Sin (A, Cycle : Real) return Real is
      T : Real'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif A = 0.0 then
         return A;
      end if;

      T := Exact_Remainder (A, Cycle) / Cycle;

      if T = 0.0 or T = 0.5 or T = -0.5 then
         return 0.0;

      elsif T = 0.25 or T = -0.75 then
         return 1.0;

      elsif T = -0.25 or T = 0.75 then
         return -1.0;

      end if;

      return Real (Aux.Sin (Double (T * Two_Pi)));
   end Sin;

   ----------
   -- Sinh --
   ----------

   function Sinh (A : Real) return Real is
   begin
      if abs A < Square_Root_Epsilon then
         return A;

      elsif  A > Log_Inverse_Epsilon then
         return Exp (A - Log_Two);

      elsif A < -Log_Inverse_Epsilon then
         return -Exp ((-A) - Log_Two);
      end if;

      return Real (Aux.Sinh (Double (A)));

   exception
      when others =>
         raise Constraint_Error;
   end Sinh;

   -------------------------
   -- Square_Root_Epsilon --
   -------------------------

   --  Cannot precompute this constant, because this is required to be a
   --  pure package, which allows no state. A pity, but no way around it!

   function Square_Root_Epsilon return Real is
   begin
      return Real (Aux.Sqrt (DEpsilon));
   end Square_Root_Epsilon;

   ----------
   -- Sqrt --
   ----------

   function Sqrt (A : Real) return Real is
   begin
      if A < 0.0 then
         raise Argument_Error;

      --  Special case Sqrt (0.0) to preserve possible minus sign per IEEE

      elsif A = 0.0 then
         return A;

      --  Sqrt (1.0) must be exact for good complex accuracy

      elsif A = 1.0 then
         return 1.0;

      end if;

      return Real (Aux.Sqrt (Double (A)));
   end Sqrt;

   ---------
   -- Tan --
   ---------

   --  Natural cycle

   function Tan (A : Real) return Real is
   begin
      if abs A < Square_Root_Epsilon then
         return A;

      elsif abs A = Pi / 2.0 then
         raise Constraint_Error;
      end if;

      return Real (Aux.tan (Double (A)));
   end Tan;

   --  Arbitrary cycle

   function Tan (A, Cycle : Real) return Real is
      T : Real'Base;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif A = 0.0 then
         return A;
      end if;

      T := Exact_Remainder (A, Cycle) / Cycle;

      if T = 0.25
        or else T = 0.75
        or else T = -0.25
        or else T = -0.75
      then
         raise Constraint_Error;

      else
         return  Sin (T * Two_Pi) / Cos (T * Two_Pi);
      end if;
   end Tan;

   ----------
   -- Tanh --
   ----------

   function Tanh (A : Real) return Real is
   begin
      if A < Half_Log_Epsilon then
         return -1.0;

      elsif A > -Half_Log_Epsilon then
         return 1.0;

      elsif abs A < Square_Root_Epsilon then
         return A;
      end if;

      return Real (Aux.tanh (Double (A)));
   end Tanh;

   ----------------------------
   -- DEC-Specific functions --
   ----------------------------

   function LOG10  (A : REAL) return REAL is
   begin
      return Log (A, 10.0);
   end LOG10;

   function LOG2   (A : REAL) return REAL is
   begin
      return Log (A, 2.0);
   end LOG2;

   function ASIN (A : REAL) return REAL renames Arcsin;
   function ACOS (A : REAL) return REAL renames Arccos;

   function ATAN (A : REAL) return REAL is
   begin
      return Arctan (A, 1.0);
   end ATAN;

   function ATAN2 (A1, A2 : REAL) return REAL renames Arctan;

   function SIND   (A : REAL) return REAL is
   begin
      return Sin (A, 360.0);
   end SIND;

   function COSD   (A : REAL) return REAL is
   begin
      return  Cos (A, 360.0);
   end COSD;

   function TAND   (A : REAL) return REAL is
   begin
      return  Tan (A, 360.0);
   end TAND;

   function ASIND  (A : REAL) return REAL is
   begin
      return  Arcsin (A, 360.0);
   end ASIND;

   function ACOSD  (A : REAL) return REAL is
   begin
      return  Arccos (A, 360.0);
   end ACOSD;

   function Arctan  (A : REAL) return REAL is
   begin
      return  Arctan (A, 1.0, 360.0);
   end Arctan;

   function ATAND (A : REAL) return REAL is
   begin
      return Arctan (A, 1.0, 360.0);
   end ATAND;

   function ATAN2D (A1, A2 : REAL) return REAL is
   begin
      return Arctan (A1, A2, 360.0);
   end ATAN2D;

end Math_Lib;
