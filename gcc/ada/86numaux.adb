------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     A D A . N U M E R I C S . A U X                      --
--                                                                          --
--                                 B o d y                                  --
--                        (Machine Version for x86)                         --
--                                                                          --
--                                                                          --
--          Copyright (C) 1998-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

--  File a-numaux.adb <- 86numaux.adb

--  This version of Numerics.Aux is for the IEEE Double Extended floating
--  point format on x86.

with System.Machine_Code; use System.Machine_Code;

package body Ada.Numerics.Aux is

   NL           : constant String := ASCII.LF & ASCII.HT;

   type FPU_Stack_Pointer is range 0 .. 7;
   for FPU_Stack_Pointer'Size use 3;

   type FPU_Status_Word is record
      B   : Boolean; -- FPU Busy (for 8087 compatibility only)
      ES  : Boolean; -- Error Summary Status
      SF  : Boolean; -- Stack Fault

      Top : FPU_Stack_Pointer;

      --  Condition Code Flags

      --  C2 is set by FPREM and FPREM1 to indicate incomplete reduction.
      --  In case of successfull recorction, C0, C3 and C1 are set to the
      --  three least significant bits of the result (resp. Q2, Q1 and Q0).

      --  C2 is used by FPTAN, FSIN, FCOS, and FSINCOS to indicate that
      --  that source operand is beyond the allowable range of
      --  -2.0**63 .. 2.0**63.

      C3  : Boolean;
      C2  : Boolean;
      C1  : Boolean;
      C0  : Boolean;

      --  Exception Flags

      PE  : Boolean; -- Precision
      UE  : Boolean; -- Underflow
      OE  : Boolean; -- Overflow
      ZE  : Boolean; -- Zero Divide
      DE  : Boolean; -- Denormalized Operand
      IE  : Boolean; -- Invalid Operation
   end record;

   for FPU_Status_Word use record
      B   at 0 range 15 .. 15;
      C3  at 0 range 14 .. 14;
      Top at 0 range 11 .. 13;
      C2  at 0 range 10 .. 10;
      C1  at 0 range  9 ..  9;
      C0  at 0 range  8 ..  8;
      ES  at 0 range  7 ..  7;
      SF  at 0 range  6 ..  6;
      PE  at 0 range  5 ..  5;
      UE  at 0 range  4 ..  4;
      OE  at 0 range  3 ..  3;
      ZE  at 0 range  2 ..  2;
      DE  at 0 range  1 ..  1;
      IE  at 0 range  0 ..  0;
   end record;

   for FPU_Status_Word'Size use 16;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Is_Nan (X : Double) return Boolean;
   --  Return True iff X is a IEEE NaN value

   function Logarithmic_Pow (X, Y : Double) return Double;
   --  Implementation of X**Y using Exp and Log functions (binary base)
   --  to calculate the exponentiation. This is used by Pow for values
   --  for values of Y in the open interval (-0.25, 0.25)

   function Reduce (X : Double) return Double;
   --  Implement partial reduction of X by Pi in the x86.

   --  Note that for the Sin, Cos and Tan functions completely accurate
   --  reduction of the argument is done for arguments in the range of
   --  -2.0**63 .. 2.0**63, using a 66-bit approximation of Pi.

   pragma Inline (Is_Nan);
   pragma Inline (Reduce);

   ---------------------------------
   --  Basic Elementary Functions --
   ---------------------------------

   --  This section implements a few elementary functions that are
   --  used to build the more complex ones. This ordering enables
   --  better inlining.

   ----------
   -- Atan --
   ----------

   function Atan (X : Double) return Double is
      Result  : Double;

   begin
      Asm (Template =>
           "fld1" & NL
         & "fpatan",
         Outputs  => Double'Asm_Output ("=t", Result),
         Inputs   => Double'Asm_Input  ("0", X));

      --  The result value is NaN iff input was invalid

      if not (Result = Result) then
         raise Argument_Error;
      end if;

      return Result;
   end Atan;

   ---------
   -- Exp --
   ---------

   function Exp (X : Double) return Double is
      Result : Double;
   begin
      Asm (Template =>
         "fldl2e               " & NL
       & "fmulp   %%st, %%st(1)" & NL -- X * log2 (E)
       & "fld     %%st(0)      " & NL
       & "frndint              " & NL -- Integer (X * Log2 (E))
       & "fsubr   %%st, %%st(1)" & NL -- Fraction (X * Log2 (E))
       & "fxch                 " & NL
       & "f2xm1                " & NL -- 2**(...) - 1
       & "fld1                 " & NL
       & "faddp   %%st, %%st(1)" & NL -- 2**(Fraction (X * Log2 (E)))
       & "fscale               " & NL -- E ** X
       & "fstp    %%st(1)      ",
         Outputs  => Double'Asm_Output ("=t", Result),
         Inputs   => Double'Asm_Input  ("0", X));
      return Result;
   end Exp;

   ------------
   -- Is_Nan --
   ------------

   function Is_Nan (X : Double) return Boolean is
   begin
      --  The IEEE NaN values are the only ones that do not equal themselves

      return not (X = X);
   end Is_Nan;

   ---------
   -- Log --
   ---------

   function Log (X : Double) return Double is
      Result : Double;

   begin
      Asm (Template =>
         "fldln2               " & NL
       & "fxch                 " & NL
       & "fyl2x                " & NL,
         Outputs  => Double'Asm_Output ("=t", Result),
         Inputs   => Double'Asm_Input  ("0", X));
      return Result;
   end Log;

   ------------
   -- Reduce --
   ------------

   function Reduce (X : Double) return Double is
      Result : Double;
   begin
      Asm
        (Template =>
         --  Partial argument reduction
         "fldpi                " & NL
       & "fadd    %%st(0), %%st" & NL
       & "fxch    %%st(1)      " & NL
       & "fprem1               " & NL
       & "fstp    %%st(1)      ",
         Outputs  => Double'Asm_Output ("=t", Result),
         Inputs   => Double'Asm_Input  ("0", X));
      return Result;
   end Reduce;

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Double) return Double is
      Result  : Double;

   begin
      if X < 0.0 then
         raise Argument_Error;
      end if;

      Asm (Template => "fsqrt",
           Outputs  => Double'Asm_Output ("=t", Result),
           Inputs   => Double'Asm_Input  ("0", X));

      return Result;
   end Sqrt;

   ---------------------------------
   --  Other Elementary Functions --
   ---------------------------------

   --  These are built using the previously implemented basic functions

   ----------
   -- Acos --
   ----------

   function Acos (X : Double) return Double is
      Result  : Double;
   begin
      Result := 2.0 * Atan (Sqrt ((1.0 - X) / (1.0 + X)));

      --  The result value is NaN iff input was invalid

      if Is_Nan (Result) then
         raise Argument_Error;
      end if;

      return Result;
   end Acos;

   ----------
   -- Asin --
   ----------

   function Asin (X : Double) return Double is
      Result  : Double;
   begin

      Result := Atan (X / Sqrt ((1.0 - X) * (1.0 + X)));

      --  The result value is NaN iff input was invalid

      if Is_Nan (Result) then
         raise Argument_Error;
      end if;

      return Result;
   end Asin;

   ---------
   -- Cos --
   ---------

   function Cos (X : Double) return Double is
      Reduced_X : Double := X;
      Result    : Double;
      Status    : FPU_Status_Word;

   begin

      loop
         Asm
           (Template =>
            "fcos                 " & NL
          & "xorl    %%eax, %%eax " & NL
          & "fnstsw  %%ax         ",
            Outputs  => (Double'Asm_Output         ("=t", Result),
                        FPU_Status_Word'Asm_Output ("=a", Status)),
            Inputs   => Double'Asm_Input           ("0", Reduced_X));

         exit when not Status.C2;

         --  Original argument was not in range and the result
         --  is the unmodified argument.

         Reduced_X := Reduce (Result);
      end loop;

      return Result;
   end Cos;

   ---------------------
   -- Logarithmic_Pow --
   ---------------------

   function Logarithmic_Pow (X, Y : Double) return Double is
      Result  : Double;

   begin
      Asm (Template => ""             --  X                  : Y
       & "fyl2x                " & NL --  Y * Log2 (X)
       & "fst     %%st(1)      " & NL --  Y * Log2 (X)       : Y * Log2 (X)
       & "frndint              " & NL --  Int (...)          : Y * Log2 (X)
       & "fsubr   %%st, %%st(1)" & NL --  Int (...)          : Fract (...)
       & "fxch                 " & NL --  Fract (...)        : Int (...)
       & "f2xm1                " & NL --  2**Fract (...) - 1 : Int (...)
       & "fld1                 " & NL --  1 : 2**Fract (...) - 1 : Int (...)
       & "faddp   %%st, %%st(1)" & NL --  2**Fract (...)     : Int (...)
       & "fscale               " & NL --  2**(Fract (...) + Int (...))
       & "fstp    %%st(1)      ",
         Outputs  => Double'Asm_Output ("=t", Result),
         Inputs   =>
           (Double'Asm_Input  ("0", X),
            Double'Asm_Input  ("u", Y)));

      return Result;
   end Logarithmic_Pow;

   ---------
   -- Pow --
   ---------

   function Pow (X, Y : Double) return Double is
      type Mantissa_Type is mod 2**Double'Machine_Mantissa;
      --  Modular type that can hold all bits of the mantissa of Double

      --  For negative exponents, a division is done
      --  at the end of the processing.

      Negative_Y : constant Boolean := Y < 0.0;
      Abs_Y      : constant Double := abs Y;

      --  During this function the following invariant is kept:
      --  X ** (abs Y) = Base**(Exp_High + Exp_Mid + Exp_Low) * Factor

      Base : Double := X;

      Exp_High : Double := Double'Floor (Abs_Y);
      Exp_Mid  : Double;
      Exp_Low  : Double;
      Exp_Int  : Mantissa_Type;

      Factor : Double := 1.0;

   begin
      --  Select algorithm for calculating Pow:
      --  integer cases fall through

      if Exp_High >= 2.0**Double'Machine_Mantissa then

         --  In case of Y that is IEEE infinity, just raise constraint error

         if Exp_High > Double'Safe_Last then
            raise Constraint_Error;
         end if;

         --  Large values of Y are even integers and will stay integer
         --  after division by two.

         loop
            --  Exp_Mid and Exp_Low are zero, so
            --    X**(abs Y) = Base ** Exp_High = (Base**2) ** (Exp_High / 2)

            Exp_High := Exp_High / 2.0;
            Base := Base * Base;
            exit when Exp_High < 2.0**Double'Machine_Mantissa;
         end loop;

      elsif Exp_High /= Abs_Y then
         Exp_Low := Abs_Y - Exp_High;

         Factor := 1.0;

         if Exp_Low /= 0.0 then

            --  Exp_Low now is in interval (0.0, 1.0)
            --  Exp_Mid := Double'Floor (Exp_Low * 4.0) / 4.0;

            Exp_Mid := 0.0;
            Exp_Low := Exp_Low - Exp_Mid;

            if Exp_Low >= 0.5 then
               Factor := Sqrt (X);
               Exp_Low := Exp_Low - 0.5;  -- exact

               if Exp_Low >= 0.25 then
                  Factor := Factor * Sqrt (Factor);
                  Exp_Low := Exp_Low - 0.25; --  exact
               end if;

            elsif Exp_Low >= 0.25 then
               Factor := Sqrt (Sqrt (X));
               Exp_Low := Exp_Low - 0.25; --  exact
            end if;

            --  Exp_Low now is in interval (0.0, 0.25)

            --  This means it is safe to call Logarithmic_Pow
            --  for the remaining part.

            Factor := Factor * Logarithmic_Pow (X, Exp_Low);
         end if;

      elsif X = 0.0 then
         return 0.0;
      end if;

      --  Exp_High is non-zero integer smaller than 2**Double'Machine_Mantissa

      Exp_Int := Mantissa_Type (Exp_High);

      --  Standard way for processing integer powers > 0

      while Exp_Int > 1 loop
         if (Exp_Int and 1) = 1 then

            --  Base**Y = Base**(Exp_Int - 1) * Exp_Int for Exp_Int > 0

            Factor := Factor * Base;
         end if;

         --  Exp_Int is even and Exp_Int > 0, so
         --    Base**Y = (Base**2)**(Exp_Int / 2)

         Base := Base * Base;
         Exp_Int := Exp_Int / 2;
      end loop;

      --  Exp_Int = 1 or Exp_Int = 0

      if Exp_Int = 1 then
         Factor := Base * Factor;
      end if;

      if Negative_Y then
         Factor := 1.0 / Factor;
      end if;

      return Factor;
   end Pow;

   ---------
   -- Sin --
   ---------

   function Sin (X : Double) return Double is
      Reduced_X : Double := X;
      Result    : Double;
      Status    : FPU_Status_Word;

   begin

      loop
         Asm
           (Template =>
            "fsin                 " & NL
          & "xorl    %%eax, %%eax " & NL
          & "fnstsw  %%ax         ",
            Outputs  => (Double'Asm_Output          ("=t", Result),
                         FPU_Status_Word'Asm_Output ("=a", Status)),
            Inputs   => Double'Asm_Input            ("0", Reduced_X));

         exit when not Status.C2;

         --  Original argument was not in range and the result
         --  is the unmodified argument.

         Reduced_X := Reduce (Result);
      end loop;

      return Result;
   end Sin;

   ---------
   -- Tan --
   ---------

   function Tan (X : Double) return Double is
      Reduced_X : Double := X;
      Result    : Double;
      Status    : FPU_Status_Word;

   begin

      loop
         Asm
           (Template =>
            "fptan                " & NL
          & "xorl    %%eax, %%eax " & NL
          & "fnstsw  %%ax         " & NL
          & "ffree   %%st(0)      " & NL
          & "fincstp              ",

            Outputs  => (Double'Asm_Output         ("=t", Result),
                        FPU_Status_Word'Asm_Output ("=a", Status)),
            Inputs   => Double'Asm_Input           ("0", Reduced_X));

         exit when not Status.C2;

         --  Original argument was not in range and the result
         --  is the unmodified argument.

         Reduced_X := Reduce (Result);
      end loop;

      return Result;
   end Tan;

   ----------
   -- Sinh --
   ----------

   function Sinh (X : Double) return Double is
   begin
      --  Mathematically Sinh (x) is defined to be (Exp (X) - Exp (-X)) / 2.0

      if abs X < 25.0 then
         return (Exp (X) - Exp (-X)) / 2.0;

      else
         return Exp (X) / 2.0;
      end if;

   end Sinh;

   ----------
   -- Cosh --
   ----------

   function Cosh (X : Double) return Double is
   begin
      --  Mathematically Cosh (X) is defined to be (Exp (X) + Exp (-X)) / 2.0

      if abs X < 22.0 then
         return (Exp (X) + Exp (-X)) / 2.0;

      else
         return Exp (X) / 2.0;
      end if;

   end Cosh;

   ----------
   -- Tanh --
   ----------

   function Tanh (X : Double) return Double is
   begin
      --  Return the Hyperbolic Tangent of x
      --
      --                                    x    -x
      --                                   e  - e        Sinh (X)
      --       Tanh (X) is defined to be -----------   = --------
      --                                    x    -x      Cosh (X)
      --                                   e  + e

      if abs X > 23.0 then
         return Double'Copy_Sign (1.0, X);
      end if;

      return 1.0 / (1.0 + Exp (-2.0 * X)) - 1.0 / (1.0 + Exp (2.0 * X));

   end Tanh;

end Ada.Numerics.Aux;
