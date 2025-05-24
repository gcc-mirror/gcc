-- CXG1005.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that the subprograms defined in the package
--      Ada.Numerics.Generic_Complex_Elementary_Functions provide correct
--      results.
--      
-- TEST DESCRIPTION:
--      This test checks that specific subprograms defined in the generic
--      package Generic_Complex_Elementary_Functions are available, and that
--      they provide prescribed results given specific input values.
--      The generic package Ada.Numerics.Generic_Complex_Types is instantiated 
--      with a real type (new Float). The resulting new package is used as
--      the generic actual to package Complex_IO.
--      
-- SPECIAL REQUIREMENTS:
--      Implementations for which Float'Signed_Zeros is True must provide
--      a body for ImpDef.Annex_G.Negative_Zero which returns a negative 
--      zero.
--
-- APPLICABILITY CRITERIA
--      This test only applies to implementations that support the
--      numerics annex.
--
--         
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      16 Nov 95   SAIC    Corrected visibility problems for ACVC 2.0.1.
--      21 Feb 96   SAIC    Incorporated new structure for package Impdef.
--      29 Sep 96   SAIC    Incorporated reviewer comments.
--
--!

with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with ImpDef.Annex_G;
with Report;

procedure CXG1005 is
begin

   Report.Test ("CXG1005", "Check that the subprograms defined in "  &
                           "the package Generic_Complex_Elementary_" & 
                           "Functions provide correct results");

   Test_Block:
   declare

      type Real_Type is new Float;

      TC_Signed_Zeros : Boolean := Real_Type'Signed_Zeros; 

      package Complex_Pack is new 
        Ada.Numerics.Generic_Complex_Types(Real_Type);

      package CEF is 
        new Ada.Numerics.Generic_Complex_Elementary_Functions(Complex_Pack);

      use Ada.Numerics, Complex_Pack, CEF;

      Complex_Zero : constant Complex := Compose_From_Cartesian( 0.0, 0.0);
      Plus_One     : constant Complex := Compose_From_Cartesian( 1.0, 0.0);
      Minus_One    : constant Complex := Compose_From_Cartesian(-1.0, 0.0);
      Plus_i       : constant Complex := Compose_From_Cartesian(i);
      Minus_i      : constant Complex := Compose_From_Cartesian(-i);

      Complex_Positive_Real      : constant Complex := 
                                         Compose_From_Cartesian(4.0, 2.0);
      Complex_Positive_Imaginary : constant Complex := 
                                         Compose_From_Cartesian(3.0, 5.0);
      Complex_Negative_Real      : constant Complex := 
                                         Compose_From_Cartesian(-4.0, 2.0);
      Complex_Negative_Imaginary : constant Complex := 
                                         Compose_From_Cartesian(3.0, -5.0);


      function A_Zero_Result (Z : Complex) return Boolean is
      begin
         return (Re(Z) = 0.0 and Im(Z) = 0.0);
      end A_Zero_Result;


      -- In order to evaluate complex elementary functions that are
      -- prescribed to return a "real" result (meaning that the imaginary
      -- component is zero), the Function A_Real_Result is defined.

      function A_Real_Result (Z : Complex) return Boolean is
      begin
         return Im(Z) = 0.0;
      end A_Real_Result;


      -- In order to evaluate complex elementary functions that are
      -- prescribed to return an "imaginary" result (meaning that the real
      -- component of the complex number is zero, and the imaginary
      -- component is non-zero), the Function An_Imaginary_Result is defined.

      function An_Imaginary_Result (Z : Complex) return Boolean is
      begin
         return  (Re(Z) = 0.0 and Im(Z) /= 0.0);
      end An_Imaginary_Result;


   begin

      -- Check that when the input parameter value is zero, the following
      -- functions yield a zero result.

      if not A_Zero_Result( Sqrt(Complex_Zero) ) then
         Report.Failed("Non-zero result from Function Sqrt with zero input");
      end if;

      if not A_Zero_Result( Sin(Complex_Zero) ) then
         Report.Failed("Non-zero result from Function Sin with zero input");
      end if;

      if not A_Zero_Result( Arcsin(Complex_Zero) ) then
         Report.Failed("Non-zero result from Function Arcsin with zero " &
                       "input");
      end if;

      if not A_Zero_Result( Tan(Complex_Zero) ) then
         Report.Failed("Non-zero result from Function Tan with zero input");
      end if;

      if not A_Zero_Result( Arctan(Complex_Zero) ) then
         Report.Failed("Non-zero result from Function Arctan with zero " &
                       "input");
      end if;

      if not A_Zero_Result( Sinh(Complex_Zero) ) then
         Report.Failed("Non-zero result from Function Sinh with zero input");
      end if;

      if not A_Zero_Result( Arcsinh(Complex_Zero) ) then
         Report.Failed("Non-zero result from Function Arcsinh with zero " &
                       "input");
      end if;

      if not A_Zero_Result( Tanh(Complex_Zero) ) then
         Report.Failed("Non-zero result from Function Tanh with zero input");
      end if;

      if not A_Zero_Result( Arctanh(Complex_Zero) ) then
         Report.Failed("Non-zero result from Function Arctanh with zero " &
                       "input");
      end if;


      -- Check that when the input parameter value is zero, the following
      -- functions yield a result of one.

      if Exp(Complex_Zero) /= Plus_One 
      then
         Report.Failed("Non-zero result from Function Exp with zero input");
      end if;

      if Cos(Complex_Zero) /= Plus_One 
      then
         Report.Failed("Non-zero result from Function Cos with zero input");
      end if;

      if Cosh(Complex_Zero) /= Plus_One 
      then
         Report.Failed("Non-zero result from Function Cosh with zero input");
      end if;


      -- Check that when the input parameter value is zero, the following
      -- functions yield a real result.

      if not A_Real_Result( Arccos(Complex_Zero) ) then
        Report.Failed("Non-real result from Function Arccos with zero input");
      end if;

      if not A_Real_Result( Arccot(Complex_Zero) ) then
        Report.Failed("Non-real result from Function Arccot with zero input");
      end if;


      -- Check that when the input parameter value is zero, the following
      -- functions yield an imaginary result.

      if not An_Imaginary_Result( Arccoth(Complex_Zero) ) then
        Report.Failed("Non-imaginary result from Function Arccoth with " &
                      "zero input");
      end if;


      -- Check that when the input parameter value is one, the Sqrt function 
      -- yields a result of one.

      if Sqrt(Plus_One) /= Plus_One then
         Report.Failed("Incorrect result from Function Sqrt with input " &
                       "value of one");
      end if;


      -- Check that when the input parameter value is one, the following 
      -- functions yield a result of zero.

      if not A_Zero_Result( Log(Plus_One) ) then
         Report.Failed("Non-zero result from Function Log with input " &
                       "value of one");
      end if;

      if not A_Zero_Result( Arccos(Plus_One) ) then
         Report.Failed("Non-zero result from Function Arccos with input " &
                       "value of one");
      end if;

      if not A_Zero_Result( Arccosh(Plus_One) ) then
         Report.Failed("Non-zero result from Function Arccosh with input " &
                       "value of one");
      end if;


      -- Check that when the input parameter value is one, the Arcsin 
      -- function yields a real result.

      if not A_Real_Result( Arcsin(Plus_One) ) then
         Report.Failed("Non-real result from Function Arcsin with input " &
                       "value of one");
      end if;


      -- Check that when the input parameter value is minus one, the Sqrt 
      -- function yields a result of "i", when the sign of the imaginary
      -- component of the input parameter is positive (and yields "-i", if 
      -- the sign on the imaginary component is negative), and the 
      -- Complex_Types.Real'Signed_Zeros attribute is True.

      if TC_Signed_Zeros then

         declare
            Minus_One_With_Pos_Zero_Im_Component : Complex := 
                                  Compose_From_Cartesian(-1.0, +0.0);
            Minus_One_With_Neg_Zero_Im_Component : Complex := 
              Compose_From_Cartesian
                (-1.0, Real_Type(ImpDef.Annex_G.Negative_Zero));
         begin

            if Sqrt(Minus_One_With_Pos_Zero_Im_Component) /= Plus_i then
               Report.Failed("Incorrect result from Function Sqrt, when " &
                             "input value is minus one with a positive "  &
                             "imaginary component, Signed_Zeros being True");
            end if;

            if Sqrt(Minus_One_With_Neg_Zero_Im_Component) /= Minus_i then
               Report.Failed("Incorrect result from Function Sqrt, when " &
                             "input value is minus one with a negative "  &
                             "imaginary component, Signed_Zeros being True");
            end if;
         end;

      else   -- Signed_Zeros is False.

         -- Check that when the input parameter value is minus one, the Sqrt 
         -- function yields a result of "i", when the 
         -- Complex_Types.Real'Signed_Zeros attribute is False.

         if Sqrt(Minus_One) /= Plus_i then
            Report.Failed("Incorrect result from Function Sqrt, when "    &
                          "input value is minus one, Signed_Zeros being " & 
                          "False");
         end if;

      end if;


      -- Check that when the input parameter value is minus one, the Log
      -- function yields an imaginary result.

      if not An_Imaginary_Result( Log(Minus_One) ) then
         Report.Failed("Non-imaginary result from Function Log with a " &
                       "minus one input value");
      end if;

      -- Check that when the input parameter is minus one, the following 
      -- functions yield a real result.

      if not A_Real_Result( Arcsin(Minus_One) ) then
         Report.Failed("Non-real result from Function Arcsin with a " &
                       "minus one input value");
      end if;

      if not A_Real_Result( Arccos(Minus_One) ) then
         Report.Failed("Non-real result from Function Arccos with a " &
                       "minus one input value");
      end if;


      -- Check that when the input parameter has a value of +i or -i, the
      -- Log function yields an imaginary result.

      if not An_Imaginary_Result( Log(Plus_i) ) then
         Report.Failed("Non-imaginary result from Function Log with an " &
                       "input value of ""+i""");
      end if;

      if not An_Imaginary_Result( Log(Minus_i) ) then
         Report.Failed("Non-imaginary result from Function Log with an " &
                       "input value of ""-i""");
      end if;


      -- Check that exponentiation by a zero exponent yields the value one.

      if "**"(Left  => Compose_From_Cartesian(5.0, 3.0), 
              Right => Complex_Zero)                     /= Plus_One  or
         Complex_Negative_Real**0.0                      /= Plus_One  or
         15.0**Complex_Zero                              /= Plus_One
      then
         Report.Failed("Incorrect result from exponentiation with a zero " &
                       "exponent");
      end if;


      -- Check that exponentiation by a unit exponent yields the value of 
      -- the left operand (as a complex value).
      -- Note: a "unit exponent" is considered the complex number (1.0, 0.0)

      if "**"(Complex_Negative_Real, Plus_One) /= 
         Complex_Negative_Real                    or
         Complex_Negative_Imaginary**Plus_One  /= 
         Complex_Negative_Imaginary               or
         4.0**Plus_One                         /= 
         Compose_From_Cartesian(4.0, 0.0)
      then
         Report.Failed("Incorrect result from exponentiation with a unit " &
                       "exponent");
      end if;


      -- Check that exponentiation of the value one yields the value one.

      if "**"(Plus_One, Complex_Negative_Imaginary) /= Plus_One  or
         Plus_One**9.0                              /= Plus_One  or
         1.0**Complex_Negative_Real                 /= Plus_One
      then
         Report.Failed("Incorrect result from exponentiation of the value " &
                       "One");
      end if;


      -- Check that exponentiation of the value zero yields the value zero.
      begin
         if not A_Zero_Result("**"(Complex_Zero, 
                                   Complex_Positive_Imaginary)) or
            not A_Zero_Result(Complex_Zero**4.0)                or
            not A_Zero_Result(0.0**Complex_Positive_Real)
         then
            Report.Failed("Incorrect result from exponentiation of the " &
                          "value zero");
         end if;
      exception
         when others =>
           Report.Failed("Exception raised during the exponentiation of " &
                         "the complex value zero");
      end;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXG1005;
