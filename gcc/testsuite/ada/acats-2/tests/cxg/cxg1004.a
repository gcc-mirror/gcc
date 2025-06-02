-- CXG1004.A
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
--      Check that the specified exceptions are raised by the subprograms
--      defined in package Ada.Numerics.Generic_Complex_Elementary_Functions
--      given the prescribed input parameter values.
--
-- TEST DESCRIPTION:
--      This test checks that specific subprograms defined in the 
--      package Ada.Numerics.Generic_Complex_Elementary_Functions raise the
--      exceptions Argument_Error and Constraint_Error when their input
--      parameter value are those specified as causing each exception.
--      In the case of Constraint_Error, the exception will be raised in
--      each test case, provided that the value of the attribute
--      'Machine_Overflows (for the actual type of package 
--      Generic_Complex_Type) is True.
--      
-- APPLICABILITY CRITERIA: 
--      This test only applies to implementations supporting the 
--      numerics annex.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      16 Nov 95   SAIC    Corrected visibility problems for ACVC 2.0.1.
--      29 Sep 96   SAIC    Incorporated reviewer comments.
--      02 Jun 98   EDS     Replace "_i" with "_One".
--!

with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Report;

procedure CXG1004 is
begin

   Report.Test ("CXG1004", "Check that the specified exceptions are "      &
                           "raised by the subprograms defined in package " &
                           "Ada.Numerics.Generic_Complex_Elementary_"      &
                           "Functions given the prescribed input "         &
                           "parameter values");

   Test_Block:
   declare

      type Real_Type is new Float;

      TC_Overflows : Boolean := Real_Type'Machine_Overflows;

      package Complex_Pack is 
        new Ada.Numerics.Generic_Complex_Types(Real_Type);

      package CEF is 
        new Ada.Numerics.Generic_Complex_Elementary_Functions(Complex_Pack);

      use Ada.Numerics, Complex_Pack, CEF;

      Complex_Zero : constant Complex := Compose_From_Cartesian(0.0, 0.0);
      Plus_One     : constant Complex := Compose_From_Cartesian(1.0, 0.0);
      Minus_One    : constant Complex := Compose_From_Cartesian(-1.0, 0.0);
      Plus_i       : constant Complex := Compose_From_Cartesian(i);
      Minus_i      : constant Complex := Compose_From_Cartesian(-i);

      Complex_Negative_Real      : constant Complex := 
                                            Compose_From_Cartesian(-4.0, 2.0);
      Complex_Negative_Imaginary : constant Complex := 
                                            Compose_From_Cartesian(3.0, -5.0);

      TC_Complex   : Complex;


      -- This procedure is used in "Exception Raising" calls below in an
      -- attempt to avoid elimination of the subtest through optimization.

      procedure No_Optimize (The_Complex_Number : Complex) is
      begin
         Report.Comment("No Optimize: Should never be printed " &
                        Integer'Image(Integer(The_Complex_Number.Im)));
      end No_Optimize;


   begin

      -- Check that the exception Numerics.Argument_Error is raised by the
      -- exponentiation operator when the value of the left operand is zero,
      -- and the real component of the exponent (or the exponent itself) is
      -- zero.

      begin
         TC_Complex := "**"(Left => Complex_Zero, Right => Complex_Zero);
         Report.Failed("Argument_Error not raised by exponentiation "  &
                       "operator, left operand = complex zero, right " &
                       "operand = complex zero");
         No_Optimize(TC_Complex);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Incorrect exception raised by exponentiation " &
                          "operator, left operand = complex zero, right " &
                          "operand = complex zero");
      end;

      begin
         TC_Complex := Complex_Zero**0.0;
         Report.Failed("Argument_Error not raised by exponentiation "  &
                       "operator, left operand = complex zero, right " &
                       "operand = real zero");
         No_Optimize(TC_Complex);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Incorrect exception raised by exponentiation " &
                          "operator, left operand = complex zero, right " &
                          "operand = real zero");
      end;


      begin
         TC_Complex := "**"(Left => 0.0, Right => Complex_Zero);
         Report.Failed("Argument_Error not raised by exponentiation " &
                       "operator, left operand = real zero, right "   &
                       "operand = complex zero");
         No_Optimize(TC_Complex);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Incorrect exception raised by exponentiation " &
                          "operator, left operand = real zero, right "    &
                          "operand = complex zero");
      end;


      -- Check that the exception Constraint_Error is raised under the 
      -- specified circumstances, provided that
      -- Complex_Types.Real'Machine_Overflows is True.

      if TC_Overflows then

         -- Raised by Log, when the value of the parameter X is zero.
         begin
            TC_Complex := Log (X => Complex_Zero);
            Report.Failed("Constraint_Error not raised when Function " &
                          "Log given parameter value of complex zero");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when Function " &
                             "Log given parameter value of complex zero");
         end;

         -- Raised by Cot, when the value of the parameter X is zero.
         begin
            TC_Complex := Cot (X => Complex_Zero);
            Report.Failed("Constraint_Error not raised when Function " &
                          "Cot given parameter value of complex zero");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when Function " &
                             "Cot given parameter value of complex zero");
         end;

         -- Raised by Coth, when the value of the parameter X is zero.
         begin
            TC_Complex := Coth (Complex_Zero);
            Report.Failed("Constraint_Error not raised when Function " &
                          "Coth given parameter value of complex zero");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when Function " &
                             "Coth given parameter value of complex zero");
         end;

         -- Raised by the exponentiation operator, when the value of the 
         -- left operand is zero and the real component of the exponent
         -- is negative.
         begin
            TC_Complex := Complex_Zero**Complex_Negative_Real;
            Report.Failed("Constraint_Error not raised when the "    &
                          "exponentiation operator left operand is " &
                          "complex zero, and the real component of " &
                          "the exponent is negative");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when the "     &
                             "exponentiation operator left operand is " &
                             "complex zero, and the real component of " &
                             "the exponent is negative");
         end;

         -- Raised by the exponentiation operator, when the value of the 
         -- left operand is zero and the exponent itself (when it is of
         -- type real) is negative.
         declare
            Negative_Exponent : constant Real_Type := -4.0;
         begin
            TC_Complex := Complex_Zero**Negative_Exponent;
            Report.Failed("Constraint_Error not raised when the "    &
                          "exponentiation operator left operand is " &
                          "complex zero, and the real exponent is "  &
                          "negative");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when the "     &
                             "exponentiation operator left operand is " &
                             "complex zero, and the real exponent is "  &
                             "negative");
         end;

         -- Raised by Arctan, when the value of the parameter is +i.
         begin
            TC_Complex := Arctan (Plus_i);
            Report.Failed("Constraint_Error not raised when Function " &
                          "Arctan is given parameter value +i");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when Function " &
                             "Arctan is given parameter value +i");
         end;

         -- Raised by Arctan, when the value of the parameter is -i.
         begin
            TC_Complex := Arctan (Minus_i);
            Report.Failed("Constraint_Error not raised when Function " &
                          "Arctan is given parameter value -i");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when Function " &
                             "Arctan is given parameter value -i");
         end;

         -- Raised by Arccot, when the value of the parameter is +i.
         begin
            TC_Complex := Arccot (Plus_i);
            Report.Failed("Constraint_Error not raised when Function " &
                          "Arccot is given parameter value +i");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when Function " &
                             "Arccot is given parameter value +i");
         end;

         -- Raised by Arccot, when the value of the parameter is -i.
         begin
            TC_Complex := Arccot (Minus_i);
            Report.Failed("Constraint_Error not raised when Function " &
                          "Arccot is given parameter value -i");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when Function " &
                             "Arccot is given parameter value -i");
         end;

         -- Raised by Arctanh, when the value of the parameter is +1.
         begin
            TC_Complex := Arctanh (Plus_One);
            Report.Failed("Constraint_Error not raised when Function " &
                          "Arctanh is given parameter value +1");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when Function " &
                             "Arctanh is given parameter value +1");
         end;

         -- Raised by Arctanh, when the value of the parameter is -1.
         begin
            TC_Complex := Arctanh (Minus_One);
            Report.Failed("Constraint_Error not raised when Function " &
                          "Arctanh is given parameter value -1");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when Function " &
                             "Arctanh is given parameter value -1");
         end;

         -- Raised by Arccoth, when the value of the parameter is +1.
         begin
            TC_Complex := Arccoth (Plus_One);
            Report.Failed("Constraint_Error not raised when Function " &
                          "Arccoth is given parameter value +1");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when Function " &
                             "Arccoth is given parameter value +1");
         end;

         -- Raised by Arccoth, when the value of the parameter is -1.
         begin
            TC_Complex := Arccoth (Minus_One);
            Report.Failed("Constraint_Error not raised when Function " &
                          "Arccoth is given parameter value -1");
            No_Optimize(TC_Complex);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others =>
               Report.Failed("Incorrect exception raised when Function " &
                             "Arccoth is given parameter value -1");
         end;

      else
         Report.Comment
           ("Attribute Complex_Pack.Real'Machine_Overflows is False; " &
            "evaluation of the complex elementary functions under "    &
            "specified circumstances was not performed");
      end if;


   exception
      when others => 
        Report.Failed ("Unexpected exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXG1004;
