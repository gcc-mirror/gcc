-- CXG2018.A
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
--      Check that the complex EXP function returns
--      a result that is within the error bound allowed.
--
-- TEST DESCRIPTION:
--      This test consists of a generic package that is
--      instantiated to check complex numbers based upon
--      both Float and a long float type.
--      The test for each floating point type is divided into
--      several parts:
--         Special value checks where the result is a known constant.
--         Checks that use an identity for determining the result.
--
-- SPECIAL REQUIREMENTS
--      The Strict Mode for the numerical accuracy must be
--      selected.  The method by which this mode is selected
--      is implementation dependent.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Numerics Annex.
--      This test only applies to the Strict Mode for numerical
--      accuracy.
--
--
-- CHANGE HISTORY:
--      21 Mar 96   SAIC    Initial release for 2.1
--      17 Aug 96   SAIC    Incorporated reviewer comments.
--      27 Aug 99   RLB     Repair on the error result of checks.
--      02 Apr 03   RLB     Added code to discard excess precision in the
--                          construction of the test value for the
--                          Identity_Test.
--
--!

--
-- References:
--
-- W. J. Cody
-- CELEFUNT: A Portable Test Package for Complex Elementary Functions
-- Algorithm 714, Collected Algorithms from ACM.
-- Published in Transactions On Mathematical Software,
-- Vol. 19, No. 1, March, 1993, pp. 1-21.
--
-- CRC Standard Mathematical Tables
-- 23rd Edition
--

with System;
with Report;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;
procedure CXG2018 is
   Verbose : constant Boolean := False;
   -- Note that Max_Samples is the number of samples taken in
   -- both the real and imaginary directions.  Thus, for Max_Samples
   -- of 100 the number of values checked is 10000.
   Max_Samples : constant := 100;

   E  : constant := Ada.Numerics.E;
   Pi : constant := Ada.Numerics.Pi;

   generic
      type Real is digits <>;
   package Generic_Check is
      procedure Do_Test;
   end Generic_Check;

   package body Generic_Check is
      package Complex_Type is new
           Ada.Numerics.Generic_Complex_Types (Real);
      use Complex_Type;

      package CEF is new
           Ada.Numerics.Generic_Complex_Elementary_Functions (Complex_Type);

      function Exp (X : Complex) return Complex renames CEF.Exp;
      function Exp (X : Imaginary) return Complex renames CEF.Exp;

      -- flag used to terminate some tests early
      Accuracy_Error_Reported : Boolean := False;


      -- The following value is a lower bound on the accuracy
      -- required.  It is normally 0.0 so that the lower bound
      -- is computed from Model_Epsilon.  However, for tests
      -- where the expected result is only known to a certain
      -- amount of precision this bound takes on a non-zero
      -- value to account for that level of precision.
      Error_Low_Bound : Real := 0.0;

      procedure Check (Actual, Expected : Real;
                       Test_Name : String;
                       MRE : Real) is
         Max_Error : Real;
         Rel_Error : Real;
         Abs_Error : Real;
      begin
         -- In the case where the expected result is very small or 0
         -- we compute the maximum error as a multiple of Model_Small instead
         -- of Model_Epsilon and Expected.
         Rel_Error := MRE * abs Expected * Real'Model_Epsilon;
         Abs_Error := MRE * Real'Model_Small;
         if Rel_Error > Abs_Error then
            Max_Error := Rel_Error;
         else
            Max_Error := Abs_Error;
         end if;

         -- take into account the low bound on the error
         if Max_Error < Error_Low_Bound then
            Max_Error := Error_Low_Bound;
         end if;

         if abs (Actual - Expected) > Max_Error then
            Accuracy_Error_Reported := True;
            Report.Failed (Test_Name &
                           " actual: " & Real'Image (Actual) &
                           " expected: " & Real'Image (Expected) &
                           " difference: " & Real'Image (Actual - Expected) &
                           " max err:" & Real'Image (Max_Error) );
         elsif Verbose then
	    if Actual = Expected then
	       Report.Comment (Test_Name & "  exact result");
	    else
	       Report.Comment (Test_Name & "  passed");
	    end if;
         end if;
      end Check;


      procedure Check (Actual, Expected : Complex;
                       Test_Name : String;
                       MRE : Real) is
      begin
         Check (Actual.Re, Expected.Re, Test_Name & " real part", MRE);
         Check (Actual.Im, Expected.Im, Test_Name & " imaginary part", MRE);
      end Check;


      procedure Special_Value_Test is
         -- In the following tests the expected result is accurate
         -- to the machine precision so the minimum guaranteed error
         -- bound can be used.
         --
         -- The error bounds given assumed z is  exact.  When using
         -- pi there is an extra error of 1.0ME.
         -- The pi inside the exp call requires that the complex
         -- component have an extra error allowance of 1.0*angle*ME.
         -- Thus for pi/2,the Minimum_Error_I is
         -- (2.0 + 1.0(pi/2))ME <= 3.6ME.
         -- For pi, it is (2.0 + 1.0*pi)ME <= 5.2ME,
         -- and for 2pi, it is (2.0 + 1.0(2pi))ME <= 8.3ME.

         -- The addition of 1 or i to a result is so that neither of
         -- the components of an expected result is 0.  This is so
         -- that a reasonable relative error is allowed.
         Minimum_Error_C : constant := 7.0;   -- for exp(Complex)
         Minimum_Error_I : constant := 2.0;   -- for exp(Imaginary)
      begin
         Check (Exp (1.0 + 0.0*i) + i,
                E + i,
                "exp(1+0i)",
                Minimum_Error_C);
         Check (Exp ((Pi / 2.0) * i) + 1.0,
                1.0 + 1.0*i,
                "exp(pi/2*i)",
                3.6);
         Check (Exp (Pi * i) + i,
                -1.0 + 1.0*i,
                "exp(pi*i)",
                5.2);
         Check (Exp (Pi * 2.0 * i) + i,
                1.0 + i,
                "exp(2pi*i)",
                8.3);
      exception
         when Constraint_Error =>
            Report.Failed ("Constraint_Error raised in special value test");
         when others =>
            Report.Failed ("exception in special value test");
      end Special_Value_Test;



      procedure Exact_Result_Test is
         No_Error : constant := 0.0;
      begin
         -- G.1.2(36);6.0
         Check (Exp(0.0 + 0.0*i),  1.0 + 0.0 * i, "exp(0+0i)", No_Error);
         Check (Exp(      0.0*i),  1.0 + 0.0 * i, "exp(0i)", No_Error);
      exception
         when Constraint_Error =>
            Report.Failed ("Constraint_Error raised in Exact_Result Test");
         when others =>
            Report.Failed ("exception in Exact_Result Test");
      end Exact_Result_Test;


      procedure Identity_Test (A, B : Real) is
      -- For this test we use the identity
      --    Exp(Z) = Exp(Z-W) * Exp (W)
      -- where W = (1+i)/16
      --
      -- The second part of this test checks the identity
      --    Exp(Z) * Exp(-Z)  = 1
      --

         X, Y : Complex;
         Actual1, Actual2 : Complex;
         W : constant Complex := (0.0625, 0.0625);
         -- the following constant was taken from the CELEFUNC EXP test.
         -- This is the value EXP(W) - 1
         C : constant Complex := (6.2416044877018563681e-2,
                                  6.6487597751003112768e-2);
      begin
         if Real'Digits > 20 then
            -- constant ExpW is accurate to 20 digits.
            -- The low bound is 19 * 10**-20
            Error_Low_Bound := 0.00000_00000_00019;
            Report.Comment ("complex exp accuracy checked to 20 digits");
         end if;

         Accuracy_Error_Reported := False;  -- reset
         for II in 1..Max_Samples loop
            X.Re :=  Real'Machine ((B - A) * Real (II) / Real (Max_Samples)
                        + A);
            for J in 1..Max_Samples loop
               X.Im :=  Real'Machine ((B - A) * Real (J) / Real (Max_Samples)
                           + A);

               Actual1 := Exp(X);

               -- Exp(X) = Exp(X-W) * Exp (W)
               --        = Exp(X-W) * (1 - (1-Exp(W))
               --        = Exp(X-W) * (1 + (Exp(W) - 1))
               --        = Exp(X-W) * (1 + C)
               Y := X - W;
               Actual2 := Exp(Y);
               Actual2 := Actual2 + Actual2 * C;

               Check (Actual1, Actual2,
                      "Identity_1_Test " & Integer'Image (II) &
                         Integer'Image (J) & ": Exp((" &
		         Real'Image (X.Re) & ", " &
		         Real'Image (X.Im) & ")) ",
                      20.0);   -- 2 exp and 1 multiply and 1 add = 2*7+1*5+1
                    -- Note: The above is not strictly correct, as multiply
                    -- has a box error, rather than a relative error.
                    -- Supposedly, the interval is chosen to avoid the need
                    -- to worry about this.

               -- Exp(X) * Exp(-X) + i  = 1 + i
               -- The addition of i is to allow a reasonable relative
               -- error in the imaginary part
               Actual2 := (Actual1 * Exp(-X)) + i;
               Check (Actual2, (1.0, 1.0),
                      "Identity_2_Test " & Integer'Image (II) &
                         Integer'Image (J) & ": Exp((" &
		         Real'Image (X.Re) & ", " &
		         Real'Image (X.Im) & ")) ",
                      20.0);   -- 2 exp and 1 multiply and one add = 2*7+1*5+1

               if Accuracy_Error_Reported then
                 -- only report the first error in this test in order to keep
                 -- lots of failures from producing a huge error log
                 return;
               end if;
            end loop;
         end loop;
         Error_Low_Bound := 0.0;
      exception
         when Constraint_Error =>
            Report.Failed
               ("Constraint_Error raised in Identity_Test" &
                " for X=(" & Real'Image (X.Re) &
                ", " & Real'Image (X.Im) & ")");
         when others =>
            Report.Failed ("exception in Identity_Test" &
                " for X=(" & Real'Image (X.Re) &
                ", " & Real'Image (X.Im) & ")");
      end Identity_Test;



      procedure Do_Test is
      begin
         Special_Value_Test;
         Exact_Result_Test;
            -- test regions where we can avoid cancellation error problems
            -- See Cody page 10.
         Identity_Test (0.0625, 1.0);
         Identity_Test (15.0, 17.0);
         Identity_Test (1.625, 3.0);
      end Do_Test;
   end Generic_Check;

   -----------------------------------------------------------------------
   -----------------------------------------------------------------------
   package Float_Check is new Generic_Check (Float);

   -- check the floating point type with the most digits
   type A_Long_Float is digits System.Max_Digits;
   package A_Long_Float_Check is new Generic_Check (A_Long_Float);

   -----------------------------------------------------------------------
   -----------------------------------------------------------------------


begin
   Report.Test ("CXG2018",
                "Check the accuracy of the complex EXP function");

   if Verbose then
      Report.Comment ("checking Standard.Float");
   end if;

   Float_Check.Do_Test;

   if Verbose then
      Report.Comment ("checking a digits" &
                      Integer'Image (System.Max_Digits) &
                      " floating point type");
   end if;

   A_Long_Float_Check.Do_Test;


   Report.Result;
end CXG2018;
