-- CXG2014.A
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
--      Check that the SINH and COSH functions return
--      results that are within the error bound allowed.
--
-- TEST DESCRIPTION:
--      This test consists of a generic package that is 
--      instantiated to check both Float and a long float type.
--      The test for each floating point type is divided into
--      several parts:
--         Special value checks where the result is a known constant.
--         Checks that use an identity for determining the result.
--         Exception checks.
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
--      15 Mar 96   SAIC    Initial release for 2.1
--      03 Jun 98   EDS     In line 80, change 1000 to 1024, making it a model
--                          number.  Add Taylor Series terms in line 281.
--      15 Feb 99   RLB     Repaired Subtraction_Error_Test to avoid precision
--                          problems.
--!

--
-- References:
--
-- Software Manual for the Elementary Functions
-- William J. Cody, Jr. and William Waite
-- Prentice-Hall, 1980
--
-- CRC Standard Mathematical Tables
-- 23rd Edition 
--
-- Implementation and Testing of Function Software
-- W. J. Cody
-- Problems and Methodologies in Mathematical Software Production
-- editors P. C. Messina and A. Murli
-- Lecture Notes in Computer Science   Volume 142
-- Springer Verlag, 1982
--

with System;
with Report;
with Ada.Numerics.Generic_Elementary_Functions;
procedure CXG2014 is
   Verbose : constant Boolean := False;
   Max_Samples : constant := 1024;

   E  : constant := Ada.Numerics.E;
   Cosh1 : constant := (E + 1.0 / E) / 2.0;    -- cosh(1.0) 

   generic
      type Real is digits <>;
   package Generic_Check is
      procedure Do_Test;
   end Generic_Check;

   package body Generic_Check is
      package Elementary_Functions is new 
           Ada.Numerics.Generic_Elementary_Functions (Real);
      function Sinh (X : Real) return Real renames
           Elementary_Functions.Sinh;
      function Cosh (X : Real) return Real renames
           Elementary_Functions.Cosh;
      function Log (X : Real) return Real renames
           Elementary_Functions.Log;

      -- flag used to terminate some tests early
      Accuracy_Error_Reported : Boolean := False;


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


      procedure Special_Value_Test is
         -- In the following tests the expected result is accurate
         -- to the machine precision so the minimum guaranteed error
         -- bound can be used.
         Minimum_Error : constant := 8.0;
      begin
         Check (Sinh (1.0),
                (E - 1.0 / E) / 2.0,  
                "sinh(1)",
                Minimum_Error);
         Check (Cosh (1.0),
                Cosh1,
                "cosh(1)",
                Minimum_Error);
         Check (Sinh (2.0),
                (E * E - (1.0 / (E * E))) / 2.0,
                "sinh(2)",
                Minimum_Error);
         Check (Cosh (2.0),
                (E * E + (1.0 / (E * E))) / 2.0,
                "cosh(2)",
                Minimum_Error);
         Check (Sinh (-1.0),
                (1.0 / E - E) / 2.0,  
                "sinh(-1)",
                Minimum_Error);
      exception
         when Constraint_Error => 
            Report.Failed ("Constraint_Error raised in special value test");
         when others =>
            Report.Failed ("exception in special value test");
      end Special_Value_Test;



      procedure Exact_Result_Test is
         No_Error : constant := 0.0;
      begin
         -- A.5.1(38);6.0
         Check (Sinh (0.0),  0.0, "sinh(0)", No_Error);
         Check (Cosh (0.0),  1.0, "cosh(0)", No_Error);
      exception
         when Constraint_Error => 
            Report.Failed ("Constraint_Error raised in Exact_Result Test");
         when others =>
            Report.Failed ("exception in Exact_Result Test");
      end Exact_Result_Test;


      procedure Identity_1_Test is
      -- For the Sinh test use the identity
      --    2 * Sinh(x) * Cosh(1) = Sinh(x+1) + Sinh (x-1)
      -- which is transformed to
      --    Sinh(x) = ((Sinh(x+1) + Sinh(x-1)) * C
      -- where C = 1/(2*Cosh(1))
      --
      -- For the Cosh test use the identity
      --    2 * Cosh(x) * Cosh(1) = Cosh(x+1) + Cosh(x-1)
      -- which is transformed to
      --    Cosh(x) = C * (Cosh(x+1) + Cosh(x-1))
      -- where C is the same as above
      --
      -- see Cody pg 230-231 for details on the error analysis.
      -- The net result is a relative error bound of 16 * Model_Epsilon.

         A : constant := 3.0;
            -- large upper bound but not so large as to cause Cosh(B)
            -- to overflow
         B : constant Real := Log(Real'Safe_Last) - 2.0;
         X_Minus_1, X, X_Plus_1 : Real; 
         Actual1, Actual2 : Real;
         C : constant := 1.0 / (2.0 * Cosh1);
      begin
         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            -- make sure there is no error in x-1, x, and x+1
            X_Plus_1 :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            X_Plus_1  := Real'Machine (X_Plus_1);
            X         := Real'Machine (X_Plus_1 - 1.0);
            X_Minus_1 := Real'Machine (X - 1.0);
           
            -- Sinh(x) = ((Sinh(x+1) + Sinh(x-1)) * C
            Actual1 := Sinh(X);
            Actual2 := C * (Sinh(X_Plus_1) + Sinh(X_Minus_1));
 
            Check (Actual1, Actual2,
                   "Identity_1_Test " & Integer'Image (I) & ": sinh(" &
		   Real'Image (X) & ") ",
                   16.0);

            -- Cosh(x) = C * (Cosh(x+1) + Cosh(x-1))
            Actual1 := Cosh (X);
            Actual2 := C * (Cosh(X_Plus_1) + Cosh (X_Minus_1));
            Check (Actual1, Actual2,
                   "Identity_1_Test " & Integer'Image (I) & ": cosh(" &
		   Real'Image (X) & ") ",
                   16.0);

            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;

         end loop;

      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in Identity_1_Test" &
                " for X=" & Real'Image (X));
         when others =>
            Report.Failed ("exception in Identity_1_Test" &
                " for X=" & Real'Image (X));
      end Identity_1_Test;



      procedure Subtraction_Error_Test is
      -- This test detects the error resulting from subtraction if
      -- the obvious algorithm was used for computing sinh.  That is,
      -- it it is computed as (e**x - e**-x)/2.
      -- We check the result by using a Taylor series expansion that
      -- will produce a result accurate to the machine precision for
      -- the range under test.
      --
      -- The maximum relative error bound for this test is 
      --  8 for the sinh operation and 7 for the Taylor series
      -- for a total of 15 * Model_Epsilon
         A : constant := 0.0;
         B : constant := 0.5;
         X : Real;
         X_Squared : Real;
         Actual, Expected : Real;
      begin
         if Real'digits > 15 then
             return; -- The approximation below is not accurate beyond
                     -- 15 digits. Adding more terms makes the error
                     -- larger, so it makes the test worse for more normal
                     -- values. Thus, we skip this subtest for larger than
                     -- 15 digits.
         end if;
         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            X_Squared := X * X;
           
            Actual := Sinh(X);

            -- The Taylor series regrouped a bit
            Expected := 
               X * (1.0 + (X_Squared / 6.0) *
                          (1.0 + (X_Squared/20.0) *
                                 (1.0 + (X_Squared/42.0) *
                                        (1.0 + (X_Squared/72.0) *
                                               (1.0 + (X_Squared/110.0) *
                                                      (1.0 + (X_Squared/156.0)
                   ))))));
 
            Check (Actual, Expected,
                   "Subtraction_Error_Test " & Integer'Image (I) & ": sinh(" &
		   Real'Image (X) & ") ",
                   15.0);

            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;

         end loop;

      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in Subtraction_Error_Test");
         when others =>
            Report.Failed ("exception in Subtraction_Error_Test");
      end Subtraction_Error_Test;


      procedure Exception_Test is
         X1, X2 : Real := 0.0;
      begin
         -- this part of the test is only applicable if 'Machine_Overflows
         -- is true.
         if Real'Machine_Overflows then

	    begin
	      X1 := Sinh (Real'Safe_Last / 2.0);
	      Report.Failed ("no exception for sinh overflow");
	    exception
	       when Constraint_Error => null;
	       when others =>
	          Report.Failed ("wrong exception sinh overflow");
	    end;

	    begin
	      X2 := Cosh (Real'Safe_Last / 2.0);
	      Report.Failed ("no exception for cosh overflow");
	    exception
	       when Constraint_Error => null;
	       when others =>
	          Report.Failed ("wrong exception cosh overflow");
	    end;

         end if;

         -- optimizer thwarting
         if Report.Ident_Bool (False) then
            Report.Comment (Real'Image (X1 + X2));
         end if;
      end Exception_Test;


      procedure Do_Test is
      begin
         Special_Value_Test;
         Exact_Result_Test;
         Identity_1_Test;
         Subtraction_Error_Test;
         Exception_Test;
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
   Report.Test ("CXG2014",
                "Check the accuracy of the SINH and COSH functions"); 

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
end CXG2014;
