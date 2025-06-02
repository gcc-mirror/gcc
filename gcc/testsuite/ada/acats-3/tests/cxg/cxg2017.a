-- CXG2017.A
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
--      Check that the TANH function returns
--      a result that is within the error bound allowed.
--
-- TEST DESCRIPTION:
--      This test consists of a generic package that is 
--      instantiated to check both Float and a long float type.
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
--      20 Mar 96   SAIC    Initial release for 2.1
--      17 Aug 96   SAIC    Incorporated reviewer comments.
--      03 Jun 98   EDS     Add parens to remove the potential for overflow.
--                          Remove the invocation of Identity_Test that checks
--                          Tanh values that are too close to zero for the
--                          test's error bounds.
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
procedure CXG2017 is
   Verbose : constant Boolean := False;
   Max_Samples : constant := 1000;

   E  : constant := Ada.Numerics.E;

   generic
      type Real is digits <>;
   package Generic_Check is
      procedure Do_Test;
   end Generic_Check;

   package body Generic_Check is
      package Elementary_Functions is new 
           Ada.Numerics.Generic_Elementary_Functions (Real);

      function Tanh (X : Real) return Real renames
           Elementary_Functions.Tanh;

      function Log (X : Real) return Real renames
           Elementary_Functions.Log;

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


      procedure Special_Value_Test is
         -- In the following tests the expected result is accurate
         -- to the machine precision so the minimum guaranteed error
         -- bound can be used.
         Minimum_Error : constant := 8.0;
         E2 : constant := E * E;
      begin
         Check (Tanh (1.0),
                (E - 1.0 / E) / (E + 1.0 / E),  
                "tanh(1)",
                Minimum_Error);
         Check (Tanh (2.0),
                (E2 - 1.0 / E2) / (E2 + 1.0 / E2),  
                "tanh(2)",
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
         Check (Tanh (0.0),  0.0, "tanh(0)", No_Error);
      exception
         when Constraint_Error => 
            Report.Failed ("Constraint_Error raised in Exact_Result Test");
         when others =>
            Report.Failed ("exception in Exact_Result Test");
      end Exact_Result_Test;


      procedure Identity_Test (A, B : Real) is
      -- For this test we use the identity
      --    TANH(u+v) = [TANH(u) + TANH(v)] / [1 + TANH(u)*TANH(v)]
      -- which is transformed to
      --    TANH(x) = [TANH(y)+C] / [1 + TANH(y) * C]
      -- where C = TANH(1/8) and y = x - 1/8
      --
      -- see Cody pg 248-249 for details on the error analysis.
      -- The net result is a relative error bound of 16 * Model_Epsilon.
      --
      -- The second part of this test checks the identity
      --    TANH(-x) = -TANH(X)

         X, Y : Real; 
         Actual1, Actual2 : Real;
         C : constant := 1.2435300177159620805e-1;
      begin
         if Real'Digits > 20 then
            -- constant C is accurate to 20 digits.  Set the low bound
            -- on the error to 16*10**-20
            Error_Low_Bound := 0.00000_00000_00000_00016;
            Report.Comment ("tanh accuracy checked to 20 digits");
         end if;

         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            X :=  (B - A) * (Real (I) / Real (Max_Samples)) + A;
            Actual1 := Tanh(X);
           
            -- TANH(x) = [TANH(y)+C] / [1 + TANH(y) * C]
            Y := X - (1.0 / 8.0);
            Actual2 := (Tanh (Y) + C) / (1.0 + Tanh(Y) * C);
 
            Check (Actual1, Actual2,
                   "Identity_1_Test " & Integer'Image (I) & ": tanh(" &
		   Real'Image (X) & ") ",
                   16.0);

            -- TANH(-x) = -TANH(X)
            Actual2 := Tanh(-X);
            Check (-Actual1, Actual2,
                   "Identity_2_Test " & Integer'Image (I) & ": tanh(" &
		   Real'Image (X) & ") ",
                   16.0);

            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;

         end loop;
         Error_Low_Bound := 0.0;   -- reset
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in Identity_Test" &
                " for X=" & Real'Image (X));
         when others =>
            Report.Failed ("exception in Identity_Test" &
                " for X=" & Real'Image (X));
      end Identity_Test;



      procedure Do_Test is
      begin
         Special_Value_Test;
         Exact_Result_Test;
            -- cover a large range
         Identity_Test (1.0, Real'Safe_Last);
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
   Report.Test ("CXG2017",
                "Check the accuracy of the TANH function"); 

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
end CXG2017;
