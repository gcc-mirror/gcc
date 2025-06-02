-- CXG2011.A
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
--      Check that the log function returns
--      results that are within the error bound allowed.
--
-- TEST DESCRIPTION:
--      This test consists of a generic package that is 
--      instantiated to check both Float and a long float type.
--      The test for each floating point type is divided into
--      several parts:
--         Special value checks where the result is a known constant.
--         Checks in a range where a Taylor series can be used to compute 
--            the expected result.
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
--       1 Mar 96   SAIC    Initial release for 2.1
--      22 Aug 96   SAIC    Improved Check routine
--      02 DEC 97   EDS     Log (0.0) must raise Constraint_Error, 
--                          not Argument_Error
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
procedure CXG2011 is
   Verbose : constant Boolean := False;
   Max_Samples : constant := 1000;

   -- CRC Handbook Page 738
   Ln10 : constant := 2.30258_50929_94045_68401_79914_54684_36420_76011_01489;
   Ln2  : constant := 0.69314_71805_59945_30941_72321_21458_17656_80755_00134;

   generic
      type Real is digits <>;
   package Generic_Check is
      procedure Do_Test;
   end Generic_Check;

   package body Generic_Check is
      package Elementary_Functions is new 
           Ada.Numerics.Generic_Elementary_Functions (Real);
      function Sqrt (X : Real'Base) return Real'Base renames
           Elementary_Functions.Sqrt;
      function Exp (X : Real'Base) return Real'Base renames
           Elementary_Functions.Exp;
      function Log (X : Real'Base) return Real'Base renames
           Elementary_Functions.Log;
      function Log (X, Base : Real'Base) return Real'Base renames
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
         -- we compute the maximum error as a multiple of Model_Epsilon 
         -- instead of Model_Epsilon and Expected.
         Rel_Error := MRE * abs Expected * Real'Model_Epsilon;
         Abs_Error := MRE * Real'Model_Epsilon;
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
      begin

         --- test 1 ---
         declare
            Y : Real;
         begin
            Y := Log(1.0);
            Check (Y, 0.0, "special value test 1 -- log(1)",
                   0.0);  -- no error allowed
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 1");
            when others =>
               Report.Failed ("exception in test 1");
         end;

         --- test 2 ---
         declare
            Y : Real;
         begin
            Y := Log(10.0);
            Check (Y, Ln10, "special value test 2 -- log(10)", 4.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 2");
            when others =>
               Report.Failed ("exception in test 2");
         end;

         --- test 3 ---
         declare
            Y : Real;
         begin
            Y := Log (2.0);
            Check (Y, Ln2, "special value test 3 -- log(2)", 4.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 3");
            when others =>
               Report.Failed ("exception in test 3");
         end;

         --- test 4 ---
         declare
            Y : Real;
         begin
            Y := Log (2.0 ** 18, 2.0);
            Check (Y, 18.0, "special value test 4 -- log(2**18,2)", 4.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 4");
            when others =>
               Report.Failed ("exception in test 4");
         end;
      end Special_Value_Test;


      procedure Taylor_Series_Test is
      -- Use a 4 term taylor series expansion to check a selection of
      -- arguments very near 1.0.
      -- The range is chosen so that the 4 term taylor series will
      -- provide accuracy to machine precision.   Cody pg 49-50.
         Half_Range : constant Real := Real'Model_Epsilon * 50.0;
         A : constant Real := 1.0 - Half_Range;
         B : constant Real := 1.0 + Half_Range;
         X : Real;
         Xm1 : Real;
         Expected : Real;
         Actual : Real;

      begin
         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            
            Xm1 := X - 1.0;
            -- The following is the first 4 terms of the taylor series
            -- that has been rearranged to minimize error in the calculation
            Expected := (Xm1 * (1.0/3.0 - Xm1/4.0) - 0.5) * Xm1 * Xm1 + Xm1;

            Actual := Log (X);
            Check (Actual, Expected,
                   "Taylor Series Test -" &
                   Integer'Image (I) &
                   " log (" & Real'Image (X) & ")",
                   4.0);
            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in Taylor Series Test");
         when others =>
            Report.Failed ("exception in Taylor Series Test");
      end Taylor_Series_Test;



      procedure Log_Difference_Identity is
      -- Check using the identity ln(x) = ln(17x/16) - ln(17/16)
      -- over the range A to B.
      -- The selected range assures that both X and 17x/16 will
      -- have the same exponents and neither argument gets too close
      -- to 1.    Cody pg 50.
         A : constant Real := 1.0 / Sqrt (2.0);
         B : constant Real := 15.0 / 16.0;
         X : Real;
         Expected : Real;
         Actual : Real;
      begin
         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            -- magic argument purification
            X := Real'Machine (Real'Machine (X+8.0) - 8.0);
            
            Expected := Log (X + X / 16.0) - Log (17.0/16.0);

            Actual := Log (X);
            Check (Actual, Expected,
                   "Log Difference Identity -" &
                   Integer'Image (I) &
                   " log (" & Real'Image (X) & ")",
                   4.0);

            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in Log Difference Identity Test");
         when others =>
            Report.Failed ("exception in Log Difference Identity Test");
      end Log_Difference_Identity;


      procedure Log_Product_Identity is
      -- Check using the identity ln(x**2) = 2ln(x)
      -- over the range A to B.
      -- This large range is chosen to minimize the possibility of
      -- undetected systematic errors.   Cody pg 53.
         A : constant Real := 16.0;
         B : constant Real := 240.0;
         X : Real;
         Expected : Real;
         Actual : Real;
      begin
         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            -- magic argument purification
            X := Real'Machine (Real'Machine (X+8.0) - 8.0);
            
            Expected := 2.0 * Log (X);

            Actual := Log (X*X);
            Check (Actual, Expected,
                   "Log Product Identity -" &
                   Integer'Image (I) &
                   " log (" & Real'Image (X) & ")",
                   4.0);
            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in Log Product Identity Test");
         when others =>
            Report.Failed ("exception in Log Product Identity Test");
      end Log_Product_Identity;


      procedure Log10_Test is
      -- Check using the identity log(x) = log(11x/10) - log(1.1)
      -- over the range A to B.  See Cody pg 52.
         A : constant Real := 1.0 / Sqrt (10.0);
         B : constant Real := 0.9;
         X : Real;
         Expected : Real;
         Actual : Real;
      begin
         if Real'Digits > 17 then
             -- constant used below is accuract to 17 digits
            Error_Low_Bound := 0.00000_00000_00000_01;
            Report.Comment ("log accuracy checked to 19 digits");
         end if;
         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            
            Expected := Log (X + X/10.0, 10.0)
                     - 3.77060_15822_50407_5E-4 - 21.0 / 512.0;

            Actual := Log (X, 10.0);
            Check (Actual, Expected,
                   "Log 10 Test -" &
                   Integer'Image (I) &
                   " log (" & Real'Image (X) & ")",
                   4.0);

              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
            exit when Accuracy_Error_Reported;
         end loop;
         Error_Low_Bound := 0.0;   -- reset

      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in Log 10 Test");
         when others =>
            Report.Failed ("exception in Log 10 Test");
      end Log10_Test;


      procedure Exception_Test is
         X1, X2, X3, X4 : Real;
      begin
         begin
            X1 := Log (0.0);
            Report.Failed ("exception not raised for LOG(0)");
         exception
            -- Log (0.0) must raise Constraint_Error, not Argument_Error,
            -- as per A.5.1(28,29).  Was incorrect in ACVC 2.1 release.
            when Ada.Numerics.Argument_Error =>
               Report.Failed ("Argument_Error raised instead of" &
                              " Constraint_Error for LOG(0)--A.5.1(28,29)");
            when Constraint_Error =>  null;  -- ok
            when others =>
               Report.Failed ("wrong exception raised for LOG(0)");
         end;

         begin
            X2 := Log ( 1.0, 0.0);
            Report.Failed ("exception not raised for LOG(1,0)");
         exception
            when Ada.Numerics.Argument_Error =>  null;  -- ok
            when Constraint_Error => 
               Report.Failed ("constraint_error raised instead of" &
                              " argument_error for LOG(1,0)");
            when others =>
               Report.Failed ("wrong exception raised for LOG(1,0)");
         end;

         begin
            X3 := Log (1.0, 1.0);
            Report.Failed ("exception not raised for LOG(1,1)");
         exception
            when Ada.Numerics.Argument_Error =>  null;  -- ok
            when Constraint_Error => 
               Report.Failed ("constraint_error raised instead of" &
                              " argument_error for LOG(1,1)");
            when others =>
               Report.Failed ("wrong exception raised for LOG(1,1)");
         end;

         begin
            X4 := Log (1.0, -10.0);
            Report.Failed ("exception not raised for LOG(1,-10)");
         exception
            when Ada.Numerics.Argument_Error =>  null;  -- ok
            when Constraint_Error => 
               Report.Failed ("constraint_error raised instead of" &
                              " argument_error for LOG(1,-10)");
            when others =>
               Report.Failed ("wrong exception raised for LOG(1,-10)");
         end;

         -- optimizer thwarting
         if Report.Ident_Bool (False) then
            Report.Comment (Real'Image (X1+X2+X3+X4));
         end if;
      end Exception_Test;


      procedure Do_Test is
      begin
         Special_Value_Test;
         Taylor_Series_Test;
         Log_Difference_Identity;
         Log_Product_Identity;
         Log10_Test;
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
   Report.Test ("CXG2011",
                "Check the accuracy of the log function"); 

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
end CXG2011;
