-- CXG2013.A
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
--      Check that the TAN and COT functions return
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
--      11 Mar 96   SAIC    Initial release for 2.1
--      17 Aug 96   SAIC    Commentary fixes.
--      03 Feb 97   PWB.CTA Removed checks with explicit Cycle => 2.0*Pi
--      02 DEC 97   EDS     Change Max_Samples constant to 1001.
--      29 JUN 98   EDS     Deleted Special_Angle_Test as fatally flawed.

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
procedure CXG2013 is
   Verbose : constant Boolean := False;
   Max_Samples : constant := 1001;

   -- CRC Standard Mathematical Tables;  23rd Edition; pg 738
   Sqrt2 : constant := 
        1.41421_35623_73095_04880_16887_24209_69807_85696_71875_37695;
   Sqrt3 : constant :=
        1.73205_08075_68877_29352_74463_41505_87236_69428_05253_81039;
   
   Pi : constant := Ada.Numerics.Pi;

   generic
      type Real is digits <>;
   package Generic_Check is
      procedure Do_Test;
   end Generic_Check;

   package body Generic_Check is
      package Elementary_Functions is new 
           Ada.Numerics.Generic_Elementary_Functions (Real);
      function Sqrt (X : Real) return Real renames
           Elementary_Functions.Sqrt;
      function Tan (X : Real) return Real renames
           Elementary_Functions.Tan;
      function Cot (X : Real) return Real renames
           Elementary_Functions.Cot;
      function Tan (X, Cycle : Real) return Real renames
           Elementary_Functions.Tan;
      function Cot (X, Cycle : Real) return Real renames
           Elementary_Functions.Cot;

      -- flag used to terminate some tests early
      Accuracy_Error_Reported : Boolean := False;

      -- factor to be applied in computing MRE
      Maximum_Relative_Error : constant Real := 4.0;

      procedure Check (Actual, Expected : Real;
                       Test_Name : String;
                       MRE : Real) is
         Max_Error : Real;
         Rel_Error : Real;
         Abs_Error : Real;
      begin
         -- In the case where the expected result is very small or 0
         -- we compute the maximum error as a multiple of Model_Epsilon instead
         -- of Model_Epsilon and Expected.
         Rel_Error := MRE * abs Expected * Real'Model_Epsilon;
         Abs_Error := MRE * Real'Model_Epsilon;
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



      procedure Exact_Result_Test is
         No_Error : constant := 0.0;
      begin
         -- A.5.1(38);6.0
         Check (Tan (0.0),  0.0, "tan(0)", No_Error);

         -- A.5.1(41);6.0
         Check (Tan (180.0, 360.0), 0.0, "tan(180,360)", No_Error);
         Check (Tan (360.0, 360.0), 0.0, "tan(360,360)", No_Error);
         Check (Tan (720.0, 360.0), 0.0, "tan(720,360)", No_Error);

         -- A.5.1(41);6.0
         Check (Cot ( 90.0, 360.0), 0.0, "cot( 90,360)", No_Error);
         Check (Cot (270.0, 360.0), 0.0, "cot(270,360)", No_Error);
         Check (Cot (810.0, 360.0), 0.0, "cot(810,360)", No_Error);

      exception
         when Constraint_Error => 
            Report.Failed ("Constraint_Error raised in Exact_Result Test");
         when others =>
            Report.Failed ("exception in Exact_Result Test");
      end Exact_Result_Test;


      procedure Tan_Test (A, B : Real) is
      -- Use identity Tan(X) = [2*Tan(x/2)]/[1-Tan(x/2) ** 2]
      -- checks over the range -pi/4 .. pi/4 require no argument reduction
      -- checks over the range 7pi/8 .. 9pi/8 require argument reduction
         X, Y : Real;
         Actual1, Actual2 : Real;
      begin
         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            -- argument purification to insure x and x/2 are exact
            -- See Cody page 170.
            Y := Real'Machine (X*0.5);
            X := Real'Machine (Y + Y);
           
            Actual1 := Tan(X);
            Actual2 := (2.0 * Tan (Y)) / (1.0 - Tan (Y) ** 2);
 
            if abs (X - Pi) > ( (B-A)/Real(2*Max_Samples) ) then
              Check (Actual1, Actual2,
                     "Tan_Test " & Integer'Image (I) & ": tan(" &
                     Real'Image (X) & ") ",
                     (1.0 + Sqrt2) * Maximum_Relative_Error);
                     -- see Cody pg 165 for error bound info
            end if;

            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;

         end loop;

      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in Tan_Test");
         when others =>
            Report.Failed ("exception in Tan_Test");
      end Tan_Test;



      procedure Cot_Test is
      -- Use identity Cot(X) = [Cot(X/2)**2 - 1]/[2*Cot(X/2)]
         A : constant := 6.0 * Pi;
         B : constant := 25.0 / 4.0 * Pi;
         X, Y : Real;
         Actual1, Actual2 : Real;
      begin
         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            -- argument purification to insure x and x/2 are exact.
            -- See Cody page 170.
            Y := Real'Machine (X*0.5);
            X := Real'Machine (Y + Y);
           
            Actual1 := Cot(X);
            Actual2 := (Cot (Y) ** 2 - 1.0) / (2.0 * Cot (Y));
 
            Check (Actual1, Actual2,
                   "Cot_Test " & Integer'Image (I) & ": cot(" &
                   Real'Image (X) & ") ",
                   (1.0 + Sqrt2) * Maximum_Relative_Error);
                   -- see Cody pg 165 for error bound info

            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;

         end loop;

      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in Cot_Test");
         when others =>
            Report.Failed ("exception in Cot_Test");
      end Cot_Test;


      procedure Exception_Test is
         X1, X2, X3, X4, X5 : Real := 0.0;
      begin


         begin  -- A.5.1(20);6.0
           X1 := Tan (0.0, Cycle => 0.0);
           Report.Failed ("no exception for cycle = 0.0");
         exception
            when Ada.Numerics.Argument_Error => null;
            when others =>
               Report.Failed ("wrong exception for cycle = 0.0");
         end;

         begin  -- A.5.1(20);6.0
           X2 := Cot (1.0, Cycle => -3.0);
           Report.Failed ("no exception for cycle < 0.0");
         exception
            when Ada.Numerics.Argument_Error => null;
            when others =>
               Report.Failed ("wrong exception for cycle < 0.0");
         end;

         -- the remaining tests only apply to machines that overflow
         if Real'Machine_Overflows then    -- A.5.1(28);6.0

            begin   -- A.5.1(29);6.0
               X3 := Cot (0.0);
               Report.Failed ("exception not raised for cot(0)");
            exception
               when Constraint_Error => null;   -- ok
               when others =>
                  Report.Failed ("wrong exception raised for cot(0)");
            end;
   
            begin   -- A.5.1(31);6.0
               X4 := Tan (90.0, 360.0);
               Report.Failed ("exception not raised for tan(90,360)");
            exception
               when Constraint_Error => null;  -- ok
               when others =>
                  Report.Failed ("wrong exception raised for tan(90,360)");
            end;

            begin   -- A.5.1(32);6.0
               X5 := Cot (180.0, 360.0);
               Report.Failed ("exception not raised for cot(180,360)");
            exception
               when Constraint_Error => null;  -- ok
               when others =>
                  Report.Failed ("wrong exception raised for cot(180,360)");
            end;
         end if;

         -- optimizer thwarting
         if Report.Ident_Bool (False) then
            Report.Comment (Real'Image (X1+X2+X3+X4+X5));
         end if;
      end Exception_Test;


      procedure Do_Test is
      begin
         Exact_Result_Test;
         Tan_Test (-Pi/4.0, Pi/4.0);
         Tan_Test (7.0*Pi/8.0, 9.0*Pi/8.0);
         Cot_Test;
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
   Report.Test ("CXG2013",
                "Check the accuracy of the TAN and COT functions"); 

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
end CXG2013;
