-- CXG2016.A
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
--      Check that the ARCTAN function returns a
--      result that is within the error bound allowed.
--
-- TEST DESCRIPTION:
--      This test consists of a generic package that is
--      instantiated to check both Float and a long float type.
--      The test for each floating point type is divided into
--      several parts:
--         Special value checks where the result is a known constant.
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
--      19 Mar 96   SAIC    Initial release for 2.1
--      30 APR 96   SAIC    Fixed optimization issue
--      17 AUG 96   SAIC    Incorporated Reviewer's suggestions.
--      12 OCT 96   SAIC    Incorporated Reviewer's suggestions.
--      02 DEC 97   EDS     Remove procedure Identity_1_Test and calls to
--                          procedure.
--      29 JUN 98   EDS     Replace -0.0 with call to ImpDef.Annex_G.Negative_Zero
--      28 APR 99   RLB     Replaced comma accidentally deleted in above change.
--      15 DEC 99   RLB     Added model range checking to "exact" results,
--                          in order to avoid too strictly requiring a specific
--                          result.
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
with Impdef.Annex_G;
procedure CXG2016 is
   Verbose : constant Boolean := False;
   Max_Samples : constant := 1000;

   -- CRC Standard Mathematical Tables;  23rd Edition; pg 738
   Sqrt2 : constant :=
        1.41421_35623_73095_04880_16887_24209_69807_85696_71875_37695;
   Sqrt3 : constant :=
        1.73205_08075_68877_29352_74463_41505_87236_69428_05253_81039;

   Pi : constant := Ada.Numerics.Pi;

   generic
      type Real is digits <>;
      Half_PI_Low : in Real; -- The machine number closest to, but not greater
                             -- than PI/2.0.
      Half_PI_High : in Real;-- The machine number closest to, but not less
                             -- than PI/2.0.
      PI_Low : in Real;      -- The machine number closest to, but not greater
                             -- than PI.
      PI_High : in Real;     -- The machine number closest to, but not less
                             -- than PI.
   package Generic_Check is
      procedure Do_Test;
   end Generic_Check;

   package body Generic_Check is
      package Elementary_Functions is new
           Ada.Numerics.Generic_Elementary_Functions (Real);

      function Arctan (Y : Real;
                       X : Real := 1.0) return Real renames
           Elementary_Functions.Arctan;
      function Arctan (Y : Real;
                       X : Real := 1.0;
                       Cycle : Real) return Real renames
           Elementary_Functions.Arctan;

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
      -- If eta is very small, arctan(x + eta) ~= arctan(x) + eta/(1+x*x).
      --
      -- For tests 4 and 5, there is an error of 4.0ME for arctan + an
      -- additional error of 1.0ME because pi is not exact for a total of 5.0ME.
      --
      -- In test 3 there is the error for pi plus an additional error
      -- of (1.0ME)/4 since sqrt3 is not exact, for a total of 5.25ME.
      --
      -- In test 2 there is the error for pi plus an additional error
      -- of (3/4)(1.0ME) since sqrt3 is not exact, for a total of 5.75ME.


         type Data_Point is
            record
               Degrees,
               Radians,
               Tangent,
               Allowed_Error : Real;
            end record;

         type Test_Data_Type is array (Positive range <>) of Data_Point;

         -- the values in the following table only involve static
         -- expressions so no additional loss of precision occurs.
         Test_Data : constant Test_Data_Type := (
         --  degrees      radians       tangent   error     test #
            (  0.0,           0.0,          0.0,   4.0 ),    -- 1
            ( 30.0,        Pi/6.0,    Sqrt3/3.0,   5.75),    -- 2
            ( 60.0,        Pi/3.0,        Sqrt3,   5.25),    -- 3
            ( 45.0,        Pi/4.0,          1.0,   5.0 ),    -- 4
            (-45.0,       -Pi/4.0,         -1.0,   5.0 ) );  -- 5

      begin
         for I in Test_Data'Range loop
            Check (Arctan (Test_Data (I).Tangent),
                   Test_Data (I).Radians,
                   "special value test" & Integer'Image (I) &
                      " arctan(" &
                      Real'Image (Test_Data (I).Tangent) &
                      ")",
                   Test_Data (I).Allowed_Error);
            Check (Arctan (Test_Data (I).Tangent, Cycle => 360.0),
                   Test_Data (I).Degrees,
                   "special value test" & Integer'Image (I) &
                      " arctan(" &
                      Real'Image (Test_Data (I).Tangent) &
                      ", cycle=>360)",
                   Test_Data (I).Allowed_Error);
         end loop;

      exception
         when Constraint_Error =>
            Report.Failed ("Constraint_Error raised in special value test");
         when others =>
            Report.Failed ("exception in special value test");
      end Special_Value_Test;



      procedure Check_Exact (Actual, Expected_Low, Expected_High : Real;
	                     Test_Name : String) is
         -- If the expected result is not a model number, then Expected_Low is
         -- the first machine number less than the (exact) expected
         -- result, and Expected_High is the first machine number greater than
         -- the (exact) expected result. If the expected result is a model
         -- number, Expected_Low = Expected_High = the result.
         Model_Expected_Low  : Real := Expected_Low;
         Model_Expected_High : Real := Expected_High;
      begin
         -- Calculate the first model number nearest to, but below (or equal)
         -- to the expected result:
         while Real'Model (Model_Expected_Low) /= Model_Expected_Low loop
            -- Try the next machine number lower:
            Model_Expected_Low := Real'Adjacent(Model_Expected_Low, 0.0);
         end loop;
         -- Calculate the first model number nearest to, but above (or equal)
         -- to the expected result:
         while Real'Model (Model_Expected_High) /= Model_Expected_High loop
            -- Try the next machine number higher:
            Model_Expected_High := Real'Adjacent(Model_Expected_High, 100.0);
         end loop;

         if Actual < Model_Expected_Low or Actual > Model_Expected_High then
            Accuracy_Error_Reported := True;
            if Actual < Model_Expected_Low then
               Report.Failed (Test_Name &
                              " actual: " & Real'Image (Actual) &
                              " expected low: " & Real'Image (Model_Expected_Low) &
                              " expected high: " & Real'Image (Model_Expected_High) &
                              " difference: " & Real'Image (Actual - Expected_Low));
            else
               Report.Failed (Test_Name &
                              " actual: " & Real'Image (Actual) &
                              " expected low: " & Real'Image (Model_Expected_Low) &
                              " expected high: " & Real'Image (Model_Expected_High) &
                              " difference: " & Real'Image (Expected_High - Actual));
            end if;
         elsif Verbose then
            Report.Comment (Test_Name & "  passed");
         end if;
      end Check_Exact;


      procedure Exact_Result_Test is
      begin
         --  A.5.1(40);6.0
         Check_Exact (Arctan (0.0, 1.0),       0.0, 0.0, "arctan(0,1)");
         Check_Exact (Arctan (0.0, 1.0, 27.0), 0.0, 0.0, "arctan(0,1,27)");

         --  G.2.4(11-13);6.0

         Check_Exact (Arctan (1.0, 0.0), Half_PI_Low, Half_PI_High,
              "arctan(1,0)");
         Check_Exact (Arctan (1.0, 0.0, 360.0), 90.0, 90.0, "arctan(1,0,360)");

         Check_Exact (Arctan (-1.0, 0.0), -Half_PI_High, -Half_PI_Low,
              "arctan(-1,0)");
         Check_Exact (Arctan (-1.0, 0.0, 360.0), -90.0, -90.0,
              "arctan(-1,0,360)");

         if Real'Signed_Zeros then
            Check_Exact (Arctan (0.0, -1.0), PI_Low, PI_High, "arctan(+0,-1)");
            Check_Exact (Arctan (0.0, -1.0, 360.0), 180.0, 180.0,
                  "arctan(+0,-1,360)");
            Check_Exact (Arctan ( Real ( ImpDef.Annex_G.Negative_Zero ), -1.0),
                   -PI_High, -PI_Low, "arctan(-0,-1)");
            Check_Exact (Arctan ( Real ( ImpDef.Annex_G.Negative_Zero ), -1.0,
                   360.0), -180.0, -180.0, "arctan(-0,-1,360)");
         else
            Check_Exact (Arctan (0.0, -1.0), PI_Low, PI_High, "arctan(0,-1)");
            Check_Exact (Arctan (0.0, -1.0, 360.0), 180.0, 180.0,
                   "arctan(0,-1,360)");
         end if;
      exception
         when Constraint_Error =>
            Report.Failed ("Constraint_Error raised in Exact_Result Test");
         when others =>
            Report.Failed ("Exception in Exact_Result Test");
      end Exact_Result_Test;


      procedure Taylor_Series_Test is
      -- This test checks the Arctan by using a taylor series expansion that
      -- will produce a result accurate to 19 decimal digits for
      -- the range under test.
      --
      -- The maximum relative error bound for this test is
      --  4 for the arctan operation and 2 for the Taylor series
      -- for a total of 6 * Model_Epsilon

         A : constant := -1.0/16.0;
         B : constant :=  1.0/16.0;
         X : Real;
         Actual, Expected : Real;
         Sum, Em, X_Squared : Real;
      begin
         if Real'Digits > 19 then
            -- Taylor series calculation produces result accurate to 19
            -- digits.  If type being tested has more digits then set
            -- the error low bound to account for this.
            -- The error low bound is conservatively set to 6*10**-19
            Error_Low_Bound := 0.00000_00000_00000_0006;
            Report.Comment ("arctan accuracy checked to 19 digits");
         end if;

         Accuracy_Error_Reported := False;  -- reset
         for I in 0..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            X_Squared := X * X;
            Em := 17.0;
            Sum := X_Squared / Em;

            for II in 1 .. 7 loop
               Em := Em - 2.0;
               Sum := (1.0 / Em - Sum) * X_Squared;
            end loop;
            Sum := -X * Sum;
            Expected := X + Sum;
            Sum := (X - Expected) + Sum;
            if not Real'Machine_Rounds then
               Expected := Expected + (Sum + Sum);
            end if;

            Actual := Arctan (X);

            Check (Actual, Expected,
                   "Taylor_Series_Test " & Integer'Image (I) & ": arctan(" &
                   Real'Image (X) & ") ",
                   6.0);

            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;

         end loop;
         Error_Low_Bound := 0.0;  -- reset
      exception
         when Constraint_Error =>
            Report.Failed
               ("Constraint_Error raised in Taylor_Series_Test");
         when others =>
            Report.Failed ("exception in Taylor_Series_Test");
      end Taylor_Series_Test;


      procedure Exception_Test is
         X1, X2, X3 : Real := 0.0;
      begin

         begin  -- A.5.1(20);6.0
           X1 := Arctan(0.0, Cycle => 0.0);
           Report.Failed ("no exception for cycle = 0.0");
         exception
            when Ada.Numerics.Argument_Error => null;
            when others =>
               Report.Failed ("wrong exception for cycle = 0.0");
         end;

         begin  -- A.5.1(20);6.0
           X2 := Arctan (0.0, Cycle => -1.0);
           Report.Failed ("no exception for cycle < 0.0");
         exception
            when Ada.Numerics.Argument_Error => null;
            when others =>
               Report.Failed ("wrong exception for cycle < 0.0");
         end;

         begin  -- A.5.1(25);6.0
           X3 := Arctan (0.0, 0.0);
           Report.Failed ("no exception for arctan(0,0)");
         exception
            when Ada.Numerics.Argument_Error => null;
            when others =>
               Report.Failed ("wrong exception for arctan(0,0)");
         end;

         -- optimizer thwarting
         if Report.Ident_Bool (False) then
            Report.Comment (Real'Image (X1 + X2 + X3));
         end if;
      end Exception_Test;


      procedure Do_Test is
      begin
         Special_Value_Test;
         Exact_Result_Test;
         Taylor_Series_Test;
         Exception_Test;
      end Do_Test;
   end Generic_Check;

   -----------------------------------------------------------------------
   -----------------------------------------------------------------------
   -- These expressions must be truly static, which is why we have to do them
   -- outside of the generic, and we use the named numbers. Note that we know
   -- that PI is not a machine number (it is irrational), and it should be
   -- represented to more digits than supported by the target machine.
   Float_Half_PI_Low  : constant := Float'Adjacent(PI/2.0,  0.0);
   Float_Half_PI_High : constant := Float'Adjacent(PI/2.0, 10.0);
   Float_PI_Low       : constant := Float'Adjacent(PI,      0.0);
   Float_PI_High      : constant := Float'Adjacent(PI,     10.0);
   package Float_Check is new Generic_Check (Float,
	Half_PI_Low  => Float_Half_PI_Low,
	Half_PI_High => Float_Half_PI_High,
	PI_Low  => Float_PI_Low,
	PI_High => Float_PI_High);

   -- check the Floating point type with the most digits
   type A_Long_Float is digits System.Max_Digits;
   A_Long_Float_Half_PI_Low  : constant := A_Long_Float'Adjacent(PI/2.0,  0.0);
   A_Long_Float_Half_PI_High : constant := A_Long_Float'Adjacent(PI/2.0, 10.0);
   A_Long_Float_PI_Low       : constant := A_Long_Float'Adjacent(PI,      0.0);
   A_Long_Float_PI_High      : constant := A_Long_Float'Adjacent(PI,     10.0);
   package A_Long_Float_Check is new Generic_Check (A_Long_Float,
	Half_PI_Low  => A_Long_Float_Half_PI_Low,
	Half_PI_High => A_Long_Float_Half_PI_High,
	PI_Low  => A_Long_Float_PI_Low,
	PI_High => A_Long_Float_PI_High);

   -----------------------------------------------------------------------
   -----------------------------------------------------------------------


begin
   Report.Test ("CXG2016",
                "Check the accuracy of the ARCTAN function");

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
end CXG2016;
