-- CXG2004.A
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
--      Check that the sin and cos functions return
--      results that are within the error bound allowed.
--
-- TEST DESCRIPTION:
--      This test consists of a generic package that is 
--      instantiated to check both float and a long float type.
--      The test for each floating point type is divided into
--      the following parts:
--         Special value checks where the result is a known constant.
--         Checks using an identity relationship.
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
--      13 FEB 96   SAIC    Initial release for 2.1
--      22 APR 96   SAIC    Changed to generic implementation.
--      18 AUG 96   SAIC    Improvements to commentary.
--      23 OCT 96   SAIC    Exact results are not required unless the
--                          cycle is specified.  
--      28 FEB 97   PWB.CTA Removed checks where cycle 2.0*Pi is specified
--      02 JUN 98   EDS     Revised calculations to ensure that X is exactly
--                          three times Y per advice of numerics experts.
--
-- CHANGE NOTE:
--      According to Ken Dritz, author of the Numerics Annex of the RM,
--      one should never specify the cycle 2.0*Pi for the trigonometric
--      functions.  In particular, if the machine number for the first
--      argument is not an exact multiple of the machine number for the
--      explicit cycle, then the specified exact results cannot be
--      reasonably expected.  The affected checks in this test have been
--      marked as comments, with the additional notation "pwb-math".
--      Phil Brashear
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
-- The sin and cos checks are translated directly from 
-- the netlib FORTRAN code that was written by W. Cody.
--

with System;
with Report;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Elementary_Functions;
procedure CXG2004 is
   Verbose : constant Boolean := False;
   Number_Samples : constant := 1000;

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

      function Sin (X : Real) return Real renames
           Elementary_Functions.Sin;
      function Cos (X : Real) return Real renames
           Elementary_Functions.Cos;
      function Sin (X, Cycle : Real) return Real renames
           Elementary_Functions.Sin;
      function Cos (X, Cycle : Real) return Real renames
           Elementary_Functions.Cos;

      Accuracy_Error_Reported : Boolean := False;

      procedure Check (Actual, Expected : Real;
                       Test_Name : String;
                       MRE : Real) is
         Rel_Error,
         Abs_Error,
         Max_Error : Real;
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


         -- in addition to the relative error checks we apply the
         -- criteria of G.2.4(16)
         if abs (Actual) > 1.0 then
            Accuracy_Error_Reported := True;
            Report.Failed (Test_Name & " result > 1.0");
         elsif abs (Actual - Expected) > Max_Error then
            Accuracy_Error_Reported := True;
            Report.Failed (Test_Name & 
                           " actual: " & Real'Image (Actual) &
                           " expected: " & Real'Image (Expected) &
                           " difference: " &
                           Real'Image (Actual - Expected) &
                           " mre:" & 
                           Real'Image (Max_Error) );
         elsif Verbose then
	    if Actual = Expected then
	       Report.Comment (Test_Name & "  exact result");
	    else
	       Report.Comment (Test_Name & "  passed");
	    end if;
         end if;
      end Check;


      procedure Sin_Check (A, B : Real;
                           Arg_Range : String) is
         -- test a selection of 
         -- arguments selected from the range A to B. 
         --
         -- This test uses the identity
	 --   sin(x) = sin(x/3)*(3 - 4 * sin(x/3)**2)
	 --
         -- Note that in this test we must take into account the
         -- error in the calculation of the expected result so
         -- the maximum relative error is larger than the
         -- accuracy required by the ARM.

         X, Y, ZZ : Real;
	 Actual, Expected : Real;
	 MRE : Real;
	 Ran : Real;
      begin
         Accuracy_Error_Reported := False;  -- reset
         for I in 1 .. Number_Samples loop
	    -- Evenly distributed selection of arguments
	    Ran := Real (I) / Real (Number_Samples);

	    -- make sure x and x/3 are both exactly representable
	    -- on the machine.  See "Implementation and Testing of
            -- Function Software" page 44.
            X := (B - A) * Ran + A;
            Y := Real'Leading_Part 
                      ( X/3.0, 
                        Real'Machine_Mantissa - Real'Exponent (3.0) );
            X := Y * 3.0;

	    Actual := Sin (X);

	    ZZ := Sin(Y);
	    Expected := ZZ * (3.0 - 4.0 * ZZ * ZZ);

            -- note that since the expected value is computed, we
            -- must take the error in that computation into account.
            -- See Cody pp 139-141.
	    MRE := 4.0;

            Check (Actual, Expected,
                   "sin test of range" & Arg_Range &
                      Integer'Image (I),
                   MRE);
            exit when Accuracy_Error_Reported;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in sin check");
         when others =>
            Report.Failed ("exception in sin check");
      end Sin_Check;



      procedure Cos_Check (A, B : Real;
                                  Arg_Range : String) is
         -- test a selection of 
         -- arguments selected from the range A to B. 
         --
         -- This test uses the identity
	 --   cos(x) = cos(x/3)*(4 * cos(x/3)**2 - 3)
	 --
         -- Note that in this test we must take into account the
         -- error in the calculation of the expected result so
         -- the maximum relative error is larger than the
         -- accuracy required by the ARM.

         X, Y, ZZ : Real;
	 Actual, Expected : Real;
	 MRE : Real;
	 Ran : Real;
      begin
         Accuracy_Error_Reported := False;  -- reset
         for I in 1 .. Number_Samples loop
	    -- Evenly distributed selection of arguments
	    Ran := Real (I) / Real (Number_Samples);

	    -- make sure x and x/3 are both exactly representable
	    -- on the machine.  See "Implementation and Testing of
            -- Function Software" page 44.
            X := (B - A) * Ran + A;
            Y := Real'Leading_Part 
                      ( X/3.0, 
                        Real'Machine_Mantissa - Real'Exponent (3.0) );
            X := Y * 3.0;

	    Actual := Cos (X);

	    ZZ := Cos(Y);
	    Expected := ZZ * (4.0 * ZZ * ZZ - 3.0);

            -- note that since the expected value is computed, we
            -- must take the error in that computation into account.
            -- See Cody pp 141-143.
	    MRE := 6.0;

            Check (Actual, Expected,
                   "cos test of range" & Arg_Range &
                      Integer'Image (I),
                   MRE);
            exit when Accuracy_Error_Reported;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in cos check");
         when others =>
            Report.Failed ("exception in cos check");
      end Cos_Check;


      procedure Special_Angle_Checks is
         type Data_Point is 
            record
               Degrees,
               Radians,
               Sine,
               Cosine         : Real;
               Sin_Result_Error,
               Cos_Result_Error : Boolean;
            end record;

         type Test_Data_Type is array (Positive range <>) of Data_Point;

         -- the values in the following table only involve static
         -- expressions to minimize any loss of precision.  However,
         -- there are two sources of error that must be accounted for
         -- in the following tests.
         -- First, when a cycle is not specified there can be a roundoff
         -- error in the value of Pi used.  This error does not apply
         -- when a cycle of 2.0 * Pi is explicitly provided.
         -- Second, the expected results that involve sqrt values also
         -- have a potential roundoff error.
         -- The amount of error due to error in the argument is computed
         -- as follows:
         --   sin(x+err) = sin(x)*cos(err) + cos(x)*sin(err)
         --              ~= sin(x) + err * cos(x)
         -- similarly for cos the error due to error in the argument is
         -- computed as follows:
         --   cos(x+err) = cos(x)*cos(err) - sin(x)*sin(err)
         --              ~= cos(x) - err * sin(x)
         -- In both cases the term "err" is bounded by 0.5 * argument.

         Test_Data : constant Test_Data_Type := (
--  degrees      radians          sine       cosine  sin_er cos_er    test #
  (  0.0,           0.0,          0.0,         1.0,  False, False ),    -- 1
  ( 30.0,        Pi/6.0,          0.5,   Sqrt3/2.0,  False, True  ),    -- 2
  ( 60.0,        Pi/3.0,    Sqrt3/2.0,         0.5,  True,  False ),    -- 3
  ( 90.0,        Pi/2.0,          1.0,         0.0,  False, False ),    -- 4
  (120.0,    2.0*Pi/3.0,    Sqrt3/2.0,        -0.5,  True,  False ),    -- 5
  (150.0,    5.0*Pi/6.0,          0.5,  -Sqrt3/2.0,  False, True  ),    -- 6
  (180.0,            Pi,          0.0,        -1.0,  False, False ),    -- 7
  (210.0,    7.0*Pi/6.0,         -0.5,  -Sqrt3/2.0,  False, True  ),    -- 8
  (240.0,    8.0*Pi/6.0,   -Sqrt3/2.0,        -0.5,  True,  False ),    -- 9
  (270.0,    9.0*Pi/6.0,         -1.0,         0.0,  False, False ),    -- 10
  (300.0,   10.0*Pi/6.0,   -Sqrt3/2.0,         0.5,  True,  False ),    -- 11
  (330.0,   11.0*Pi/6.0,         -0.5,   Sqrt3/2.0,  False, True  ),    -- 12
  (360.0,        2.0*Pi,          0.0,         1.0,  False, False ),    -- 13
  ( 45.0,        Pi/4.0,    Sqrt2/2.0,   Sqrt2/2.0,  True,  True  ),    -- 14
  (135.0,    3.0*Pi/4.0,    Sqrt2/2.0,  -Sqrt2/2.0,  True,  True  ),    -- 15
  (225.0,    5.0*Pi/4.0,   -Sqrt2/2.0,  -Sqrt2/2.0,  True,  True  ),    -- 16
  (315.0,    7.0*Pi/4.0,   -Sqrt2/2.0,   Sqrt2/2.0,  True,  True  ),    -- 17
  (405.0,    9.0*Pi/4.0,    Sqrt2/2.0,   Sqrt2/2.0,  True,  True  ) );  -- 18
            
     
         Y : Real;
         Sin_Arg_Err,
         Cos_Arg_Err,
         Sin_Result_Err,
         Cos_Result_Err  : Real;
      begin
         for I in Test_Data'Range loop
            -- compute error components
            Sin_Arg_Err := abs Test_Data (I).Cosine * 
                           abs Test_Data (I).Radians / 2.0;
            Cos_Arg_Err := abs Test_Data (I).Sine * 
                           abs Test_Data (I).Radians / 2.0;

            if Test_Data (I).Sin_Result_Error then
               Sin_Result_Err := 0.5;
            else 
               Sin_Result_Err := 0.0;
            end if;

            if Test_Data (I).Cos_Result_Error then
               Cos_Result_Err := 1.0;
            else 
               Cos_Result_Err := 0.0;
            end if;



            Y := Sin (Test_Data (I).Radians);
            Check (Y, Test_Data (I).Sine,
                   "test" & Integer'Image (I) & " sin(r)",
                   2.0 + Sin_Arg_Err + Sin_Result_Err); 
            Y := Cos (Test_Data (I).Radians);
            Check (Y, Test_Data (I).Cosine,
                   "test" & Integer'Image (I) & " cos(r)",
                   2.0 + Cos_Arg_Err + Cos_Result_Err); 
            Y := Sin (Test_Data (I).Degrees, 360.0);
            Check (Y, Test_Data (I).Sine,
                   "test" & Integer'Image (I) & " sin(d,360)",
                   2.0 + Sin_Result_Err); 
            Y := Cos (Test_Data (I).Degrees, 360.0);
            Check (Y, Test_Data (I).Cosine,
                   "test" & Integer'Image (I) & " cos(d,360)",
                   2.0 + Cos_Result_Err); 
--pwb-math            Y := Sin (Test_Data (I).Radians, 2.0*Pi);
--pwb-math            Check (Y, Test_Data (I).Sine,
--pwb-math                   "test" & Integer'Image (I) & " sin(r,2pi)",
--pwb-math                   2.0 + Sin_Result_Err); 
--pwb-math            Y := Cos (Test_Data (I).Radians, 2.0*Pi);
--pwb-math            Check (Y, Test_Data (I).Cosine,
--pwb-math                   "test" & Integer'Image (I) & " cos(r,2pi)",
--pwb-math                   2.0 + Cos_Result_Err); 
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed ("Constraint_Error raised in special angle test");
         when others =>
            Report.Failed ("exception in special angle test");
      end Special_Angle_Checks;


      -- check the rule of A.5.1(41);6.0 which requires that the
      -- result be exact if the mathematical result is 0.0, 1.0,
      -- or -1.0
      procedure Exact_Result_Checks is
         type Data_Point is 
            record
               Degrees,
               Sine,
               Cosine         : Real;
            end record;

         type Test_Data_Type is array (Positive range <>) of Data_Point;
         Test_Data : constant Test_Data_Type := (
            -- degrees       sine       cosine       test #
              (  0.0,         0.0,         1.0  ),    -- 1
              ( 90.0,         1.0,         0.0  ),    -- 2
              (180.0,         0.0,        -1.0  ),    -- 3
              (270.0,        -1.0,         0.0  ),    -- 4
              (360.0,         0.0,         1.0  ),    -- 5
              ( 90.0 + 360.0, 1.0,         0.0  ),    -- 6
              (180.0 + 360.0, 0.0,        -1.0  ),    -- 7
              (270.0 + 360.0,-1.0,         0.0  ),    -- 8
              (360.0 + 360.0, 0.0,         1.0  ) );  -- 9
         
         Y : Real;   
      begin
         for I in Test_Data'Range loop
            Y := Sin (Test_Data(I).Degrees, 360.0);
            if Y /= Test_Data(I).Sine then
               Report.Failed ("exact result for sin(" &
                  Real'Image (Test_Data(I).Degrees) & 
                  ", 360.0) is not" &
                  Real'Image (Test_Data(I).Sine) &
                  "  Difference is " &
                  Real'Image (Y - Test_Data(I).Sine) );
            end if;

            Y := Cos (Test_Data(I).Degrees, 360.0);
            if Y /= Test_Data(I).Cosine then
               Report.Failed ("exact result for cos(" &
                  Real'Image (Test_Data(I).Degrees) & 
                  ", 360.0) is not" &
                  Real'Image (Test_Data(I).Cosine) &
                  "  Difference is " &
                  Real'Image (Y - Test_Data(I).Cosine) );
            end if;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed ("Constraint_Error raised in exact result check");
         when others =>
            Report.Failed ("exception in exact result check");
      end Exact_Result_Checks;


      procedure Do_Test is
      begin
         Special_Angle_Checks;
         Sin_Check (0.0, Pi/2.0, "0..pi/2");
         Sin_Check (6.0*Pi, 6.5*Pi, "6pi..6.5pi");
         Cos_Check (7.0*Pi, 7.5*Pi, "7pi..7.5pi");
         Exact_Result_Checks;
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
   Report.Test ("CXG2004",
                "Check the accuracy of the sin and cos functions"); 

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
end CXG2004;
