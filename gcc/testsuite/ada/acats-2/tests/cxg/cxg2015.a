-- CXG2015.A
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
--      Check that the ARCSIN and ARCCOS functions return
--      results that are within the error bound allowed.
--
-- TEST DESCRIPTION:
--      This test consists of a generic package that is
--      instantiated to check both Float and a long float type.
--      The test for each floating point type is divided into
--      several parts:
--         Special value checks where the result is a known constant.
--         Checks in a specific range where a Taylor series can be
--         used to compute an accurate result for comparison.
--         Exception checks.
--      The Taylor series tests are a direct translation of the
--      FORTRAN code found in the reference.
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
--      18 Mar 96   SAIC    Initial release for 2.1
--      24 Apr 96   SAIC    Fixed error bounds.
--      17 Aug 96   SAIC    Added reference information and improved
--                          checking for machines with more than 23
--                          digits of precision.
--      03 Feb 97   PWB.CTA Removed checks with explicit Cycle => 2.0*Pi
--      22 Dec 99   RLB     Added model range checking to "exact" results,
--                          in order to avoid too strictly requiring a specific
--                          result, and too weakly checking results.
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
-- CELEFUNT: A Portable Test Package for Complex Elementary Functions
-- ACM Collected Algorithms number 714

with System;
with Report;
with Ada.Numerics.Generic_Elementary_Functions;
procedure CXG2015 is
   Verbose : constant Boolean := False;
   Max_Samples : constant := 1000;


   -- CRC Standard Mathematical Tables;  23rd Edition; pg 738
   Sqrt2 : constant :=
        1.41421_35623_73095_04880_16887_24209_69807_85696_71875_37695;
   Sqrt3 : constant :=
        1.73205_08075_68877_29352_74463_41505_87236_69428_05253_81039;

   Pi : constant := Ada.Numerics.Pi;

   -- relative error bound from G.2.4(7);6.0
   Minimum_Error : constant := 4.0;

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

      function Arcsin (X : Real) return Real renames
           Elementary_Functions.Arcsin;
      function Arcsin (X, Cycle : Real) return Real renames
           Elementary_Functions.Arcsin;
      function Arccos (X : Real) return Real renames
           Elementary_Functions.ArcCos;
      function Arccos (X, Cycle : Real) return Real renames
           Elementary_Functions.ArcCos;

      -- needed for support
      function Log (X, Base : Real) return Real renames
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
         -- we compute the maximum error as a multiple of Model_Epsilon instead
         -- of Model_Epsilon and Expected.
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
         -- In the following tests the expected result is accurate
         -- to the machine precision so the minimum guaranteed error
         -- bound can be used.

         type Data_Point is
            record
               Degrees,
               Radians,
               Argument,
               Error_Bound : Real;
            end record;

         type Test_Data_Type is array (Positive range <>) of Data_Point;

         -- the values in the following tables only involve static
         -- expressions so no loss of precision occurs.  However,
         -- rounding can be an issue with expressions involving Pi
         -- and square roots.  The error bound specified in the
         -- table takes the sqrt error into account but not the
         -- error due to Pi.  The Pi error is added in in the
         -- radians test below.

         Arcsin_Test_Data : constant Test_Data_Type := (
         --  degrees      radians          sine  error_bound   test #
          --(  0.0,           0.0,          0.0,     0.0 ),    -- 1 - In Exact_Result_Test.
            ( 30.0,        Pi/6.0,          0.5,     4.0 ),    -- 2
            ( 60.0,        Pi/3.0,    Sqrt3/2.0,     5.0 ),    -- 3
          --( 90.0,        Pi/2.0,          1.0,     4.0 ),    -- 4 - In Exact_Result_Test.
          --(-90.0,       -Pi/2.0,         -1.0,     4.0 ),    -- 5 - In Exact_Result_Test.
            (-60.0,       -Pi/3.0,   -Sqrt3/2.0,     5.0 ),    -- 6
            (-30.0,       -Pi/6.0,         -0.5,     4.0 ),    -- 7
            ( 45.0,        Pi/4.0,    Sqrt2/2.0,     5.0 ),    -- 8
            (-45.0,       -Pi/4.0,   -Sqrt2/2.0,     5.0 ) );  -- 9

         Arccos_Test_Data : constant Test_Data_Type := (
         --  degrees      radians       cosine   error_bound   test #
          --(  0.0,           0.0,         1.0,      0.0 ),    -- 1 - In Exact_Result_Test.
            ( 30.0,        Pi/6.0,   Sqrt3/2.0,      5.0 ),    -- 2
            ( 60.0,        Pi/3.0,         0.5,      4.0 ),    -- 3
          --( 90.0,        Pi/2.0,         0.0,      4.0 ),    -- 4 - In Exact_Result_Test.
            (120.0,    2.0*Pi/3.0,        -0.5,      4.0 ),    -- 5
            (150.0,    5.0*Pi/6.0,  -Sqrt3/2.0,      5.0 ),    -- 6
          --(180.0,            Pi,        -1.0,      4.0 ),    -- 7 - In Exact_Result_Test.
            ( 45.0,        Pi/4.0,   Sqrt2/2.0,      5.0 ),    -- 8
            (135.0,    3.0*Pi/4.0,  -Sqrt2/2.0,      5.0 ) );  -- 9

         Cycle_Error,
         Radian_Error : Real;
      begin
         for I in Arcsin_Test_Data'Range loop

            -- note exact result requirements  A.5.1(38);6.0 and
            -- G.2.4(12);6.0
            if Arcsin_Test_Data (I).Error_Bound = 0.0 then
               Cycle_Error := 0.0;
               Radian_Error := 0.0;
            else
               Cycle_Error := Arcsin_Test_Data (I).Error_Bound;
               -- allow for rounding error in the specification of Pi
               Radian_Error := Cycle_Error + 1.0;
            end if;

            Check (Arcsin (Arcsin_Test_Data (I).Argument),
                   Arcsin_Test_Data (I).Radians,
                   "test" & Integer'Image (I) &
                   " arcsin(" &
                   Real'Image (Arcsin_Test_Data (I).Argument) &
                   ")",
                   Radian_Error);
--pwb-math            Check (Arcsin (Arcsin_Test_Data (I).Argument, 2.0 * Pi),
--pwb-math                   Arcsin_Test_Data (I).Radians,
--pwb-math                   "test" & Integer'Image (I) &
--pwb-math                   " arcsin(" &
--pwb-math                   Real'Image (Arcsin_Test_Data (I).Argument) &
--pwb-math                   ", 2pi)",
--pwb-math                   Cycle_Error);
            Check (Arcsin (Arcsin_Test_Data (I).Argument, 360.0),
                   Arcsin_Test_Data (I).Degrees,
                   "test" & Integer'Image (I) &
                   " arcsin(" &
                   Real'Image (Arcsin_Test_Data (I).Argument) &
                   ", 360)",
                   Cycle_Error);
         end loop;


         for I in Arccos_Test_Data'Range loop

            -- note exact result requirements  A.5.1(39);6.0 and
            -- G.2.4(12);6.0
            if Arccos_Test_Data (I).Error_Bound = 0.0 then
               Cycle_Error := 0.0;
               Radian_Error := 0.0;
            else
               Cycle_Error := Arccos_Test_Data (I).Error_Bound;
               -- allow for rounding error in the specification of Pi
               Radian_Error := Cycle_Error + 1.0;
            end if;

            Check (Arccos (Arccos_Test_Data (I).Argument),
                   Arccos_Test_Data (I).Radians,
                   "test" & Integer'Image (I) &
                   " arccos(" &
                   Real'Image (Arccos_Test_Data (I).Argument) &
                   ")",
                   Radian_Error);
--pwb-math            Check (Arccos (Arccos_Test_Data (I).Argument, 2.0 * Pi),
--pwb-math                   Arccos_Test_Data (I).Radians,
--pwb-math                   "test" & Integer'Image (I) &
--pwb-math                   " arccos(" &
--pwb-math                   Real'Image (Arccos_Test_Data (I).Argument) &
--pwb-math                   ", 2pi)",
--pwb-math                   Cycle_Error);
            Check (Arccos (Arccos_Test_Data (I).Argument, 360.0),
                   Arccos_Test_Data (I).Degrees,
                   "test" & Integer'Image (I) &
                   " arccos(" &
                   Real'Image (Arccos_Test_Data (I).Argument) &
                   ", 360)",
                   Cycle_Error);
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
         --  A.5.1(38)
         Check_Exact (Arcsin (0.0),       0.0, 0.0, "arcsin(0)");
         Check_Exact (Arcsin (0.0, 45.0), 0.0, 0.0, "arcsin(0,45)");

         --  A.5.1(39)
         Check_Exact (Arccos (1.0),       0.0, 0.0, "arccos(1)");
         Check_Exact (Arccos (1.0, 75.0), 0.0, 0.0, "arccos(1,75)");

         --  G.2.4(11-13)
         Check_Exact (Arcsin (1.0), Half_PI_Low, Half_PI_High, "arcsin(1)");
         Check_Exact (Arcsin (1.0, 360.0), 90.0, 90.0, "arcsin(1,360)");

         Check_Exact (Arcsin (-1.0), -Half_PI_High, -Half_PI_Low, "arcsin(-1)");
         Check_Exact (Arcsin (-1.0, 360.0), -90.0, -90.0, "arcsin(-1,360)");

         Check_Exact (Arccos (0.0), Half_PI_Low, Half_PI_High, "arccos(0)");
         Check_Exact (Arccos (0.0, 360.0), 90.0, 90.0, "arccos(0,360)");

         Check_Exact (Arccos (-1.0), PI_Low, PI_High, "arccos(-1)");
         Check_Exact (Arccos (-1.0, 360.0), 180.0, 180.0, "arccos(-1,360)");

      exception
         when Constraint_Error =>
            Report.Failed ("Constraint_Error raised in Exact_Result Test");
         when others =>
            Report.Failed ("Exception in Exact_Result Test");
      end Exact_Result_Test;


      procedure Arcsin_Taylor_Series_Test is
         -- the following range is chosen so that the Taylor series
         -- used will produce a result accurate to machine precision.
         --
         -- The following formula is used for the Taylor series:
         --  TS(x) =  x { 1 + (xsq/2) [ (1/3) + (3/4)xsq { (1/5) +
         --                (5/6)xsq [ (1/7) + (7/8)xsq/9 ] } ] }
         --   where xsq = x * x
         --
         A : constant := -0.125;
         B : constant :=  0.125;
         X : Real;
         Y, Y_Sq : Real;
         Actual, Sum, Xm : Real;
         -- terms in Taylor series
         K : constant Integer := Integer (
                Log (
                  Real (Real'Machine_Radix) ** Real'Machine_Mantissa,
                  10.0)) + 1;
      begin
         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            -- make sure there is no error in x-1, x, and x+1
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;

            Y := X;
            Y_Sq := Y * Y;
            Sum := 0.0;
            Xm := Real (K + K + 1);
            for M in 1 .. K loop
               Sum := Y_Sq * (Sum + 1.0/Xm);
               Xm := Xm - 2.0;
               Sum := Sum * (Xm /(Xm + 1.0));
            end loop;
            Sum := Sum * Y;
            Actual := Y + Sum;
            Sum := (Y - Actual) + Sum;
            if not Real'Machine_Rounds then
               Actual := Actual + (Sum + Sum);
            end if;

            Check (Actual, Arcsin (X),
                   "Taylor Series test" & Integer'Image (I) & ": arcsin(" &
		   Real'Image (X) & ") ",
                   Minimum_Error);

            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;

         end loop;

      exception
         when Constraint_Error =>
            Report.Failed
               ("Constraint_Error raised in Arcsin_Taylor_Series_Test" &
                " for X=" & Real'Image (X));
         when others =>
            Report.Failed ("exception in Arcsin_Taylor_Series_Test" &
                " for X=" & Real'Image (X));
      end Arcsin_Taylor_Series_Test;



      procedure Arccos_Taylor_Series_Test is
         -- the following range is chosen so that the Taylor series
         -- used will produce a result accurate to machine precision.
         --
         -- The following formula is used for the Taylor series:
         --  TS(x) =  x { 1 + (xsq/2) [ (1/3) + (3/4)xsq { (1/5) +
         --                (5/6)xsq [ (1/7) + (7/8)xsq/9 ] } ] }
         --  arccos(x) = pi/2 - TS(x)
         A : constant := -0.125;
         B : constant :=  0.125;
         C1, C2 : Real;
         X : Real;
         Y, Y_Sq : Real;
         Actual, Sum, Xm, S : Real;
         -- terms in Taylor series
         K : constant Integer := Integer (
                Log (
                  Real (Real'Machine_Radix) ** Real'Machine_Mantissa,
                  10.0)) + 1;
      begin
         if Real'Digits > 23 then
            -- constants in this section only accurate to 23 digits
            Error_Low_Bound := 0.00000_00000_00000_00000_001;
            Report.Comment ("arctan accuracy checked to 23 digits");
         end if;

         -- C1 + C2 equals Pi/2 accurate to 23 digits
         if Real'Machine_Radix = 10 then
            C1 := 1.57;
            C2 := 7.9632679489661923132E-4;
         else
            C1 := 201.0 / 128.0;
            C2 := 4.8382679489661923132E-4;
         end if;

         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            -- make sure there is no error in x-1, x, and x+1
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;

            Y := X;
            Y_Sq := Y * Y;
            Sum := 0.0;
            Xm := Real (K + K + 1);
            for M in 1 .. K loop
               Sum := Y_Sq * (Sum + 1.0/Xm);
               Xm := Xm - 2.0;
               Sum := Sum * (Xm /(Xm + 1.0));
            end loop;
            Sum := Sum * Y;

            -- at this point we have arcsin(x).
            -- We compute arccos(x) = pi/2 - arcsin(x).
            -- The following code segment is translated directly from
            -- the CELEFUNT FORTRAN implementation

            S := C1 + C2;
            Sum := ((C1 - S) + C2) - Sum;
            Actual := S + Sum;
            Sum := ((S - Actual) + Sum) - Y;
            S := Actual;
            Actual := S + Sum;
            Sum := (S - Actual) + Sum;

            if not Real'Machine_Rounds then
               Actual := Actual + (Sum + Sum);
            end if;

            Check (Actual, Arccos (X),
                   "Taylor Series test" & Integer'Image (I) & ": arccos(" &
		   Real'Image (X) & ") ",
                   Minimum_Error);

              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
            exit when Accuracy_Error_Reported;
         end loop;
         Error_Low_Bound := 0.0;  -- reset
      exception
         when Constraint_Error =>
            Report.Failed
               ("Constraint_Error raised in Arccos_Taylor_Series_Test" &
                " for X=" & Real'Image (X));
         when others =>
            Report.Failed ("exception in Arccos_Taylor_Series_Test" &
                " for X=" & Real'Image (X));
      end Arccos_Taylor_Series_Test;



      procedure Identity_Test is
         -- test the identity arcsin(-x) = -arcsin(x)
         -- range chosen to be most of the valid range of the argument.
         A : constant := -0.999;
         B : constant :=  0.999;
         X : Real;
      begin
         Accuracy_Error_Reported := False;  -- reset
         for I in 1..Max_Samples loop
            -- make sure there is no error in x-1, x, and x+1
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;

            Check (Arcsin(-X), -Arcsin (X),
                   "Identity test" & Integer'Image (I) & ": arcsin(" &
		   Real'Image (X) & ") ",
                   8.0);   -- 2 arcsin evaluations => twice the error bound

            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;
         end loop;
      end Identity_Test;


      procedure Exception_Test is
         X1, X2 : Real := 0.0;
      begin
	    begin
	      X1 := Arcsin (1.1);
	      Report.Failed ("no exception for Arcsin (1.1)");
	    exception
	       when Constraint_Error =>
	          Report.Failed ("Constraint_Error instead of " &
                     "Argument_Error for Arcsin (1.1)");
               when Ada.Numerics.Argument_Error =>
                  null;    -- expected result
	       when others =>
	          Report.Failed ("wrong exception for Arcsin(1.1)");
	    end;

	    begin
	      X2 := Arccos (-1.1);
	      Report.Failed ("no exception for Arccos (-1.1)");
	    exception
	       when Constraint_Error =>
	          Report.Failed ("Constraint_Error instead of " &
                     "Argument_Error for Arccos (-1.1)");
               when Ada.Numerics.Argument_Error =>
                  null;    -- expected result
	       when others =>
	          Report.Failed ("wrong exception for Arccos(-1.1)");
	    end;


         -- optimizer thwarting
         if Report.Ident_Bool (False) then
            Report.Comment (Real'Image (X1 + X2));
         end if;
      end Exception_Test;


      procedure Do_Test is
      begin
         Special_Value_Test;
         Exact_Result_Test;
         Arcsin_Taylor_Series_Test;
         Arccos_Taylor_Series_Test;
         Identity_Test;
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

   -- check the floating point type with the most digits
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
   Report.Test ("CXG2015",
                "Check the accuracy of the ARCSIN and ARCCOS functions");

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
end CXG2015;
