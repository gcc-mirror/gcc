-- CXG2012.A
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
--      Check that the exponentiation operator returns
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
--      While this test concentrates on the "**" operator
--      defined in Generic_Elementary_Functions, a check is also
--      performed on the standard "**" operator.
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
--       7 Mar 96   SAIC    Initial release for 2.1
--       2 Sep 96   SAIC    Improvements as suggested by reviewers
--	 3 Jun 98   EDS     Add parens to ensure that the expression is not
--                          evaluated by multiplying its two large terms
--                          together and overflowing.
--       3 Dec 01   RLB     Added 'Machine to insure that equality tests
--                          are certain to work.
--
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
procedure CXG2012 is
   Verbose : constant Boolean := False;
   Max_Samples : constant := 1000;

   -- CRC Standard Mathematical Tables;  23rd Edition; pg 738
   Sqrt2 : constant :=
        1.41421_35623_73095_04880_16887_24209_69807_85696_71875_37695;
   Sqrt3 : constant :=
        1.73205_08075_68877_29352_74463_41505_87236_69428_05253_81039;


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
      function Exp (X : Real) return Real renames
           Elementary_Functions.Exp;
      function Log (X : Real) return Real renames
           Elementary_Functions.Log;
      function "**" (L, R : Real) return Real renames
           Elementary_Functions."**";

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
         -- we compute the maximum error as a multiple of Model_Epsilon
         -- instead of Model_Epsilon and Expected.
         Rel_Error := MRE * (abs Expected * Real'Model_Epsilon);
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


      -- the following version of Check computes the allowed error bound
      -- using the operands
      procedure Check (Actual, Expected : Real;
                       Left, Right : Real;
                       Test_Name : String;
                       MRE_Factor : Real := 1.0) is
         MRE : Real;
      begin
         MRE := MRE_Factor * (4.0 + abs (Right * Log(Left)) / 32.0);
         Check (Actual, Expected, Test_Name, MRE);
      end Check;


      procedure Real_To_Integer_Test is
         type Int_Check is
	    record
	       Left : Real;
	       Right : Integer;
	       Expected : Real;
	    end record;
	 type Int_Checks is array (Positive range <>) of Int_Check;

         -- the following tests use only model numbers so the result
         -- is expected to be exact.
	 IC : constant Int_Checks :=
         ( (  2.0,   5,       32.0),
           ( -2.0,   5,      -32.0),
           (  0.5,  -5,       32.0),
           (  2.0,   0,        1.0),
           (  0.0,   0,        1.0) );
      begin
	 for I in IC'Range loop
            declare
	       Y : Real;
            begin
               Y := IC (I).Left ** IC (I).Right;
               Check (Y, IC (I).Expected,
		      "real to integer test" &
		      Real'Image (IC (I).Left) & " ** " &
		      Integer'Image (IC (I).Right),
		      0.0);  -- no error allowed
            exception
               when Constraint_Error =>
                  Report.Failed ("Constraint_Error raised in rtoi test " &
		     Integer'Image (I));
               when others =>
                  Report.Failed ("exception in rtoi test " &
		     Integer'Image (I));
            end;
         end loop;
      end Real_To_Integer_Test;


      procedure Special_Value_Test is
         No_Error : constant := 0.0;
      begin
         Check (0.0 ** 1.0, 0.0, "0**1", No_Error);
         Check (1.0 ** 0.0, 1.0, "1**0", No_Error);

         Check ( 2.0 **  5.0,  32.0,  2.0,  5.0,  "2**5");
         Check ( 0.5**(-5.0),  32.0,  0.5, -5.0,  "0.5**-5");

         Check (Sqrt2 ** 4.0,   4.0,  Sqrt2, 4.0,  "Sqrt2**4");
         Check (Sqrt3 ** 6.0,  27.0,  Sqrt3, 6.0,  "Sqrt3**6");

         Check (2.0 ** 0.5,   Sqrt2,    2.0, 0.5,  "2.0**0.5");

      exception
         when Constraint_Error =>
            Report.Failed ("Constraint_Error raised in Special Value Test");
         when others =>
            Report.Failed ("exception in Special Value Test");
      end Special_Value_Test;


      procedure Small_Range_Test is
      -- Several checks over the range 1/radix .. 1
         A : constant Real := 1.0 / Real (Real'Machine_Radix);
         B : constant Real := 1.0;
         X : Real;
         -- In the cases below where the expected result is
         -- inexact we allow an additional error amount of
         -- 1.0 * Model_Epsilon to account for that error.
         -- This is accomplished by the factor of 1.25 times
         -- the computed error bound (which is > 4.0) thus
         -- increasing the error bound by at least
         -- 1.0 * Model_Epsilon
      begin
         Accuracy_Error_Reported := False;  -- reset
         for I in 0..Max_Samples loop
            X :=  Real'Machine((B - A) * Real (I) / Real (Max_Samples) + A);

            Check (X ** 1.0, X,  -- exact result required
                   "Small range" & Integer'Image (I) & ": " &
                   Real'Image (X) & " ** 1.0",
                   0.0);

            Check ((X*X) ** 1.5, X**3,  X*X, 1.5,
                   "Small range" & Integer'Image (I) & ": " &
                   Real'Image (X*X) & " ** 1.5",
                   1.25);

            Check (X ** 13.5, 1.0 / (X ** (-13.5)),  X, 13.5,
                   "Small range" & Integer'Image (I) & ": " &
                   Real'Image (X) & " ** 13.5",
                   2.0);   -- 2 ** computations

            Check ((X*X) ** 1.25, X**(2.5),  X*X, 1.25,
                   "Small range" & Integer'Image (I) & ": " &
                   Real'Image (X*X) & " ** 1.25",
                   2.0);   -- 2 ** computations

            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;

         end loop;

      exception
         when Constraint_Error =>
            Report.Failed
               ("Constraint_Error raised in Small Range Test");
         when others =>
            Report.Failed ("exception in Small Range Test");
      end Small_Range_Test;


      procedure Large_Range_Test is
      -- Check over the range A to B where A is 1.0 and
      -- B is a large value.
         A : constant Real := 1.0;
         B : Real;
         X : Real;
         Iteration : Integer := 0;
         Subtest : Character := 'X';
      begin
         -- upper bound of range should be as large as possible where
         -- B**3 is still valid.
         B := Real'Safe_Last ** 0.333;
         Accuracy_Error_Reported := False;  -- reset
         for I in 0..Max_Samples loop
            Iteration := I;
            Subtest := 'X';
            X :=  Real'Machine((B - A) * (Real (I) / Real (Max_Samples)) + A);

            Subtest := 'A';
            Check (X ** 1.0, X,  -- exact result required
                   "Large range" & Integer'Image (I) & ": " &
                   Real'Image (X) & " ** 1.0",
                   0.0);

            Subtest := 'B';
            Check ((X*X) ** 1.5, X**3,  X*X, 1.5,
                   "Large range" & Integer'Image (I) & ": " &
                   Real'Image (X*X) & " ** 1.5",
                   1.25);   -- inexact expected result

            Subtest := 'C';
            Check ((X*X) ** 1.25, X**(2.5),  X*X, 1.25,
                   "Large range" & Integer'Image (I) & ": " &
                   Real'Image (X*X) & " ** 1.25",
                   2.0);   -- two ** operators

            if Accuracy_Error_Reported then
              -- only report the first error in this test in order to keep
              -- lots of failures from producing a huge error log
              return;
            end if;

         end loop;
      exception
         when Constraint_Error =>
            Report.Failed
               ("Constraint_Error raised in Large Range Test" &
                Integer'Image (Iteration) & Subtest);
         when others =>
            Report.Failed ("exception in Large Range Test" &
                Integer'Image (Iteration) & Subtest);
      end Large_Range_Test;


      procedure Exception_Test is
         X1, X2, X3, X4 : Real;
      begin
         begin
            X1 := 0.0 ** (-1.0);
            Report.Failed ("exception not raised for 0**-1");
         exception
            when Ada.Numerics.Argument_Error =>
               Report.Failed ("argument_error raised instead of" &
                              " constraint_error for 0**-1");
            when Constraint_Error => null;   -- ok
            when others =>
               Report.Failed ("wrong exception raised for 0**-1");
         end;

         begin
            X2 := 0.0 ** 0.0;
            Report.Failed ("exception not raised for 0**0");
         exception
            when Ada.Numerics.Argument_Error =>  null;  -- ok
            when Constraint_Error =>
               Report.Failed ("constraint_error raised instead of" &
                              " argument_error for 0**0");
            when others =>
               Report.Failed ("wrong exception raised for 0**0");
         end;

         begin
            X3 := (-1.0) ** 1.0;
            Report.Failed ("exception not raised for -1**1");
         exception
            when Ada.Numerics.Argument_Error =>  null;  -- ok
            when Constraint_Error =>
               Report.Failed ("constraint_error raised instead of" &
                              " argument_error for -1**1");
            when others =>
               Report.Failed ("wrong exception raised for -1**1");
         end;

         begin
            X4 := (-2.0) ** 2.0;
            Report.Failed ("exception not raised for -2**2");
         exception
            when Ada.Numerics.Argument_Error =>  null;  -- ok
            when Constraint_Error =>
               Report.Failed ("constraint_error raised instead of" &
                              " argument_error for -2**2");
            when others =>
               Report.Failed ("wrong exception raised for -2**2");
         end;

         -- optimizer thwarting
         if Report.Ident_Bool (False) then
            Report.Comment (Real'Image (X1+X2+X3+X4));
         end if;
      end Exception_Test;


      procedure Do_Test is
      begin
         Real_To_Integer_Test;
         Special_Value_Test;
         Small_Range_Test;
         Large_Range_Test;
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
   Report.Test ("CXG2012",
                "Check the accuracy of the ** operator");

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
end CXG2012;
