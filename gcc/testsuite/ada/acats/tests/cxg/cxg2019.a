-- CXG2019.A
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
--      Check that the complex LOG function returns
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
--         Exception conditions.
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
--      22 Mar 96   SAIC    Initial release for 2.1
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
procedure CXG2019 is
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

      function Log (X : Complex) return Complex renames CEF.Log;

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
         -- bound can be used if the argument is exact.
         --
         -- When using pi there is an extra error of 1.0ME.  
         -- Although the real component has an error bound of 13.0, 
         -- the complex component must take into account this error
         -- in the value for Pi.
         --
         -- One or i is added to the actual and expected results in
         -- order to prevent the expected result from having a 
         -- real or imaginary part of 0.  This is to allow a reasonable
         -- relative error for that component.
         Minimum_Error : constant := 13.0; 
      begin
         Check (1.0 + Log (0.0 + i),
                1.0 + Pi / 2.0 * i,
                "1+log(0+i)",
                Minimum_Error + 1.0);
         Check (1.0 + Log ((-1.0, 0.0)),
                1.0 + (Pi * i),
                "log(-1+0i)+1 ",
                Minimum_Error + 1.0);
      exception
         when Constraint_Error => 
            Report.Failed ("Constraint_Error raised in special value test");
         when others =>
            Report.Failed ("exception in special value test");
      end Special_Value_Test;



      procedure Exact_Result_Test is
         No_Error : constant := 0.0;
      begin
         -- G.1.2(37);6.0
         Check (Log(1.0 + 0.0*i),  0.0 + 0.0 * i, "log(1+0i)", No_Error);
      exception
         when Constraint_Error => 
            Report.Failed ("Constraint_Error raised in Exact_Result Test");
         when others =>
            Report.Failed ("exception in Exact_Result Test");
      end Exact_Result_Test;


      procedure Identity_Test (RA, RB, IA, IB : Real) is
      -- Tests an identity over a range of values specified
      -- by the 4 parameters.  RA and RB denote the range for the
      -- real part while IA and IB denote the range for the 
      -- imaginary part.
      --
      -- For this test we use the identity
      --    Log(Z*Z) = 2 * Log(Z)
      --

         Scale : Real := Real (Real'Machine_Radix) ** (Real'Mantissa / 2 + 4);
         W, X, Y, Z : Real;
         CX, CY : Complex;
         Actual1, Actual2 : Complex;
      begin
         Accuracy_Error_Reported := False;  -- reset
         for II in 1..Max_Samples loop
            X :=  (RB - RA) * Real (II) / Real (Max_Samples) + RA;
            for J in 1..Max_Samples loop
               Y :=  (IB - IA) * Real (J) / Real (Max_Samples) + IA;
               
               -- purify the arguments to minimize roundoff error.
               -- We construct the values so that the products X*X, 
               -- Y*Y, and X*Y are all exact machine numbers.
               -- See Cody page 7 and CELEFUNT code.
               Z := X * Scale;
               W := Z + X;
               X := W - Z;
               Z := Y * Scale;
               W := Z + Y;
               Y := W - Z;
               CX := Compose_From_Cartesian(X,Y);
               Z := X*X - Y*Y;
               W := X*Y;
               CY := Compose_From_Cartesian(Z,W+W);
 
               -- The arguments are now ready so on with the 
               -- identity computation.
               Actual1 := Log(CX);
           
               Actual2 := Log(CY) * 0.5;
 
               Check (Actual1, Actual2,
                      "Identity_1_Test " & Integer'Image (II) & 
                         Integer'Image (J) & ": Log((" &
		         Real'Image (CX.Re) & ", " &
		         Real'Image (CX.Im) & ")) ",
                      26.0);   -- 2 logs = 2*13.  no error from this multiply

               if Accuracy_Error_Reported then
                 -- only report the first error in this test in order to keep
                 -- lots of failures from producing a huge error log
                 return;
               end if;
            end loop;
         end loop;

      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in Identity_Test" &
                " for X=(" & Real'Image (X) &
                ", " & Real'Image (X) & ")");
         when others =>
            Report.Failed ("exception in Identity_Test" &
                " for X=(" & Real'Image (X) &
                ", " & Real'Image (X) & ")");
      end Identity_Test;


      procedure Exception_Test is
      -- Check that log((0,0)) causes constraint_error.
      -- G.1.2(29);

         X : Complex := (0.0, 0.0);
      begin
         if not Real'Machine_Overflows then
            -- not applicable:   G.1.2(28);6.0
            return;
         end if;

         begin
            X := Log ((0.0, 0.0));
            Report.Failed ("exception not raised for log(0,0)");
         exception
            when Constraint_Error => null;  -- ok
            when others =>
               Report.Failed ("wrong exception raised for log(0,0)");
         end;

         -- optimizer thwarting
         if Report.Ident_Bool(False) then
            Report.Comment (Real'Image (X.Re + X.Im));
         end if;
      end Exception_Test;


      procedure Do_Test is
      begin
         Special_Value_Test;
         Exact_Result_Test;
            -- test regions that do not include the unit circle so that
            -- the real part of LOG(Z) does not vanish
            -- See Cody page 9.
         Identity_Test (   2.0,   10.0,       0.0,    10.0);
         Identity_Test (1000.0, 2000.0,   -4000.0, -1000.0);
         Identity_Test (Real'Model_Epsilon, 0.25,
                        -0.25,              -Real'Model_Epsilon);
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
   Report.Test ("CXG2019",
                "Check the accuracy of the complex LOG function"); 

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
end CXG2019;
