-- CXG2021.A
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
--      Check that the complex SIN and COS functions return
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
--      27 Mar 96   SAIC    Initial release for 2.1
--      22 Aug 96   SAIC    No longer skips test for systems with
--                          more than 20 digits of precision.
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
procedure CXG2021 is
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

      function Sin (X : Complex) return Complex renames CEF.Sin;
      function Cos (X : Complex) return Complex renames CEF.Cos;

      -- flag used to terminate some tests early
      Accuracy_Error_Reported : Boolean := False;

      -- The following value is a lower bound on the accuracy
      -- required.  It is normally 0.0 so that the lower bound
      -- is computed from Model_Epsilon.  However, for tests
      -- where the expected result is only known to a certain
      -- amount of precision this bound takes on a non-zero 
      -- value to account for that level of precision.
      Error_Low_Bound : Real := 0.0;

      -- the E_Factor is an additional amount added to the Expected
      -- value prior to computing the maximum relative error.
      -- This is needed because the error analysis (Cody pg 17-20)
      -- requires this additional allowance.
      procedure Check (Actual, Expected : Real;
                       Test_Name : String;
                       MRE : Real;
                       E_Factor : Real := 0.0) is
         Max_Error : Real;
         Rel_Error : Real;
         Abs_Error : Real;
      begin
         -- In the case where the expected result is very small or 0
         -- we compute the maximum error as a multiple of Model_Epsilon instead
         -- of Model_Epsilon and Expected.
         Rel_Error := MRE * Real'Model_Epsilon * (abs Expected + E_Factor);
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
                           " max err:" & Real'Image (Max_Error) &
                           " efactor:" & Real'Image (E_Factor) );
         elsif Verbose then
	    if Actual = Expected then
	       Report.Comment (Test_Name & "  exact result");
	    else
	       Report.Comment (Test_Name & "  passed" &
                           " actual: " & Real'Image (Actual) &
                           " expected: " & Real'Image (Expected) &
                           " difference: " & Real'Image (Actual - Expected) &
                           " max err:" & Real'Image (Max_Error) &
                           " efactor:" & Real'Image (E_Factor) );
	    end if;
         end if;
      end Check;


      procedure Check (Actual, Expected : Complex;
                       Test_Name : String;
                       MRE : Real;
                       R_Factor, I_Factor : Real := 0.0) is
      begin
         Check (Actual.Re, Expected.Re, Test_Name & " real part", 
                MRE, R_Factor);
         Check (Actual.Im, Expected.Im, Test_Name & " imaginary part", 
                MRE, I_Factor);
      end Check;


      procedure Special_Value_Test is
         -- In the following tests the expected result is accurate
         -- to the machine precision so the minimum guaranteed error
         -- bound can be used if the argument is exact.
         -- Since the argument involves Pi, we must allow for this
         -- inexact argument.
         Minimum_Error : constant := 11.0; 
      begin
         Check (Sin (Pi/2.0 + 0.0*i),
                1.0 + 0.0*i,
                "sin(pi/2+0i)",
                Minimum_Error + 1.0);
         Check (Cos (Pi/2.0 + 0.0*i),
                0.0 + 0.0*i,
                "cos(pi/2+0i)",
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
         -- G.1.2(36);6.0
         Check (Sin(0.0 + 0.0*i),  0.0 + 0.0 * i, "sin(0+0i)", No_Error);
         Check (Cos(0.0 + 0.0*i),  1.0 + 0.0 * i, "cos(0+0i)", No_Error);
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
      --    Sin(Z) = Sin(Z-W) * Cos(W) + Cos(Z-W) * Sin(W)
      -- and
      --    Cos(Z) = Cos(Z-W) * Cos(W) - Sin(Z-W) * Sin(W)
      --

         X, Y : Real;
         Z   : Complex;
         W : constant Complex := Compose_From_Cartesian(0.0625, 0.0625);
         ZmW : Complex;  -- Z - W
         Sin_ZmW,
         Cos_ZmW : Complex;
         Actual1, Actual2 : Complex;
         R_Factor : Real;  -- additional real error factor
         I_Factor : Real;  -- additional imaginary error factor
         Sin_W : constant Complex := (6.2581348413276935585E-2,
                                      6.2418588008436587236E-2);
         -- numeric stability is enhanced by using Cos(W) - 1.0 instead of
         -- Cos(W) in the computation.
         Cos_W_m_1 : constant Complex := (-2.5431314180235545803E-6,
                                            -3.9062493377261771826E-3);


      begin
         if Real'Digits > 20 then
            -- constants used here accurate to 20 digits.  Allow 1
            -- additional digit of error for computation.
            Error_Low_Bound := 0.00000_00000_00000_0001;
            Report.Comment ("accuracy checked to 19 digits");
         end if;

         Accuracy_Error_Reported := False;  -- reset
         for II in 0..Max_Samples loop
            X :=  (RB - RA) * Real (II) / Real (Max_Samples) + RA;
            for J in 0..Max_Samples loop
               Y :=  (IB - IA) * Real (J) / Real (Max_Samples) + IA;
               
               Z := Compose_From_Cartesian(X,Y);
               ZmW := Z - W;
               Sin_ZmW := Sin (ZmW);
               Cos_ZmW := Cos (ZmW);

               -- now for the first identity 
               --    Sin(Z) = Sin(Z-W) * Cos(W) + Cos(Z-W) * Sin(W)
               --           = Sin(Z-W) * (1+(Cos(W)-1)) + Cos(Z-W) * Sin(W)
               --           = Sin(Z-W) + Sin(Z-W)*(Cos(W)-1) + Cos(Z-W)*Sin(W)


               Actual1 := Sin (Z);
               Actual2 := Sin_ZmW + (Sin_ZmW * Cos_W_m_1 + Cos_ZmW * Sin_W);

               -- The computation of the additional error factors are taken
               -- from Cody pages 17-20.

               R_Factor := abs (Re (Sin_ZmW) * Re (1.0 - Cos_W_m_1)) +
                           abs (Im (Sin_ZmW) * Im (1.0 - Cos_W_m_1)) +
                           abs (Re (Cos_ZmW) * Re (Sin_W)) +
                           abs (Re (Cos_ZmW) * Re (1.0 - Cos_W_m_1));
 
               I_Factor := abs (Re (Sin_ZmW) * Im (1.0 - Cos_W_m_1)) +
                           abs (Im (Sin_ZmW) * Re (1.0 - Cos_W_m_1)) +
                           abs (Re (Cos_ZmW) * Im (Sin_W)) +
                           abs (Im (Cos_ZmW) * Re (1.0 - Cos_W_m_1));
 
               Check (Actual1, Actual2,
                      "Identity_1_Test " & Integer'Image (II) & 
                         Integer'Image (J) & ": Sin((" &
		         Real'Image (Z.Re) & ", " &
		         Real'Image (Z.Im) & ")) ",
                      11.0, R_Factor, I_Factor); 

               -- now for the second identity 
               --    Cos(Z) = Cos(Z-W) * Cos(W) - Sin(Z-W) * Sin(W)
               --           = Cos(Z-W) * (1+(Cos(W)-1) - Sin(Z-W) * Sin(W)
               Actual1 := Cos (Z);
               Actual2 := Cos_ZmW + (Cos_ZmW * Cos_W_m_1 - Sin_ZmW * Sin_W);

               -- The computation of the additional error factors are taken
               -- from Cody pages 17-20.

               R_Factor := abs (Re (Sin_ZmW) * Re (Sin_W)) +
                           abs (Im (Sin_ZmW) * Im (Sin_W)) +
                           abs (Re (Cos_ZmW) * Re (1.0 - Cos_W_m_1)) +
                           abs (Im (Cos_ZmW) * Im (1.0 - Cos_W_m_1));
 
               I_Factor := abs (Re (Sin_ZmW) * Im (Sin_W)) +
                           abs (Im (Sin_ZmW) * Re (Sin_W)) +
                           abs (Re (Cos_ZmW) * Im (1.0 - Cos_W_m_1)) +
                           abs (Im (Cos_ZmW) * Re (1.0 - Cos_W_m_1));
 
               Check (Actual1, Actual2,
                      "Identity_2_Test " & Integer'Image (II) & 
                         Integer'Image (J) & ": Cos((" &
		         Real'Image (Z.Re) & ", " &
		         Real'Image (Z.Im) & ")) ",
                      11.0, R_Factor, I_Factor); 

               if Accuracy_Error_Reported then
                 -- only report the first error in this test in order to keep
                 -- lots of failures from producing a huge error log
                 Error_Low_Bound := 0.0;  -- reset
                 return;
               end if;
            end loop;
         end loop;

         Error_Low_Bound := 0.0;  -- reset
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in Identity_Test" &
                " for Z=(" & Real'Image (X) &
                ", " & Real'Image (Y) & ")");
         when others =>
            Report.Failed ("exception in Identity_Test" &
                " for Z=(" & Real'Image (X) &
                ", " & Real'Image (Y) & ")");
      end Identity_Test;


      procedure Do_Test is
      begin
         Special_Value_Test;
         Exact_Result_Test;
            -- test regions where sin and cos have the same sign and
            -- about the same magnitude.  This will minimize subtraction
            -- errors in the identities.
            -- See Cody page 17.
         Identity_Test (0.0625,   10.0,    0.0625,    10.0);
         Identity_Test (  16.0,   17.0,      16.0,    17.0);
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
   Report.Test ("CXG2021",
                "Check the accuracy of the complex SIN and COS functions"); 

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
end CXG2021;
