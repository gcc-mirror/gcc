-- CXG2006.A
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
--      Check that the complex Argument function returns
--      results that are within the error bound allowed.
--      Check that Argument_Error is raised if the Cycle parameter
--      is less than or equal to zero.
--
-- TEST DESCRIPTION:
--      This test uses a generic package to compute and check the
--      values of the Argument function.  
--      Of special interest is the case where either the real or
--      the imaginary part of the parameter is very large while the
--      other part is very small or 0. 
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
--      15 FEB 96   SAIC    Initial release for 2.1
--      03 MAR 97   PWB.CTA Removed checks involving explicit cycle => 2.0*Pi
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
-- Reference:
-- Problems and Methodologies in Mathematical Software Production;
-- editors: P. C. Messina and A Murli;
-- Lecture Notes in Computer Science
-- Volume 142
-- Springer Verlag 1982
--

with System;
with Report;
with ImpDef.Annex_G;
with Ada.Numerics;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Complex_Types;
procedure CXG2006 is
   Verbose : constant Boolean := False;


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
      package Complex_Types is new 
           Ada.Numerics.Generic_Complex_Types (Real);
      use Complex_Types;


      procedure Check (Actual, Expected : Real;
                       Test_Name : String;
                       MRE : Real) is
         Rel_Error : Real;
         Abs_Error : Real;
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

         if abs (Actual - Expected) > Max_Error then
            Report.Failed (Test_Name & 
                           " actual: " & Real'Image (Actual) &
                           " expected: " & Real'Image (Expected) &
                           " difference: " & 
                           Real'Image (Actual - Expected) &
                           " mre:" & Real'Image (Max_Error) );
         elsif Verbose then
	    if Actual = Expected then
	       Report.Comment (Test_Name & "  exact result");
	    else
	       Report.Comment (Test_Name & "  passed");
	    end if;
         end if;
      end Check;


      procedure Special_Cases is
         type Data_Point is 
            record
               Re,
               Im,
               Radians,
               Degrees,
               Error_Bound   : Real;
            end record;

         type Test_Data_Type is array (Positive range <>) of Data_Point;

         -- the values in the following table only involve static
         -- expressions to minimize errors in precision introduced by the
         -- test.  For cases where Pi is used in the argument we must
         -- allow an extra 1.0*MRE to account for roundoff error in the
         -- argument.  Where the result involves a square root we allow
         -- an extra 0.5*MRE to allow for roundoff error.
         Test_Data : constant Test_Data_Type := (
--    Re               Im                     Radians  Degrees  Err    Test #
    (0.0,               0.0,                       0.0,   0.0,  4.0 ),  -- 1
    (1.0,               0.0,                       0.0,   0.0,  4.0 ),  -- 2
    (Real'Safe_Last,    0.0,                       0.0,   0.0,  4.0 ),  -- 3
    (Real'Model_Small,  0.0,                       0.0,   0.0,  4.0 ),  -- 4
    (1.0,               1.0,                    Pi/4.0,  45.0,  5.0 ),  -- 5
    (1.0,              -1.0,                   -Pi/4.0, -45.0,  5.0 ),  -- 6
    (-1.0,             -1.0,               -3.0*Pi/4.0,-135.0,  5.0 ),  -- 7 
    (-1.0,              1.0,                3.0*Pi/4.0, 135.0,  5.0 ),  -- 8 
    (Sqrt3,             1.0,                    Pi/6.0,  30.0,  5.5 ),  -- 9
    (-Sqrt3,            1.0,                5.0*Pi/6.0, 150.0,  5.5 ),  -- 10
    (Sqrt3,            -1.0,                   -Pi/6.0, -30.0,  5.5 ),  -- 11
    (-Sqrt3,           -1.0,               -5.0*Pi/6.0,-150.0,  5.5 ),  -- 12
    (Real'Model_Small,  Real'Model_Small,       Pi/4.0,  45.0,  5.0 ),  -- 13
    (-Real'Safe_Last,   0.0,                        Pi, 180.0,  5.0 ),  -- 14
    (-Real'Safe_Last,  -Real'Model_Small,          -Pi,-180.0,  5.0 ),  -- 15
    (100000.0,          100000.0,               Pi/4.0,  45.0,  5.0 )); -- 16
    
         X : Real;
         Z : Complex;
      begin
         for I in Test_Data'Range loop
            begin
               Z := (Test_Data(I).Re, Test_Data(I).Im);
               X := Argument (Z);
	       Check (X, Test_Data(I).Radians, 
                   "test" & Integer'Image (I) & " argument(z)",
                   Test_Data (I).Error_Bound); 
--pwb-math               X := Argument (Z, 2.0*Pi);
--pwb-math	       Check (X, Test_Data(I).Radians, 
--pwb-math                   "test" & Integer'Image (I) & " argument(z, 2pi)",
--pwb-math                   Test_Data (I).Error_Bound); 
               X := Argument (Z, 360.0);
	       Check (X, Test_Data(I).Degrees, 
                   "test" & Integer'Image (I) & " argument(z, 360)",
                   Test_Data (I).Error_Bound); 

            exception
               when Constraint_Error => 
                  Report.Failed ("Constraint_Error raised in test" &
                      Integer'Image (I));
               when others =>
                  Report.Failed ("exception in test" &
                      Integer'Image (I));
            end;
         end loop;
        
	 if Real'Signed_Zeros then
	    begin
	      X := Argument ((-1.0, Real(ImpDef.Annex_G.Negative_Zero)));
	      Check (X, -Pi, "test of arg((-1,-0)", 4.0);
	    exception
	       when others =>
		  Report.Failed ("exception in signed zero test");
	    end;
	 end if;
      end Special_Cases;


      procedure Exception_Cases is
      -- check that Argument_Error is raised if Cycle is <= 0
	 Z : Complex := (1.0, 1.0);
	 X : Real;
	 Y : Real;
      begin
	 begin
	   X := Argument (Z, Cycle => 0.0);
	   Report.Failed ("no exception for cycle = 0.0");
	 exception
	    when Ada.Numerics.Argument_Error => null;
	    when others =>
	       Report.Failed ("wrong exception for cycle = 0.0");
	 end;

	 begin
	   Y := Argument (Z, Cycle => -3.0);
	   Report.Failed ("no exception for cycle < 0.0");
	 exception
	    when Ada.Numerics.Argument_Error => null;
	    when others =>
	       Report.Failed ("wrong exception for cycle < 0.0");
	 end;

         if Report.Ident_Int (2) = 1 then
            -- optimization thwarting code - never executed
            Report.Failed("2=1" & Real'Image (X+Y));
         end if;
      end Exception_Cases;


      procedure Do_Test is
      begin
	 Special_Cases;
	 Exception_Cases;
      end Do_Test;
   end Generic_Check;

   package Chk_Float is new Generic_Check (Float);

   -- check the floating point type with the most digits
   type A_Long_Float is digits System.Max_Digits;
   package Chk_A_Long_Float is new Generic_Check (A_Long_Float);
begin
   Report.Test ("CXG2006",
                "Check the accuracy of the complex argument" &
                " function");

   if Verbose then
      Report.Comment ("checking Standard.Float");
   end if;

   Chk_Float.Do_Test;

   if Verbose then
      Report.Comment ("checking a digits" & 
                      Integer'Image (System.Max_Digits) &
                      " floating point type");
   end if;

   Chk_A_Long_Float.Do_Test;

   Report.Result;
end CXG2006;
