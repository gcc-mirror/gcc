-- CXG2007.A
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
--      Check that the complex Compose_From_Polar function returns
--      results that are within the error bound allowed.
--      Check that Argument_Error is raised if the Cycle parameter
--      is less than or equal to zero.
--
-- TEST DESCRIPTION:
--      This test uses a generic package to compute and check the
--      values of the Compose_From_Polar function.  
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
--      23 FEB 96   SAIC    Initial release for 2.1
--      23 APR 96   SAIC    Fixed error checking
--      03 MAR 97   PWB.CTA Deleted checks with explicit Cycle => 2.0*Pi
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

with System;
with Report;
with Ada.Numerics;
with Ada.Numerics.Generic_Complex_Types;
procedure CXG2007 is
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

      Maximum_Relative_Error : constant Real := 3.0;

      procedure Check (Actual, Expected : Real;
                       Test_Name : String;
                       MRE : Real;
                       Arg_Error : Real) is
         -- Arg_Error is additional absolute error that is allowed beyond
         -- the MRE to account for error in the result that can be
         -- attributed to error in the arguments.
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
         Max_Error := Max_Error + Arg_Error;

         if abs (Actual - Expected) > Max_Error then
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
                       MRE : Real;
                       Arg_Error : Real) is
         -- Arg_Error is additional absolute error that is allowed beyond
         -- the MRE to account for error in the result that can be
         -- attributed to error in the arguments.
      begin
         Check (Actual.Re, Expected.Re, 
                Test_Name & " real part",
                MRE, Arg_Error);
         Check (Actual.Im, Expected.Im, 
                Test_Name & " imaginary part",
                MRE, Arg_Error);
      end Check;


      procedure Special_Cases is
         type Data_Point is 
            record
               Re,
               Im,
	       Modulus,
               Radians,
               Degrees,
	       Arg_Error : Real;
            end record;

	 -- shorthand names for various constants
	 P4 : constant := Pi/4.0;
	 P6 : constant := Pi/6.0;

	 MER2 : constant Real := Real'Model_Epsilon * Sqrt2;

         type Test_Data_Type is array (Positive range <>) of Data_Point;

         -- the values in the following table only involve static
         -- expressions so no loss of precision occurs.
         Test_Data : constant Test_Data_Type := (
         --Re          Im   Modulus     Radians  Degrees  Arg_Err
         ( 0.0,        0.0, 0.0,            0.0,   0.0,    0.0 ),    -- 1
         ( 0.0,        0.0, 0.0,             Pi, 180.0,    0.0 ),    -- 2
         
         ( 1.0,        0.0, 1.0,            0.0,   0.0,    0.0 ),    -- 3
         (-1.0,        0.0, -1.0,           0.0,   0.0,    0.0 ),    -- 4
         
         ( 1.0,        1.0,  Sqrt2,          P4,  45.0,    MER2),    -- 5
         (-1.0,        1.0, -Sqrt2,         -P4, -45.0,    MER2),    -- 6
         ( 1.0,       -1.0,  Sqrt2,         -P4, -45.0,    MER2),    -- 7
         (-1.0,       -1.0, -Sqrt2,          P4,  45.0,    MER2),    -- 8
         (-1.0,       -1.0,  Sqrt2,     -3.0*P4,-135.0,    MER2),    -- 9 
         (-1.0,        1.0,  Sqrt2,      3.0*P4, 135.0,    MER2),    -- 10
         ( 1.0,       -1.0, -Sqrt2,      3.0*P4, 135.0,    MER2),    -- 11
         
         (-1.0,        0.0, 1.0,             Pi, 180.0,    0.0 ),    -- 12
         ( 1.0,        0.0, -1.0,            Pi, 180.0,    0.0 ) );  -- 13

    
         Z : Complex;
	 Exp : Complex;
      begin
         for I in Test_Data'Range loop
            begin
	       Exp := (Test_Data (I).Re, Test_Data (I).Im);

               Z := Compose_From_Polar (Test_Data (I).Modulus,
					Test_Data (I).Radians);
	       Check (Z, Exp,
                 "test" & Integer'Image (I) & " compose_from_polar(m,r)",
		 Maximum_Relative_Error, Test_Data (I).Arg_Error); 

--pwb-math               Z := Compose_From_Polar (Test_Data (I).Modulus, 
--pwb-math					Test_Data (I).Radians,
--pwb-math					2.0*Pi);
--pwb-math	       Check (Z, Exp,
--pwb-math                 "test" & Integer'Image (I) & " compose_from_polar(m,r,2pi)",
--pwb-math		 Maximum_Relative_Error, Test_Data (I).Arg_Error); 

               Z := Compose_From_Polar (Test_Data (I).Modulus, 
					Test_Data (I).Degrees,
					360.0);
	       Check (Z, Exp,
                 "test" & Integer'Image (I) & " compose_from_polar(m,d,360)",
		 Maximum_Relative_Error, Test_Data (I).Arg_Error);

            exception
               when Constraint_Error => 
                  Report.Failed ("Constraint_Error raised in test" &
                      Integer'Image (I));
               when others =>
                  Report.Failed ("exception in test" &
                      Integer'Image (I));
            end;
         end loop;
      end Special_Cases;


      procedure Exception_Cases is
      -- check that Argument_Error is raised if Cycle is <= 0
	 Z : Complex; 
	 W : Complex; 
      begin
	 begin
	   Z := Compose_From_Polar (3.0, 0.0, Cycle => 0.0);
	   Report.Failed ("no exception for cycle = 0.0");
	 exception
	    when Ada.Numerics.Argument_Error => null;
	    when others =>
	       Report.Failed ("wrong exception for cycle = 0.0");
	 end;

	 begin
	   W := Compose_From_Polar (6.0, 1.0, Cycle => -10.0);
	   Report.Failed ("no exception for cycle < 0.0");
	 exception
	    when Ada.Numerics.Argument_Error => null;
	    when others =>
	       Report.Failed ("wrong exception for cycle < 0.0");
	 end;

	 if Report.Ident_Int (1) = 2 then
	     -- not executed - used to make it appear that we use the
	     -- results of the above computation
	     Z := Z * W;
	     Report.Failed(Real'Image (Z.Re + Z.Im));
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
   Report.Test ("CXG2007",
                "Check the accuracy of the Compose_From_Polar" &
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
end CXG2007;
