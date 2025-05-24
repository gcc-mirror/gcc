-- CXG2002.A
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
--      Check that the complex "abs" or modulus function returns
--      results that are within the error bound allowed.
--
-- TEST DESCRIPTION:
--      This test uses a generic package to compute and check the
--      values of the modulus function.  In addition, a non-generic
--      copy of this package is used to check the non-generic package
--      Ada.Numerics.Complex_Types.
--      Of special interest is the case where either the real or
--      the imaginary part of the argument is very large while the
--      other part is very small or 0. 
--      We want to check that the value is computed such that
--      an overflow does not occur.  If computed directly from the
--      definition 
--        abs (x+yi) = sqrt(x**2 + y**2)
--      then overflow or underflow is much more likely than if the
--      argument is normalized first.
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
--      31 JAN 96   SAIC    Initial release for 2.1
--      02 JUN 98   EDS     Add parens to intermediate calculations.
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
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Complex_Types;
procedure CXG2002 is
   Verbose : constant Boolean := False;
   Maximum_Relative_Error : constant := 3.0;

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
                       MRE : Real := Maximum_Relative_Error) is
         Rel_Error,
         Abs_Error,
         Max_Error : Real;
      begin
         -- In the case where the expected result is very small or 0
         -- we compute the maximum error as a multiple of Model_Epsilon instead
         -- of Model_Epsilon and Expected.
         Rel_Error := MRE * (abs Expected * Real'Model_Epsilon);
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
                           Real'Image (Expected - Actual) &
                           " max_err:" & Real'Image (Max_Error) );
         elsif Verbose then
	    if Actual = Expected then
	       Report.Comment (Test_Name & "  exact result");
	    else
	       Report.Comment (Test_Name & "  passed");
	    end if;
         end if;
      end Check;


      procedure Do_Test is
         Z : Complex;
         X : Real;
         T : Real;
      begin

         --- test 1 ---
         begin
            T := Real'Safe_Last;
            Z := T + 0.0*i;
            X := abs Z;
            Check (X, T, "test 1 -- abs(bigreal + 0i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 1");
            when others =>
               Report.Failed ("exception in test 1");
         end;

	 --- test 2 ---
         begin
            T := Real'Safe_Last;
	    Z := 0.0 + T*i;
	    X := Modulus (Z);
            Check (X, T, "test 2 -- abs(0 + bigreal*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 2");
            when others =>
               Report.Failed ("exception in test 2");
         end;

	 --- test 3 ---
         begin
	    Z := 3.0 + 4.0*i;
	    X := abs Z;
            Check (X, 5.0 , "test 3 -- abs(3 + 4*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 3");
            when others =>
               Report.Failed ("exception in test 3");
         end;

	 --- test 4 ---
         declare
            S : Real;
         begin
            S := Real(Real'Machine_Radix) ** (Real'Machine_EMax - 3);
	    Z := 3.0 * S + 4.0*S*i;
	    X := abs Z;
            Check (X, 5.0*S, "test 4 -- abs(3S + 4S*i) for large S", 
                   5.0*Real'Model_Epsilon);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 4");
            when others =>
               Report.Failed ("exception in test 4");
         end;

	 --- test 5 --- 
         begin
            T := Real'Model_Small;
	    Z := T + 0.0*i;
	    X := abs Z;
            Check (X, T , "test 5 -- abs(small + 0*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 5");
            when others =>
               Report.Failed ("exception in test 5");
         end;

	 --- test 6 ---
         begin
            T := Real'Model_Small;
	    Z := 0.0 + T*i;
	    X := abs Z;
            Check (X, T , "test 6 -- abs(0 + small*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 6");
            when others =>
               Report.Failed ("exception in test 6");
         end;

	 --- test 7 ---
         declare
            S : Real;
         begin
            S := Real(Real'Machine_Radix) ** (Real'Model_EMin + 3);
	    Z := 3.0 * S + 4.0*S*i;
	    X := abs Z;
            Check (X, 5.0*S, "test 7 -- abs(3S + 4S*i) for small S", 
                   5.0*Real'Model_Epsilon);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 7");
            when others =>
               Report.Failed ("exception in test 7");
         end;

	 --- test 8 ---
         declare
            -- CRC Standard Mathematical Tables;  23rd Edition; pg 738
            Sqrt2 : constant := 
              1.41421_35623_73095_04880_16887_24209_69807_85696_71875_37695;
         begin
	    Z := 1.0 + 1.0*i;
	    X := abs Z;
            Check (X, Sqrt2 , "test 8 -- abs(1 + 1*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 8");
            when others =>
               Report.Failed ("exception in test 8");
         end;

	 --- test 9 --- 
         begin
            T := 0.0;
	    Z := T + 0.0*i;
	    X := abs Z;
            Check (X, T , "test 5 -- abs(0 + 0*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 9");
            when others =>
               Report.Failed ("exception in test 9");
         end;
      end Do_Test;
   end Generic_Check;

   -----------------------------------------------------------------------
   --- non generic copy of the above generic package
   -----------------------------------------------------------------------

   package Non_Generic_Check is
      subtype Real is Float;
      procedure Do_Test;
   end Non_Generic_Check;

   package body Non_Generic_Check is
      use Ada.Numerics.Complex_Types;

      procedure Check (Actual, Expected : Real;
                       Test_Name : String;
                       MRE : Real := Maximum_Relative_Error) is
         Rel_Error,
         Abs_Error,
         Max_Error : Real;
      begin
         -- In the case where the expected result is very small or 0
         -- we compute the maximum error as a multiple of Model_Epsilon instead
         -- of Model_Epsilon and Expected.
         Rel_Error := MRE * (abs Expected * Real'Model_Epsilon);
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
                           Real'Image (Expected - Actual) &
                           " max_err:" & Real'Image (Max_Error) );
         elsif Verbose then
	    if Actual = Expected then
	       Report.Comment (Test_Name & "  exact result");
	    else
	       Report.Comment (Test_Name & "  passed");
	    end if;
         end if;
      end Check;


      procedure Do_Test is
         Z : Complex;
         X : Real;
         T : Real;
      begin

         --- test 1 ---
         begin
            T := Real'Safe_Last;
            Z := T + 0.0*i;
            X := abs Z;
            Check (X, T, "test 1 -- abs(bigreal + 0i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 1");
            when others =>
               Report.Failed ("exception in test 1");
         end;

	 --- test 2 ---
         begin
            T := Real'Safe_Last;
	    Z := 0.0 + T*i;
	    X := Modulus (Z);
            Check (X, T, "test 2 -- abs(0 + bigreal*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 2");
            when others =>
               Report.Failed ("exception in test 2");
         end;

	 --- test 3 ---
         begin
	    Z := 3.0 + 4.0*i;
	    X := abs Z;
            Check (X, 5.0 , "test 3 -- abs(3 + 4*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 3");
            when others =>
               Report.Failed ("exception in test 3");
         end;

	 --- test 4 ---
         declare
            S : Real;
         begin
            S := Real(Real'Machine_Radix) ** (Real'Machine_EMax - 3);
	    Z := 3.0 * S + 4.0*S*i;
	    X := abs Z;
            Check (X, 5.0*S, "test 4 -- abs(3S + 4S*i) for large S", 
                   5.0*Real'Model_Epsilon);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 4");
            when others =>
               Report.Failed ("exception in test 4");
         end;

	 --- test 5 --- 
         begin
            T := Real'Model_Small;
	    Z := T + 0.0*i;
	    X := abs Z;
            Check (X, T , "test 5 -- abs(small + 0*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 5");
            when others =>
               Report.Failed ("exception in test 5");
         end;

	 --- test 6 ---
         begin
            T := Real'Model_Small;
	    Z := 0.0 + T*i;
	    X := abs Z;
            Check (X, T , "test 6 -- abs(0 + small*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 6");
            when others =>
               Report.Failed ("exception in test 6");
         end;

	 --- test 7 ---
         declare
            S : Real;
         begin
            S := Real(Real'Machine_Radix) ** (Real'Model_EMin + 3);
	    Z := 3.0 * S + 4.0*S*i;
	    X := abs Z;
            Check (X, 5.0*S, "test 7 -- abs(3S + 4S*i) for small S", 
                   5.0*Real'Model_Epsilon);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 7");
            when others =>
               Report.Failed ("exception in test 7");
         end;

	 --- test 8 ---
         declare
            -- CRC Standard Mathematical Tables;  23rd Edition; pg 738
            Sqrt2 : constant := 
              1.41421_35623_73095_04880_16887_24209_69807_85696_71875_37695;
         begin
	    Z := 1.0 + 1.0*i;
	    X := abs Z;
            Check (X, Sqrt2 , "test 8 -- abs(1 + 1*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 8");
            when others =>
               Report.Failed ("exception in test 8");
         end;

	 --- test 9 --- 
         begin
            T := 0.0;
	    Z := T + 0.0*i;
	    X := abs Z;
            Check (X, T , "test 5 -- abs(0 + 0*i)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 9");
            when others =>
               Report.Failed ("exception in test 9");
         end;
      end Do_Test;
   end Non_Generic_Check;

   -----------------------------------------------------------------------
   --- end of "manual instantiation" 
   -----------------------------------------------------------------------
   package Chk_Float is new Generic_Check (Float);

   -- check the floating point type with the most digits
   type A_Long_Float is digits System.Max_Digits;
   package Chk_A_Long_Float is new Generic_Check (A_Long_Float);
begin
   Report.Test ("CXG2002",
                "Check the accuracy of the complex modulus" &
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

   if Verbose then
      Report.Comment ("checking non-generic package");
   end if;
   Non_Generic_Check.Do_Test;
   Report.Result;
end CXG2002;
