-- CXG2010.A
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
--      Check that the exp function returns
--      results that are within the error bound allowed.
--
-- TEST DESCRIPTION:
--      This test contains three test packages that are almost
--      identical.  The first two packages differ only in the 
--      floating point type that is being tested.  The first
--      and third package differ only in whether the generic
--      elementary functions package or the pre-instantiated
--      package is used.
--      The test package is not generic so that the arguments
--      and expected results for some of the test values
--      can be expressed as universal real instead of being
--      computed at runtime.
--
-- SPECIAL REQUIREMENTS
--      The Strict Mode for the numerical accuracy must be
--      selected.  The method by which this mode is selected
--      is implementation dependent.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Numerics Annex and where the Machine_Radix is 2, 4, 8, or 16.
--      This test only applies to the Strict Mode for numerical
--      accuracy.
--
--
-- CHANGE HISTORY:
--       1 Mar 96   SAIC    Initial release for 2.1
--       2 Sep 96   SAIC    Improved check routine 
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

--
-- Notes on derivation of error bound for exp(p)*exp(-p)
--
-- Let a = true value of exp(p) and ac be the computed value.
-- Then a = ac(1+e1), where |e1| <= 4*Model_Epsilon.
-- Similarly, let b = true value of exp(-p) and bc be the computed value.
-- Then b = bc(1+e2), where |e2| <= 4*ME.
-- 
-- The product of x and y is (x*y)(1+e3), where |e3| <= 1.0ME
-- 
-- Hence, the computed ab is [ac(1+e1)*bc(1+e2)](1+e3) =
-- (ac*bc)[1 + e1 + e2 + e3 + e1e2 + e1e3 + e2e3 + e1e2e3).
-- 
-- Throwing away the last four tiny terms, we have (ac*bc)(1 + eta),
-- 
-- where |eta| <= (4+4+1)ME = 9.0Model_Epsilon.

with System;
with Report;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Elementary_Functions;
procedure CXG2010 is
   Verbose : constant Boolean := False;
   Max_Samples : constant := 1000;
   Accuracy_Error_Reported : Boolean := False;

   package Float_Check is
      subtype Real is Float;
      procedure Do_Test;
   end Float_Check;

   package body Float_Check is
      package Elementary_Functions is new 
           Ada.Numerics.Generic_Elementary_Functions (Real);
      function Sqrt (X : Real) return Real renames
           Elementary_Functions.Sqrt;
      function Exp (X : Real) return Real renames
           Elementary_Functions.Exp;


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


      procedure Argument_Range_Check_1 (A, B : Real;
                                        Test : String) is
         -- test a evenly distributed selection of 
         -- arguments selected from the range A to B. 
	 -- Test using identity: EXP(X-V) = EXP(X) * EXP (-V)
	 -- The parameter One_Minus_Exp_Minus_V is the value
	 --   1.0 - Exp (-V) 
	 -- accurate to machine precision.
         -- This procedure is a translation of part of Cody's test 
         X : Real;
         Y : Real;
	 ZX, ZY : Real;
         V : constant := 1.0 / 16.0;
         One_Minus_Exp_Minus_V : constant := 6.058693718652421388E-2;

      begin
         Accuracy_Error_Reported := False;
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            Y := X - V;
	    if Y < 0.0 then 
	       X := Y + V;
	    end if;

	    ZX := Exp (X);
	    ZY := Exp (Y);

	    -- ZX := Exp(X) - Exp(X) * (1 - Exp(-V);
	    -- which simplifies to ZX := Exp (X-V);
	    ZX := ZX - ZX * One_Minus_Exp_Minus_V;

            -- note that since the expected value is computed, we
            -- must take the error in that computation into account.
          Check (ZY, ZX, 
                 "test " & Test & " -" &
                 Integer'Image (I) &
	     	     " exp (" & Real'Image (X) & ")",
                 9.0);
           exit when Accuracy_Error_Reported;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in argument range check 1");
         when others =>
            Report.Failed ("exception in argument range check 1");
      end Argument_Range_Check_1;



      procedure Argument_Range_Check_2 (A, B : Real;
                                        Test : String) is
         -- test a evenly distributed selection of 
         -- arguments selected from the range A to B. 
	 -- Test using identity: EXP(X-V) = EXP(X) * EXP (-V)
	 -- The parameter One_Minus_Exp_Minus_V is the value
	 --   1.0 - Exp (-V) 
	 -- accurate to machine precision.
         -- This procedure is a translation of part of Cody's test 
         X : Real;
         Y : Real;
	 ZX, ZY : Real;
         V : constant := 45.0 / 16.0;
            -- 1/16 - Exp(45/16)
         Coeff : constant := 2.4453321046920570389E-3;

      begin
         Accuracy_Error_Reported := False;
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            Y := X - V;
	    if Y < 0.0 then 
	       X := Y + V;
	    end if;

	    ZX := Exp (X);
	    ZY := Exp (Y);

	    -- ZX := Exp(X) * 1/16 - Exp(X) * Coeff;
            -- where Coeff is 1/16 - Exp(45/16)
	    -- which simplifies to ZX := Exp (X-V);
	    ZX := ZX * 0.0625 - ZX * Coeff;

            -- note that since the expected value is computed, we
            -- must take the error in that computation into account.
          Check (ZY, ZX, 
                 "test " & Test & " -" &
                 Integer'Image (I) &
                 " exp (" & Real'Image (X) & ")",
                 9.0);
           exit when Accuracy_Error_Reported;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in argument range check 2");
         when others =>
            Report.Failed ("exception in argument range check 2");
      end Argument_Range_Check_2;


      procedure Do_Test is
      begin

         --- test 1 ---
         declare
	    Y : Real;
         begin
            Y := Exp(1.0);
            -- normal accuracy requirements
            Check (Y, Ada.Numerics.e, "test 1 -- exp(1)", 4.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 1");
            when others =>
               Report.Failed ("exception in test 1");
         end;

         --- test 2 ---
	 declare
	    Y : Real;
         begin
            Y := Exp(16.0) * Exp(-16.0);
            Check (Y, 1.0, "test 2 -- exp(16)*exp(-16)", 9.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 2");
            when others =>
               Report.Failed ("exception in test 2");
         end;

         --- test 3 ---
	 declare
            Y : Real;
         begin
            Y := Exp (Ada.Numerics.Pi) * Exp (-Ada.Numerics.Pi);
            Check (Y, 1.0, "test 3 -- exp(pi)*exp(-pi)", 9.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 3");
            when others =>
               Report.Failed ("exception in test 3");
         end;

         --- test 4 ---
	 declare
            Y : Real;
         begin
            Y := Exp(0.0);
            Check (Y, 1.0, "test 4 -- exp(0.0)", 
                   0.0);   -- no error allowed
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 4");
            when others =>
               Report.Failed ("exception in test 4");
         end;

         --- test 5 ---
         -- constants used here only have 19 digits of precision
         if Real'Digits > 19 then
            Error_Low_Bound := 0.00000_00000_00000_0001;
            Report.Comment ("exp accuracy checked to 19 digits");
         end if;

         Argument_Range_Check_1 ( 1.0/Sqrt(Real(Real'Machine_Radix)), 
                                  1.0, 
                                  "5");
         Error_Low_Bound := 0.0;  -- reset

	 --- test 6 ---
         -- constants used here only have 19 digits of precision
         if Real'Digits > 19 then
            Error_Low_Bound := 0.00000_00000_00000_0001;
            Report.Comment ("exp accuracy checked to 19 digits");
         end if;

         Argument_Range_Check_2 (1.0, 
                                 Sqrt(Real(Real'Machine_Radix)), 
                                 "6");
         Error_Low_Bound := 0.0;  -- reset

      end Do_Test;
   end Float_Check;

   -----------------------------------------------------------------------
   -----------------------------------------------------------------------
   -- check the floating point type with the most digits
   type A_Long_Float is digits System.Max_Digits;


   package A_Long_Float_Check is
      subtype Real is A_Long_Float;
      procedure Do_Test;
   end A_Long_Float_Check;

   package body A_Long_Float_Check is
      package Elementary_Functions is new 
           Ada.Numerics.Generic_Elementary_Functions (Real);
      function Sqrt (X : Real) return Real renames
           Elementary_Functions.Sqrt;
      function Exp (X : Real) return Real renames
           Elementary_Functions.Exp;


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


      procedure Argument_Range_Check_1 (A, B : Real;
                                        Test : String) is
         -- test a evenly distributed selection of 
         -- arguments selected from the range A to B. 
	 -- Test using identity: EXP(X-V) = EXP(X) * EXP (-V)
	 -- The parameter One_Minus_Exp_Minus_V is the value
	 --   1.0 - Exp (-V) 
	 -- accurate to machine precision.
         -- This procedure is a translation of part of Cody's test 
         X : Real;
         Y : Real;
	 ZX, ZY : Real;
         V : constant := 1.0 / 16.0;
         One_Minus_Exp_Minus_V : constant := 6.058693718652421388E-2;

      begin
         Accuracy_Error_Reported := False;
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            Y := X - V;
	    if Y < 0.0 then 
	       X := Y + V;
	    end if;

	    ZX := Exp (X);
	    ZY := Exp (Y);

	    -- ZX := Exp(X) - Exp(X) * (1 - Exp(-V);
	    -- which simplifies to ZX := Exp (X-V);
	    ZX := ZX - ZX * One_Minus_Exp_Minus_V;

            -- note that since the expected value is computed, we
            -- must take the error in that computation into account.
          Check (ZY, ZX, 
                 "test " & Test & " -" &
                 Integer'Image (I) &
                 " exp (" & Real'Image (X) & ")",
                 9.0);
           exit when Accuracy_Error_Reported;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in argument range check 1");
         when others =>
            Report.Failed ("exception in argument range check 1");
      end Argument_Range_Check_1;



      procedure Argument_Range_Check_2 (A, B : Real;
                                        Test : String) is
         -- test a evenly distributed selection of 
         -- arguments selected from the range A to B. 
	 -- Test using identity: EXP(X-V) = EXP(X) * EXP (-V)
	 -- The parameter One_Minus_Exp_Minus_V is the value
	 --   1.0 - Exp (-V) 
	 -- accurate to machine precision.
         -- This procedure is a translation of part of Cody's test 
         X : Real;
         Y : Real;
	 ZX, ZY : Real;
         V : constant := 45.0 / 16.0;
            -- 1/16 - Exp(45/16)
         Coeff : constant := 2.4453321046920570389E-3;

      begin
         Accuracy_Error_Reported := False;
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            Y := X - V;
	    if Y < 0.0 then 
	       X := Y + V;
	    end if;

	    ZX := Exp (X);
	    ZY := Exp (Y);

	    -- ZX := Exp(X) * 1/16 - Exp(X) * Coeff;
            -- where Coeff is 1/16 - Exp(45/16)
	    -- which simplifies to ZX := Exp (X-V);
	    ZX := ZX * 0.0625 - ZX * Coeff;

            -- note that since the expected value is computed, we
            -- must take the error in that computation into account.
          Check (ZY, ZX, 
                 "test " & Test & " -" &
                 Integer'Image (I) &
                 " exp (" & Real'Image (X) & ")",
                 9.0);
           exit when Accuracy_Error_Reported;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in argument range check 2");
         when others =>
            Report.Failed ("exception in argument range check 2");
      end Argument_Range_Check_2;


      procedure Do_Test is
      begin

         --- test 1 ---
         declare
	    Y : Real;
         begin
            Y := Exp(1.0);
            -- normal accuracy requirements
            Check (Y, Ada.Numerics.e, "test 1 -- exp(1)", 4.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 1");
            when others =>
               Report.Failed ("exception in test 1");
         end;

         --- test 2 ---
	 declare
	    Y : Real;
         begin
            Y := Exp(16.0) * Exp(-16.0);
            Check (Y, 1.0, "test 2 -- exp(16)*exp(-16)", 9.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 2");
            when others =>
               Report.Failed ("exception in test 2");
         end;

         --- test 3 ---
	 declare
            Y : Real;
         begin
            Y := Exp (Ada.Numerics.Pi) * Exp (-Ada.Numerics.Pi);
            Check (Y, 1.0, "test 3 -- exp(pi)*exp(-pi)", 9.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 3");
            when others =>
               Report.Failed ("exception in test 3");
         end;

         --- test 4 ---
	 declare
            Y : Real;
         begin
            Y := Exp(0.0);
            Check (Y, 1.0, "test 4 -- exp(0.0)", 
                   0.0);   -- no error allowed
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 4");
            when others =>
               Report.Failed ("exception in test 4");
         end;

         --- test 5 ---
         -- constants used here only have 19 digits of precision
         if Real'Digits > 19 then
            Error_Low_Bound := 0.00000_00000_00000_0001;
            Report.Comment ("exp accuracy checked to 19 digits");
         end if;

         Argument_Range_Check_1 ( 1.0/Sqrt(Real(Real'Machine_Radix)), 
                                  1.0, 
                                  "5");
         Error_Low_Bound := 0.0;  -- reset

	 --- test 6 ---
         -- constants used here only have 19 digits of precision
         if Real'Digits > 19 then
            Error_Low_Bound := 0.00000_00000_00000_0001;
            Report.Comment ("exp accuracy checked to 19 digits");
         end if;

         Argument_Range_Check_2 (1.0, 
                                 Sqrt(Real(Real'Machine_Radix)), 
                                 "6");
         Error_Low_Bound := 0.0;  -- reset

      end Do_Test;
   end A_Long_Float_Check;

   -----------------------------------------------------------------------
   -----------------------------------------------------------------------

   package Non_Generic_Check is
      procedure Do_Test;
      subtype Real is Float;
   end Non_Generic_Check;

   package body Non_Generic_Check is

      package Elementary_Functions renames 
           Ada.Numerics.Elementary_Functions;
      function Sqrt (X : Real) return Real renames
           Elementary_Functions.Sqrt;
      function Exp (X : Real) return Real renames
           Elementary_Functions.Exp;


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


      procedure Argument_Range_Check_1 (A, B : Real;
                                        Test : String) is
         -- test a evenly distributed selection of 
         -- arguments selected from the range A to B. 
	 -- Test using identity: EXP(X-V) = EXP(X) * EXP (-V)
	 -- The parameter One_Minus_Exp_Minus_V is the value
	 --   1.0 - Exp (-V) 
	 -- accurate to machine precision.
         -- This procedure is a translation of part of Cody's test 
         X : Real;
         Y : Real;
	 ZX, ZY : Real;
         V : constant := 1.0 / 16.0;
         One_Minus_Exp_Minus_V : constant := 6.058693718652421388E-2;

      begin
         Accuracy_Error_Reported := False;
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            Y := X - V;
	    if Y < 0.0 then 
	       X := Y + V;
	    end if;

	    ZX := Exp (X);
	    ZY := Exp (Y);

	    -- ZX := Exp(X) - Exp(X) * (1 - Exp(-V);
	    -- which simplifies to ZX := Exp (X-V);
	    ZX := ZX - ZX * One_Minus_Exp_Minus_V;

            -- note that since the expected value is computed, we
            -- must take the error in that computation into account.
          Check (ZY, ZX, 
                 "test " & Test & " -" &
                 Integer'Image (I) &
                 " exp (" & Real'Image (X) & ")",
                 9.0);
           exit when Accuracy_Error_Reported;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in argument range check 1");
         when others =>
            Report.Failed ("exception in argument range check 1");
      end Argument_Range_Check_1;



      procedure Argument_Range_Check_2 (A, B : Real;
                                        Test : String) is
         -- test a evenly distributed selection of 
         -- arguments selected from the range A to B. 
	 -- Test using identity: EXP(X-V) = EXP(X) * EXP (-V)
	 -- The parameter One_Minus_Exp_Minus_V is the value
	 --   1.0 - Exp (-V) 
	 -- accurate to machine precision.
         -- This procedure is a translation of part of Cody's test 
         X : Real;
         Y : Real;
	 ZX, ZY : Real;
         V : constant := 45.0 / 16.0;
            -- 1/16 - Exp(45/16)
         Coeff : constant := 2.4453321046920570389E-3;

      begin
         Accuracy_Error_Reported := False;
         for I in 1..Max_Samples loop
            X :=  (B - A) * Real (I) / Real (Max_Samples) + A;
            Y := X - V;
	    if Y < 0.0 then 
	       X := Y + V;
	    end if;

	    ZX := Exp (X);
	    ZY := Exp (Y);

	    -- ZX := Exp(X) * 1/16 - Exp(X) * Coeff;
            -- where Coeff is 1/16 - Exp(45/16)
	    -- which simplifies to ZX := Exp (X-V);
	    ZX := ZX * 0.0625 - ZX * Coeff;

            -- note that since the expected value is computed, we
            -- must take the error in that computation into account.
          Check (ZY, ZX, 
                 "test " & Test & " -" &
                 Integer'Image (I) &
                 " exp (" & Real'Image (X) & ")",
                 9.0);
           exit when Accuracy_Error_Reported;
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in argument range check 2");
         when others =>
            Report.Failed ("exception in argument range check 2");
      end Argument_Range_Check_2;


      procedure Do_Test is
      begin

         --- test 1 ---
         declare
	    Y : Real;
         begin
            Y := Exp(1.0);
            -- normal accuracy requirements
            Check (Y, Ada.Numerics.e, "test 1 -- exp(1)", 4.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 1");
            when others =>
               Report.Failed ("exception in test 1");
         end;

         --- test 2 ---
	 declare
	    Y : Real;
         begin
            Y := Exp(16.0) * Exp(-16.0);
            Check (Y, 1.0, "test 2 -- exp(16)*exp(-16)", 9.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 2");
            when others =>
               Report.Failed ("exception in test 2");
         end;

         --- test 3 ---
	 declare
            Y : Real;
         begin
            Y := Exp (Ada.Numerics.Pi) * Exp (-Ada.Numerics.Pi);
            Check (Y, 1.0, "test 3 -- exp(pi)*exp(-pi)", 9.0);
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 3");
            when others =>
               Report.Failed ("exception in test 3");
         end;

         --- test 4 ---
	 declare
            Y : Real;
         begin
            Y := Exp(0.0);
            Check (Y, 1.0, "test 4 -- exp(0.0)", 
                   0.0);   -- no error allowed
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 4");
            when others =>
               Report.Failed ("exception in test 4");
         end;

         --- test 5 ---
         -- constants used here only have 19 digits of precision
         if Real'Digits > 19 then
            Error_Low_Bound := 0.00000_00000_00000_0001;
            Report.Comment ("exp accuracy checked to 19 digits");
         end if;

         Argument_Range_Check_1 ( 1.0/Sqrt(Real(Real'Machine_Radix)), 
                                  1.0, 
                                  "5");
         Error_Low_Bound := 0.0;  -- reset

	 --- test 6 ---
         -- constants used here only have 19 digits of precision
         if Real'Digits > 19 then
            Error_Low_Bound := 0.00000_00000_00000_0001;
            Report.Comment ("exp accuracy checked to 19 digits");
         end if;

         Argument_Range_Check_2 (1.0, 
                                 Sqrt(Real(Real'Machine_Radix)), 
                                 "6");
         Error_Low_Bound := 0.0;  -- reset

      end Do_Test;
   end Non_Generic_Check;

   -----------------------------------------------------------------------
   -----------------------------------------------------------------------

begin
   Report.Test ("CXG2010",
                "Check the accuracy of the exp function"); 

   -- the test only applies to machines with a radix of 2,4,8, or 16
   case Float'Machine_Radix is
      when 2 | 4 | 8 | 16 => null;
      when others =>
	     Report.Not_Applicable ("only applicable to binary radix");
	     Report.Result;
	     return;
   end case;

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

   if Verbose then
      Report.Comment ("checking non-generic package");
   end if;

   Non_Generic_Check.Do_Test;

   Report.Result;
end CXG2010;
