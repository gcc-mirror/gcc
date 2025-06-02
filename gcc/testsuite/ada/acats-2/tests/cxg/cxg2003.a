-- CXG2003.A
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
--      Check that the sqrt function returns
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
--      Numerics Annex.
--      This test only applies to the Strict Mode for numerical
--      accuracy.
--
--
-- CHANGE HISTORY:
--       2 FEB 96   SAIC    Initial release for 2.1
--      18 AUG 96   SAIC    Made Check consistent with other tests.
--
--!

with System;
with Report;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Elementary_Functions;
procedure CXG2003 is
   Verbose : constant Boolean := False;

   package Float_Check is
      subtype Real is Float;
      procedure Do_Test;
   end Float_Check;

   package body Float_Check is
      package Elementary_Functions is new 
           Ada.Numerics.Generic_Elementary_Functions (Real);
      function Sqrt (X : Real) return Real renames
           Elementary_Functions.Sqrt;
      function Log (X : Real) return Real renames
           Elementary_Functions.Log;
      function Exp (X : Real) return Real renames
           Elementary_Functions.Exp;

      -- The default Maximum Relative Error is the value specified
      -- in the LRM.
      Default_MRE : constant Real := 2.0;

      procedure Check (Actual, Expected : Real;
                       Test_Name : String;
                       MRE : Real := Default_MRE) is
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

 
     procedure Argument_Range_Check (A, B : Real;
                                      Test : String) is
         -- test a logarithmically distributed selection of 
         -- arguments selected from the range A to B. 
         X : Real;
         Expected : Real;
         Y : Real;
         C : Real := Log(B/A);
         Max_Samples : constant := 1000;

      begin
         for I in 1..Max_Samples loop
            Expected :=  A * Exp(C * Real (I) / Real (Max_Samples));
            X := Expected * Expected;
            Y := Sqrt (X);

            -- note that since the expected value is computed, we
            -- must take the error in that computation into account.
            Check (Y, Expected, 
                   "test " & Test & " -" &
                   Integer'Image (I) &
                   " of argument range",
                   3.0);
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in argument range check");
         when others =>
            Report.Failed ("exception in argument range check");
      end Argument_Range_Check;

      procedure Do_Test is
      begin

         --- test 1 ---
         declare
            T : constant := (Real'Machine_EMax - 1) / 2;
            X : constant := (1.0 * Real'Machine_Radix) ** (2 * T);
	    Expected : constant := (1.0 * Real'Machine_Radix) ** T;
	    Y : Real;
         begin
            Y := Sqrt (X);
            Check (Y, Expected, "test 1 -- sqrt(radix**((emax-1)/2))");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 1");
            when others =>
               Report.Failed ("exception in test 1");
         end;

         --- test 2 ---
	 declare
            T : constant := (Real'Model_EMin + 1) / 2;
            X : constant := (1.0 * Real'Machine_Radix) ** (2 * T);
	    Expected : constant := (1.0 * Real'Machine_Radix) ** T;
	    Y : Real;
         begin
            Y := Sqrt (X);
            Check (Y, Expected, "test 2 -- sqrt(radix**((emin+1)/2))");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 2");
            when others =>
               Report.Failed ("exception in test 2");
         end;

         --- test 3 ---
	 declare
	    X : constant := 1.0;
	    Expected : constant := 1.0;
            Y : Real;
         begin
            Y := Sqrt(X);
            Check (Y, Expected, "test 3 -- sqrt(1.0)", 
                 0.0);   -- no error allowed
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 3");
            when others =>
               Report.Failed ("exception in test 3");
         end;

         --- test 4 ---
	 declare
	    X : constant := 0.0;
	    Expected : constant := 0.0;
            Y : Real;
         begin
            Y := Sqrt(X);
            Check (Y, Expected, "test 4 -- sqrt(0.0)", 
                 0.0);   -- no error allowed
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 4");
            when others =>
               Report.Failed ("exception in test 4");
         end;

         --- test 5 ---
	 declare
	    X : constant := -1.0;
            Y : Real;
         begin
            Y := Sqrt(X);
            -- the following code should not be executed.
            -- The call to Check is to keep the call to Sqrt from
            -- appearing to be dead code.
            Check (Y, -1.0, "test 5 -- sqrt(-1)" );
            Report.Failed ("test 5 - argument_error expected");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 5");
            when Ada.Numerics.Argument_Error =>
               if Verbose then
                  Report.Comment ("test 5 correctly got argument_error");
               end if;
            when others =>
               Report.Failed ("exception in test 5");
         end;

         --- test 6 ---
	 declare
            X : constant := Ada.Numerics.Pi ** 2;
	    Expected : constant := Ada.Numerics.Pi;
	    Y : Real;
         begin
            Y := Sqrt (X);
            Check (Y, Expected, "test 6 -- sqrt(pi**2)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 6");
            when others =>
               Report.Failed ("exception in test 6");
         end;

         --- test 7 & 8 ---
         Argument_Range_Check (1.0/Sqrt(Real(Real'Machine_Radix)), 
                                1.0, 
                                "7");
         Argument_Range_Check (1.0, 
                                Sqrt(Real(Real'Machine_Radix)), 
                                "8");
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
      function Log (X : Real) return Real renames
           Elementary_Functions.Log;
      function Exp (X : Real) return Real renames
           Elementary_Functions.Exp;

      -- The default Maximum Relative Error is the value specified
      -- in the LRM.
      Default_MRE : constant Real := 2.0;

      procedure Check (Actual, Expected : Real;
                       Test_Name : String;
                       MRE : Real := Default_MRE) is
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


      procedure Argument_Range_Check (A, B : Real;
                                      Test : String) is
         -- test a logarithmically distributed selection of 
         -- arguments selected from the range A to B. 
         X : Real;
         Expected : Real;
         Y : Real;
         C : Real := Log(B/A);
         Max_Samples : constant := 1000;

      begin
         for I in 1..Max_Samples loop
            Expected :=  A * Exp(C * Real (I) / Real (Max_Samples));
            X := Expected * Expected;
            Y := Sqrt (X);

            -- note that since the expected value is computed, we
            -- must take the error in that computation into account.
            Check (Y, Expected, 
                   "test " & Test & " -" &
                   Integer'Image (I) &
                   " of argument range",
                   3.0);
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in argument range check");
         when others =>
            Report.Failed ("exception in argument range check");
      end Argument_Range_Check;


      procedure Do_Test is
      begin

         --- test 1 ---
         declare
            T : constant := (Real'Machine_EMax - 1) / 2;
            X : constant := (1.0 * Real'Machine_Radix) ** (2 * T);
	    Expected : constant := (1.0 * Real'Machine_Radix) ** T;
	    Y : Real;
         begin
            Y := Sqrt (X);
            Check (Y, Expected, "test 1 -- sqrt(radix**((emax-1)/2))");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 1");
            when others =>
               Report.Failed ("exception in test 1");
         end;

         --- test 2 ---
	 declare
            T : constant := (Real'Model_EMin + 1) / 2;
            X : constant := (1.0 * Real'Machine_Radix) ** (2 * T);
	    Expected : constant := (1.0 * Real'Machine_Radix) ** T;
	    Y : Real;
         begin
            Y := Sqrt (X);
            Check (Y, Expected, "test 2 -- sqrt(radix**((emin+1)/2))");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 2");
            when others =>
               Report.Failed ("exception in test 2");
         end;

         --- test 3 ---
	 declare
	    X : constant := 1.0;
	    Expected : constant := 1.0;
            Y : Real;
         begin
            Y := Sqrt(X);
            Check (Y, Expected, "test 3 -- sqrt(1.0)", 
                 0.0);   -- no error allowed
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 3");
            when others =>
               Report.Failed ("exception in test 3");
         end;

         --- test 4 ---
	 declare
	    X : constant := 0.0;
	    Expected : constant := 0.0;
            Y : Real;
         begin
            Y := Sqrt(X);
            Check (Y, Expected, "test 4 -- sqrt(0.0)", 
                 0.0);   -- no error allowed
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 4");
            when others =>
               Report.Failed ("exception in test 4");
         end;

         --- test 5 ---
	 declare
	    X : constant := -1.0;
            Y : Real;
         begin
            Y := Sqrt(X);
            -- the following code should not be executed.
            -- The call to Check is to keep the call to Sqrt from
            -- appearing to be dead code.
            Check (Y, -1.0, "test 5 -- sqrt(-1)" );
            Report.Failed ("test 5 - argument_error expected");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 5");
            when Ada.Numerics.Argument_Error =>
               if Verbose then
                  Report.Comment ("test 5 correctly got argument_error");
               end if;
            when others =>
               Report.Failed ("exception in test 5");
         end;

         --- test 6 ---
	 declare
            X : constant := Ada.Numerics.Pi ** 2;
	    Expected : constant := Ada.Numerics.Pi;
	    Y : Real;
         begin
            Y := Sqrt (X);
            Check (Y, Expected, "test 6 -- sqrt(pi**2)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 6");
            when others =>
               Report.Failed ("exception in test 6");
         end;

         --- test 7 & 8 ---
         Argument_Range_Check (1.0/Sqrt(Real(Real'Machine_Radix)), 
                                1.0, 
                                "7");
         Argument_Range_Check (1.0, 
                                Sqrt(Real(Real'Machine_Radix)), 
                                "8");
      end Do_Test;
   end A_Long_Float_Check;

   -----------------------------------------------------------------------
   -----------------------------------------------------------------------

   package Non_Generic_Check is
      procedure Do_Test;
   end Non_Generic_Check;

   package body Non_Generic_Check is
      package EF renames
           Ada.Numerics.Elementary_Functions;
      subtype Real is Float;

      -- The default Maximum Relative Error is the value specified
      -- in the LRM.
      Default_MRE : constant Real := 2.0;

      procedure Check (Actual, Expected : Real;
                       Test_Name : String;
                       MRE : Real := Default_MRE) is
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



      procedure Argument_Range_Check (A, B : Float;
                                      Test : String) is
         -- test a logarithmically distributed selection of 
         -- arguments selected from the range A to B. 
         X : Float;
         Expected : Float;
         Y : Float;
         C : Float := EF.Log(B/A);
         Max_Samples : constant := 1000;

      begin
         for I in 1..Max_Samples loop
            Expected :=  A * EF.Exp(C * Float (I) / Float (Max_Samples));
            X := Expected * Expected;
            Y := EF.Sqrt (X);

            -- note that since the expected value is computed, we
            -- must take the error in that computation into account.
            Check (Y, Expected, 
                   "test " & Test & " -" &
                   Integer'Image (I) &
                   " of argument range",
                   3.0);
         end loop;
      exception
         when Constraint_Error => 
            Report.Failed 
               ("Constraint_Error raised in argument range check");
         when others =>
            Report.Failed ("exception in argument range check");
      end Argument_Range_Check;


      procedure Do_Test is
      begin

         --- test 1 ---
         declare
            T : constant := (Float'Machine_EMax - 1) / 2;
            X : constant := (1.0 * Float'Machine_Radix) ** (2 * T);
	    Expected : constant := (1.0 * Float'Machine_Radix) ** T;
	    Y : Float;
         begin
            Y := EF.Sqrt (X);
            Check (Y, Expected, "test 1 -- sqrt(radix**((emax-1)/2))");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 1");
            when others =>
               Report.Failed ("exception in test 1");
         end;

         --- test 2 ---
	 declare
            T : constant := (Float'Model_EMin + 1) / 2;
            X : constant := (1.0 * Float'Machine_Radix) ** (2 * T);
	    Expected : constant := (1.0 * Float'Machine_Radix) ** T;
	    Y : Float;
         begin
            Y := EF.Sqrt (X);
            Check (Y, Expected, "test 2 -- sqrt(radix**((emin+1)/2))");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 2");
            when others =>
               Report.Failed ("exception in test 2");
         end;

         --- test 3 ---
	 declare
	    X : constant := 1.0;
	    Expected : constant := 1.0;
            Y : Float;
         begin
            Y := EF.Sqrt(X);
            Check (Y, Expected, "test 3 -- sqrt(1.0)", 
                 0.0);   -- no error allowed
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 3");
            when others =>
               Report.Failed ("exception in test 3");
         end;

         --- test 4 ---
	 declare
	    X : constant := 0.0;
	    Expected : constant := 0.0;
            Y : Float;
         begin
            Y := EF.Sqrt(X);
            Check (Y, Expected, "test 4 -- sqrt(0.0)", 
                 0.0);   -- no error allowed
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 4");
            when others =>
               Report.Failed ("exception in test 4");
         end;

         --- test 5 ---
	 declare
	    X : constant := -1.0;
            Y : Float;
         begin
            Y := EF.Sqrt(X);
            -- the following code should not be executed.
            -- The call to Check is to keep the call to Sqrt from
            -- appearing to be dead code.
            Check (Y, -1.0, "test 5 -- sqrt(-1)" );
            Report.Failed ("test 5 - argument_error expected");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 5");
            when Ada.Numerics.Argument_Error =>
               if Verbose then
                  Report.Comment ("test 5 correctly got argument_error");
               end if;
            when others =>
               Report.Failed ("exception in test 5");
         end;

         --- test 6 ---
	 declare
            X : constant := Ada.Numerics.Pi ** 2;
	    Expected : constant := Ada.Numerics.Pi;
	    Y : Float;
         begin
            Y := EF.Sqrt (X);
            Check (Y, Expected, "test 6 -- sqrt(pi**2)");
         exception
            when Constraint_Error => 
               Report.Failed ("Constraint_Error raised in test 6");
            when others =>
               Report.Failed ("exception in test 6");
         end;

         --- test 7 & 8 ---
         Argument_Range_Check (1.0/EF.Sqrt(Float(Float'Machine_Radix)), 
                                1.0, 
                                "7");
         Argument_Range_Check (1.0, 
                                EF.Sqrt(Float(Float'Machine_Radix)), 
                                "8");
      end Do_Test;
   end Non_Generic_Check;

   -----------------------------------------------------------------------
   -----------------------------------------------------------------------

begin
   Report.Test ("CXG2003",
                "Check the accuracy of the sqrt function"); 

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
end CXG2003;
