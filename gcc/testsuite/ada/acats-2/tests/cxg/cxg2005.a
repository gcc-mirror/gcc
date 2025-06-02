-- CXG2005.A
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
--      Check that floating point addition and multiplication
--      have the required accuracy.
--
-- TEST DESCRIPTION:
--      The check for the required precision is essentially a
--      check that a guard digit is used for the operations.
--      This test uses a generic package to check the addition
--      and multiplication results.  The
--      generic package is instantiated with the standard FLOAT
--      type and a floating point type for the maximum number
--      of digits of precision.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Numerics Annex.
--
--
-- CHANGE HISTORY:
--      14 FEB 96   SAIC    Initial Release for 2.1
--      16 SEP 99   RLB     Repaired to avoid printing thousands of (almost)
--                          identical failure messages.
--!

-- References:
--
--    Basic Concepts for Computational Software
--    W. J. Cody
--    Problems and Methodologies in Mathematical Software Production
--    editors P. C. Messina and A. Murli
--    Lecture Notes in Computer Science   Vol 142
--    Springer Verlag,  1982
--
--    Software Manual for the Elementary Functions
--    William J. Cody and William Waite
--    Prentice-Hall, 1980
--

with System;
with Report;
procedure CXG2005 is
   Verbose : constant Boolean := False;

   generic
      type Real is digits <>;
   package Guard_Digit_Check is
      procedure Do_Test;
   end Guard_Digit_Check;

   package body Guard_Digit_Check is
      -- made global so that the compiler will be more likely
      -- to keep the values in memory instead of in higher
      -- precision registers.
      X, Y, Z : Real;
      OneX : Real;
      Eps, BN : Real;

      -- special constants - not declared as constants so that
      -- the "stored" precision will be used instead of a "register"
      -- precision.
      Zero : Real := 0.0;
      One  : Real := 1.0;
      Two  : Real := 2.0;

      Failure_Count : Natural := 0;

      procedure Thwart_Optimization is
      -- the purpose of this procedure is to reference the
      -- global variables used by the test so
      -- that the compiler is not likely to keep them in
      -- a higher precision register for their entire lifetime.
      begin
	 if Report.Ident_Bool (False) then
	    -- never executed
	    X := X + 5.0;
	    Y := Y + 6.0;
	    Z := Z + 1.0;
	    Eps := Eps + 2.0;
	    BN := BN + 2.0;
            OneX := X + Y;
            One := 12.34;   Two := 56.78;  Zero := 90.12;
	 end if;
      end Thwart_Optimization;


      procedure Addition_Test is
      begin
         for K in 1..10 loop
	    Eps := Real (K) * Real'Model_Epsilon;
	    for N in 1.. Real'Machine_EMax - 1 loop
	       BN := Real(Real'Machine_Radix) ** N;
	       X := (One + Eps) * BN;
	       Y := (One - Eps) * BN;
	       Z := X - Y; -- true value for Z is 2*Eps*BN

	       if Z /= Eps*BN + Eps*BN then
		  Report.Failed ("addition check failed.  K=" &
		     Integer'Image (K) &
		     "  N=" & Integer'Image (N) &
		     "  difference=" & Real'Image (Z - 2.0*Eps*BN) &
		     "  Eps*BN=" & Real'Image (Eps*BN) );
                  Failure_Count := Failure_Count + 1;
                  exit when Failure_Count > K*4; -- Avoid displaying dozens of messages.
	       end if;
	    end loop;
	 end loop;
      exception
	 when others =>
	    Thwart_Optimization;
            Report.Failed ("unexpected exception in addition test");
      end Addition_Test;


      procedure Multiplication_Test is
      begin
	  X := Real (Real'Machine_Radix) ** (Real'Machine_EMax - 1);
	  OneX := One * X;
	  Thwart_Optimization;
	  if OneX /= X then
	     Report.Failed ("multiplication for large values");
	  end if;

	  X := Real (Real'Machine_Radix) ** (Real'Model_EMin + 1);
	  OneX := One * X;
	  Thwart_Optimization;
	  if OneX /= X then
	     Report.Failed ("multiplication for small values");
	  end if;

	  -- selection of "random" values between 1/radix and radix
          Y := One / Real (Real'Machine_Radix);
          Z := Real(Real'Machine_Radix) - One/Real(Real'Machine_Radix);
	  for I in 0..100 loop
	     X := Y + Real (I) / 100.0 * Z;
	     OneX := One * X;
	     Thwart_Optimization;
	     if OneX /= X then
                Report.Failed ("multiplication for case" & Integer'Image (I));
                exit when Failure_Count > 40+8; -- Avoid displaying dozens of messages.
	     end if;
	  end loop;
      exception
	 when others =>
	    Thwart_Optimization;
            Report.Failed ("unexpected exception in multiplication test");
      end Multiplication_Test;


      procedure Do_Test is
      begin
         Addition_Test;
         Multiplication_Test;
      end Do_Test;
   end Guard_Digit_Check;

   package Chk_Float is new Guard_Digit_Check (Float);

   -- check the floating point type with the most digits
   type A_Long_Float is digits System.Max_Digits;
   package Chk_A_Long_Float is new Guard_Digit_Check (A_Long_Float);
begin
   Report.Test ("CXG2005",
                "Check the accuracy of floating point" &
                " addition and multiplication");

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
end CXG2005;
