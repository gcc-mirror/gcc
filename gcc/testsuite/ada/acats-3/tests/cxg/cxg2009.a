-- CXG2009.A
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
--      Check that the real sqrt and complex modulus functions
--      return results that are within the allowed
--      error bound.
--
-- TEST DESCRIPTION:
--      This test checks the accuracy of the sqrt and modulus functions 
--      by computing the norm of various vectors where the result
--      is known in advance.
--      This test uses real and complex math together as would an 
--      actual application.  Considerable use of generics is also
--      employed.
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
--      26 FEB 96   SAIC    Initial release for 2.1
--      22 AUG 96   SAIC    Revised Check procedure
--
--!

------------------------------------------------------------------------------

with System;
with Report;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Elementary_Functions;
procedure CXG2009 is
   Verbose : constant Boolean := False;

   --=====================================================================

   generic
      type Real is digits <>;
   package Generic_Real_Norm_Check is
      procedure Do_Test;
   end Generic_Real_Norm_Check;

   -----------------------------------------------------------------------

   package body Generic_Real_Norm_Check is
      type Vector is array (Integer range <>) of Real;

      package GEF is new Ada.Numerics.Generic_Elementary_Functions (Real);
      function Sqrt (X : Real) return Real renames GEF.Sqrt;

      function One_Norm (V : Vector) return Real is
      -- sum of absolute values of the elements of the vector
	 Result : Real := 0.0;
      begin
	 for I in V'Range loop
	    Result := Result + abs V(I);
	 end loop;
	 return Result;
      end One_Norm;

      function Inf_Norm (V : Vector) return Real is
      -- greatest absolute vector element
	 Result : Real := 0.0;
      begin
	 for I in V'Range loop
	    if abs V(I) > Result then
	       Result := abs V(I);
	    end if;
	 end loop;
	 return Result;
      end Inf_Norm;

      function Two_Norm (V : Vector) return Real is
      -- if greatest absolute vector element is 0 then return 0
      -- else return greatest * sqrt (sum((element / greatest) ** 2)))
      --   where greatest is Inf_Norm of the vector
	 Inf_N : Real;
	 Sum_Squares : Real;
	 Term : Real;
      begin
	 Inf_N := Inf_Norm (V);
	 if Inf_N = 0.0 then
	    return 0.0;
	 end if;
         Sum_Squares := 0.0;
	 for I in V'Range loop
	    Term := V (I) / Inf_N;
	    Sum_Squares := Sum_Squares + Term * Term;
	 end loop;
	 return Inf_N * Sqrt (Sum_Squares);
      end Two_Norm;


      procedure Check (Actual, Expected : Real;
		       Test_Name : String;
		       MRE : Real;
		       Vector_Length : Integer) is
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
	                     "  VectLength:" & 
                           Integer'Image (Vector_Length) &
                           " actual: " & Real'Image (Actual) &
                           " expected: " & Real'Image (Expected) &
                           " difference: " & 
                           Real'Image (Actual - Expected) &
                           " mre:" & Real'Image (Max_Error) );
         elsif Verbose then
            Report.Comment (Test_Name & " vector length" &
                            Integer'Image (Vector_Length));
	  end if;
      end Check;


      procedure Do_Test is
      begin
	 for Vector_Length in 1 .. 10 loop
	    declare
	       V  : Vector (1..Vector_Length) := (1..Vector_Length => 0.0);
	       V1 : Vector (1..Vector_Length) := (1..Vector_Length => 1.0);
	    begin
	       Check (One_Norm (V), 0.0, "one_norm (z)", 0.0, Vector_Length);
	       Check (Inf_Norm (V), 0.0, "inf_norm (z)", 0.0, Vector_Length);

	       for J in 1..Vector_Length loop
		 V := (1..Vector_Length => 0.0);
		 V (J) := 1.0;
	         Check (One_Norm (V), 1.0, "one_norm (010)", 
			0.0, Vector_Length);
	         Check (Inf_Norm (V), 1.0, "inf_norm (010)", 
			0.0, Vector_Length);
	         Check (Two_Norm (V), 1.0, "two_norm (010)", 
			0.0, Vector_Length);
	       end loop;

	       Check (One_Norm (V1), Real (Vector_Length), "one_norm (1)", 
		      0.0, Vector_Length);
	       Check (Inf_Norm (V1), 1.0, "inf_norm (1)", 
		      0.0, Vector_Length);

               -- error in computing Two_Norm and expected result
               -- are as follows  (ME is Model_Epsilon * Expected_Value):
               --   2ME from expected Sqrt
               --   2ME from Sqrt in Two_Norm times the error in the
               --   vector calculation.
               --   The vector calculation contains the following error
               --   based upon the length N of the vector:
               --      N*1ME from squaring terms in Two_Norm
               --      N*1ME from the division of each term in Two_Norm
               --      (N-1)*1ME from the sum of the terms
               -- This gives (2 + 2 * (N + N + (N-1)) ) * ME
               -- which simplifies to (2 + 2N + 2N + 2N - 2) * ME
               -- or 6*N*ME
	       Check (Two_Norm (V1), Sqrt (Real(Vector_Length)), 
                      "two_norm (1)", 
		      (Real (6 * Vector_Length)), 
		      Vector_Length);
	    exception
	       when others => Report.Failed ("exception for vector length" &
				Integer'Image (Vector_Length) );
	    end;
	 end loop;
      end Do_Test;
   end Generic_Real_Norm_Check;

   --=====================================================================

   generic
      type Real is digits <>;
   package Generic_Complex_Norm_Check is
      procedure Do_Test;
   end Generic_Complex_Norm_Check;

   -----------------------------------------------------------------------

   package body Generic_Complex_Norm_Check is
      package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Real);
      use Complex_Types;
      type Vector is array (Integer range <>) of Complex;

      package GEF is new Ada.Numerics.Generic_Elementary_Functions (Real);
      function Sqrt (X : Real) return Real renames GEF.Sqrt;

      function One_Norm (V : Vector) return Real is
	 Result : Real := 0.0;
      begin
	 for I in V'Range loop
	    Result := Result + abs V(I);
	 end loop;
	 return Result;
      end One_Norm;

      function Inf_Norm (V : Vector) return Real is
	 Result : Real := 0.0;
      begin
	 for I in V'Range loop
	    if abs V(I) > Result then
	       Result := abs V(I);
	    end if;
	 end loop;
	 return Result;
      end Inf_Norm;

      function Two_Norm (V : Vector) return Real is
	 Inf_N : Real;
	 Sum_Squares : Real;
	 Term : Real;
      begin
	 Inf_N := Inf_Norm (V);
	 if Inf_N = 0.0 then
	    return 0.0;
	 end if;
         Sum_Squares := 0.0;
	 for I in V'Range loop
	    Term := abs (V (I) / Inf_N );
	    Sum_Squares := Sum_Squares + Term * Term;
	 end loop;
	 return Inf_N * Sqrt (Sum_Squares);
      end Two_Norm;


      procedure Check (Actual, Expected : Real;
		       Test_Name : String;
		       MRE : Real;
		       Vector_Length : Integer) is
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
	                     "  VectLength:" & 
                           Integer'Image (Vector_Length) &
                           " actual: " & Real'Image (Actual) &
                           " expected: " & Real'Image (Expected) &
                           " difference: " & 
                           Real'Image (Actual - Expected) &
                           " mre:" & Real'Image (Max_Error) );
         elsif Verbose then
            Report.Comment (Test_Name & " vector length" &
                            Integer'Image (Vector_Length));
	  end if;
      end Check;


      procedure Do_Test is
      begin
	 for Vector_Length in 1 .. 10 loop
	    declare
	       V  : Vector (1..Vector_Length) := 
                      (1..Vector_Length => (0.0, 0.0));
               X, Y : Vector (1..Vector_Length);
	    begin
	       Check (One_Norm (V), 0.0, "one_norm (z)", 0.0, Vector_Length);
	       Check (Inf_Norm (V), 0.0, "inf_norm (z)", 0.0, Vector_Length);

	       for J in 1..Vector_Length loop
		 X := (1..Vector_Length => (0.0, 0.0) );
                 Y := X;   -- X and Y are now both zeroed
		 X (J).Re := 1.0;
                 Y (J).Im := 1.0;
	         Check (One_Norm (X), 1.0, "one_norm (0x0)", 
			0.0, Vector_Length);
	         Check (Inf_Norm (X), 1.0, "inf_norm (0x0)", 
			0.0, Vector_Length);
	         Check (Two_Norm (X), 1.0, "two_norm (0x0)", 
			0.0, Vector_Length);
	         Check (One_Norm (Y), 1.0, "one_norm (0y0)", 
			0.0, Vector_Length);
	         Check (Inf_Norm (Y), 1.0, "inf_norm (0y0)", 
			0.0, Vector_Length);
	         Check (Two_Norm (Y), 1.0, "two_norm (0y0)", 
			0.0, Vector_Length);
	       end loop;

               V := (1..Vector_Length => (3.0, 4.0));

               -- error in One_Norm is 3*N*ME for abs computation +
               --  (N-1)*ME for the additions
               -- which gives (4N-1) * ME
	       Check (One_Norm (V), 5.0 * Real (Vector_Length), 
		      "one_norm ((3,4))", 
		      Real (4*Vector_Length - 1), 
		      Vector_Length);

               -- error in Inf_Norm is from abs of single element (3ME)
	       Check (Inf_Norm (V), 5.0, 
		      "inf_norm ((3,4))", 
		      3.0, 
		      Vector_Length);

               -- error in following comes from:
               --   2ME in sqrt of expected result
               --   3ME in Inf_Norm calculation
               --   2ME in sqrt of vector calculation
               --   vector calculation has following error
               --      3N*ME for abs
               --       N*ME for squaring
               --       N*ME for division
               --       (N-1)ME for sum
               -- this results in [2 + 3 + 2(6N-1) ] * ME
               -- or (12N + 3)ME
	       Check (Two_Norm (V), 5.0 * Sqrt (Real(Vector_Length)), 
                      "two_norm ((3,4))", 
		      (12.0 * Real (Vector_Length) + 3.0), 
		      Vector_Length);
	    exception
	       when others => Report.Failed ("exception for complex " &
                                             "vector length" &
	                                     Integer'Image (Vector_Length) );
	    end;
	 end loop;
      end Do_Test;
   end Generic_Complex_Norm_Check;

   --=====================================================================

   generic
      type Real is digits <>;
   package Generic_Norm_Check is
      procedure Do_Test;
   end Generic_Norm_Check;

   -----------------------------------------------------------------------

   package body Generic_Norm_Check is
      package RNC is new Generic_Real_Norm_Check (Real);
      package CNC is new Generic_Complex_Norm_Check (Real);
      procedure Do_Test is
      begin
         RNC.Do_Test;
         CNC.Do_Test;
      end Do_Test;
   end Generic_Norm_Check;

   --=====================================================================

   package Float_Check is new Generic_Norm_Check (Float);

   type A_Long_Float is digits System.Max_Digits;
   package A_Long_Float_Check is new Generic_Norm_Check (A_Long_Float);

   -----------------------------------------------------------------------

begin
   Report.Test ("CXG2009",
                "Check the accuracy of the real sqrt and complex " &
                " modulus functions"); 

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
end CXG2009;
