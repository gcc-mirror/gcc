-- CXG1001.A
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
--      Check that the subprograms defined in the package
--      Ada.Numerics.Generic_Complex_Types provide correct results.  
--      Specifically, check the functions Re, Im (both versions), procedures
--      Set_Re, Set_Im (both versions), functions Compose_From_Cartesian (all
--      versions), Compose_From_Polar, Modulus, Argument, and "abs".
--
-- TEST DESCRIPTION:
--      The generic package Generic_Complex_Types 
--      is instantiated with a real type (new Float), and the results
--      produced by the specified subprograms are verified.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Numerics Annex.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      15 Nov 95   SAIC    Corrected visibility problems for ACVC 2.0.1.
--                          Modified subtest for Compose_From_Polar.
--      29 Sep 96   SAIC    Incorporated reviewer comments.
--
--!

with Ada.Numerics.Generic_Complex_Types;
with Report;

procedure CXG1001 is

begin

   Report.Test ("CXG1001", "Check that the subprograms defined in " &
                           "the package Ada.Numerics.Generic_Complex_Types " & 
                           "provide correct results");

   Test_Block:
   declare

      type Real_Type is new Float;

      package Complex_Pack is new 
        Ada.Numerics.Generic_Complex_Types(Real_Type);

      use type Complex_Pack.Complex;

      -- Declare a zero valued complex number.
      Complex_Zero : constant Complex_Pack.Complex := (0.0, 0.0);

      TC_Complex   : Complex_Pack.Complex := Complex_Zero;
      TC_Imaginary : Complex_Pack.Imaginary;    

   begin

      -- Check that the procedures Set_Re and Set_Im (both versions) provide
      -- correct results.

      declare
         TC_Complex_Real_Field : Complex_Pack.Complex := (5.0, 0.0);
         TC_Complex_Both_Fields : Complex_Pack.Complex := (5.0, 7.0);
      begin

         Complex_Pack.Set_Re(X => TC_Complex, Re => 5.0);

         if TC_Complex /= TC_Complex_Real_Field then
            Report.Failed("Incorrect results from Procedure Set_Re");
         end if;

         Complex_Pack.Set_Im(X => TC_Complex, Im => 7.0);

         if TC_Complex.Re /= 5.0 or
            TC_Complex.Im /= 7.0 or
            TC_Complex    /= TC_Complex_Both_Fields 
         then
            Report.Failed("Incorrect results from Procedure Set_Im " &
                          "with Complex argument");
         end if;

         Complex_Pack.Set_Im(X => TC_Imaginary, Im => 3.0);


         if Complex_Pack.Im(TC_Imaginary) /= 3.0 then
            Report.Failed("Incorrect results returned following the use " &
                          "of Procedure Set_Im with Imaginary argument");
         end if;

      end;


      -- Check that the functions Re and Im (both versions) provide
      -- correct results.

      declare
         TC_Complex_1 : Complex_Pack.Complex := (1.0, 0.0);
         TC_Complex_2 : Complex_Pack.Complex := (0.0, 2.0);
         TC_Complex_3 : Complex_Pack.Complex := (4.0, 3.0);
      begin

         -- Function Re.

         if Complex_Pack.Re(X => TC_Complex_1) /= 1.0  or
            Complex_Pack.Re(X => TC_Complex_2) /= 0.0  or
            Complex_Pack.Re(X => TC_Complex_3) /= 4.0 
         then
            Report.Failed("Incorrect results from Function Re");
         end if;

         -- Function Im; version with Complex argument.

         if Complex_Pack.Im(X => TC_Complex_1) /= 0.0  or
            Complex_Pack.Im(X => TC_Complex_2) /= 2.0  or
            Complex_Pack.Im(X => TC_Complex_3) /= 3.0 
         then
            Report.Failed("Incorrect results from Function Im " &
                          "with Complex argument");
         end if;


         -- Function Im; version with Imaginary argument.

         if Complex_Pack.Im(Complex_Pack.i) /= 1.0  or
            Complex_Pack.Im(Complex_Pack.j) /= 1.0
         then
            Report.Failed("Incorrect results from use of Function Im " &
                          "when used with an Imaginary argument");
         end if;

      end;


      -- Verify the results of the three versions of Function
      -- Compose_From_Cartesian

      declare

         Zero  : constant Real_Type := 0.0;
         Six   : constant Real_Type := 6.0;

         TC_Complex_1 : Complex_Pack.Complex := (3.0, 8.0);
         TC_Complex_2 : Complex_Pack.Complex := (Six, Zero);
         TC_Complex_3 : Complex_Pack.Complex := (Zero, 1.0);

      begin

         TC_Complex := Complex_Pack.Compose_From_Cartesian(3.0, 8.0);

         if TC_Complex /= TC_Complex_1 then
            Report.Failed("Incorrect results from Function " &
                          "Compose_From_Cartesian - 1");
         end if;

         -- If only one component is given, the other component is 
         -- implicitly zero (Both components are set by the following two
         -- function calls).

         TC_Complex := Complex_Pack.Compose_From_Cartesian(Re => 6.0);

         if TC_Complex /= TC_Complex_2 then
            Report.Failed("Incorrect results from Function " &
                          "Compose_From_Cartesian - 2");
         end if;

         TC_Complex := 
           Complex_Pack.Compose_From_Cartesian(Im => Complex_Pack.i);

         if TC_Complex /= TC_Complex_3 then
            Report.Failed("Incorrect results from Function " &
                          "Compose_From_Cartesian - 3");
         end if;

      end; 


      -- Verify the results of Function Compose_From_Polar, Modulus, "abs",
      -- and Argument.

      declare

         use Complex_Pack;

         TC_Modulus,
         TC_Argument : Real_Type := 0.0;


         Angle_0     : constant Real_Type :=   0.0;
         Angle_90    : constant Real_Type :=  90.0;
         Angle_180   : constant Real_Type := 180.0;
         Angle_270   : constant Real_Type := 270.0;
         Angle_360   : constant Real_Type := 360.0;

      begin

         -- Verify the result of Function Compose_From_Polar.
         -- When the value of the parameter Modulus is zero, the 
         -- Compose_From_Polar function yields a result of zero.

         if Compose_From_Polar(0.0, 30.0, 360.0) /= Complex_Zero
         then
            Report.Failed("Incorrect result from Function " &
                          "Compose_From_Polar - 1");
         end if;

         -- When the value of the parameter Argument is equal to a multiple
         -- of the quarter cycle, the result of the Compose_From_Polar
         -- function with specified cycle lies on one of the axes.

         if Compose_From_Polar( 5.0,  Angle_0,   Angle_360) /= (5.0,  0.0) or
            Compose_From_Polar( 5.0,  Angle_90,  Angle_360) /= (0.0,  5.0) or
            Compose_From_Polar(-5.0,  Angle_180, Angle_360) /= (5.0,  0.0) or
            Compose_From_Polar(-5.0,  Angle_270, Angle_360) /= (0.0,  5.0) or
            Compose_From_Polar(-5.0,  Angle_90,  Angle_360) /= (0.0, -5.0) or
            Compose_From_Polar( 5.0,  Angle_270, Angle_360) /= (0.0, -5.0) 
         then
            Report.Failed("Incorrect result from Function " &
                          "Compose_From_Polar - 2");
         end if;

         -- When the parameter to Function Argument represents a point on 
         -- the non-negative real axis, the function yields a zero result.

         if Argument(Complex_Zero, Angle_360) /= 0.0 then
            Report.Failed("Incorrect result from Function Argument");
         end if;

         -- Function Modulus

         if Modulus(Complex_Zero) /= 0.0  or
            Modulus(Compose_From_Polar( 5.0, Angle_90,  Angle_360)) /= 5.0 or
            Modulus(Compose_From_Polar(-5.0, Angle_180, Angle_360)) /= 5.0 
         then
            Report.Failed("Incorrect results from Function Modulus");
         end if;

         -- Function "abs", a rename of Function Modulus.

         if "abs"(Complex_Zero) /= 0.0  or
            "abs"(Compose_From_Polar( 5.0, Angle_90,  Angle_360)) /= 5.0 or
            "abs"(Compose_From_Polar(-5.0, Angle_180, Angle_360)) /= 5.0 
         then
            Report.Failed("Incorrect results from Function abs");
         end if;

      end;

   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXG1001;
