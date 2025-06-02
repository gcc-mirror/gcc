-- CXA5A02.A
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
--      Check that the functions Cos and Cosh provide correct results.
--
-- TEST DESCRIPTION:
--      This test examines both the version of Cos and Cosh resulting from
--      the instantiation of the Ada.Numerics.Generic_Elementary_Functions
--      with type derived from type Float, as well as the pre-instantiated 
--      version of this package for type Float.
--      Prescribed results, including instances prescribed to raise 
--      exceptions, are examined in the test cases.  In addition, 
--      certain evaluations are performed where the actual function result 
--      is compared with the expected result (within an epsilon range of 
--      accuracy).
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FXA5A00.A   (foundation code)
--         CXA5A02.A
--
--
-- CHANGE HISTORY:
--      09 Mar 95   SAIC    Initial prerelease version.
--      03 Apr 95   SAIC    Removed reference to derived type.
--      13 Jun 95   SAIC    Incorporated use of Dont_Optimize procedure, and
--                          use of Result_Within_Range function overloaded for
--                          FXA5A00.New_Float_Type.
--      28 Feb 97   PWB.CTA Removed checks specifying Cycle => 2.0 * Pi
--      26 Jun 98   EDS     Protected exception checks by first testing
--                          for 'Machine_Overflows.  Removed code deleted
--                          by comment.
-- CHANGE NOTE:
--      According to Ken Dritz, author of the Numerics Annex of the RM,
--      one should never specify the cycle 2.0*Pi for the trigonometric
--      functions.  In particular, if the machine number for the first
--      argument is not an exact multiple of the machine number for the
--      explicit cycle, then the specified exact results cannot be
--      reasonably expected.  The affected checks have been deleted.
--!

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Generic_Elementary_Functions;
with FXA5A00;
with Report;

procedure CXA5A02 is
begin

   Report.Test ("CXA5A02", "Check that the functions Cos and Cosh provide " &
                           "correct results");

   Test_Block:
   declare

      use Ada.Numerics;
      use FXA5A00;

      package GEF is new Ada.Numerics.Generic_Elementary_Functions(New_Float);
      package  EF renames Ada.Numerics.Elementary_Functions;

      The_Result       : Float;
      New_Float_Result : New_Float;

      procedure Dont_Optimize_Float     is new Dont_Optimize(Float);
      procedure Dont_Optimize_New_Float is new Dont_Optimize(New_Float);

   begin

      -- Testing of Cos Function, both instantiated and pre-instantiated
      -- version.

      -- Check that no exception occurs on computing the Cos with very
      -- large (positive and negative) input values.

      begin 
         New_Float_Result := GEF.Cos (New_Float(FXA5A00.Large));
         Dont_Optimize_New_Float(New_Float_Result, 1);
      exception
         when others =>
           Report.Failed("Unexpected exception on GEF.Cos with large " &
                         "positive value");
      end;

      begin 
         The_Result := EF.Cos (FXA5A00.Minus_Large);
         Dont_Optimize_Float(The_Result, 2);
      exception
         when others =>
           Report.Failed("Unexpected exception on GEF.Cos with large " &
                         "negative value");
      end;


      -- Test of Cos for prescribed result at zero.

      if GEF.Cos (0.0) /= 1.0 or
          EF.Cos (0.0) /= 1.0
      then
         Report.Failed("Incorrect value returned from Cos(0.0)");
      end if;


      -- Test of Cos with expected result value between 1.0 and -1.0.

      if not (Result_Within_Range( EF.Cos(Ada.Numerics.Pi/3.0),
                                          0.500, 
                                          0.001)                       and
              Result_Within_Range(GEF.Cos(0.6166), 0.816, 0.001)       and
              Result_Within_Range(GEF.Cos(0.1949), 0.981, 0.001)       and
              Result_Within_Range( EF.Cos(Ada.Numerics.Pi/2.0),
                                          0.00,
                                          0.001)                       and
              Result_Within_Range( EF.Cos(2.0*Ada.Numerics.Pi/3.0),
                                          -0.500,
                                          0.001)                       and
              Result_Within_Range(GEF.Cos(New_Float(Ada.Numerics.Pi)),
                                          -1.00,
                                           0.001))
      then
         Report.Failed("Incorrect value returned from Cos function when " &
                       "the expected result is between 1.0 and -1.0");
      end if;


      -- Testing of the Cos function with Cycle parameter.

      -- Check that Argument_Error is raised when the value of the Cycle
      -- parameter is zero.

      begin 
         New_Float_Result := GEF.Cos (X => 1.0, Cycle => 0.0);
         Report.Failed("Argument_Error not raised by GEF.Cos function " &
                       "when the Cycle parameter is zero");
         Dont_Optimize_New_Float(New_Float_Result, 3);
      exception
         when Ada.Numerics.Argument_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Unexpected exception raised by GEF.cos function " &
                          "when the Cycle parameter is zero");
      end;

      begin 
         The_Result := EF.Cos (X => 0.55, Cycle => 0.0);
         Report.Failed("Argument_Error not raised by EF.Cos function when " &
                       "the Cycle parameter is zero");
         Dont_Optimize_Float(The_Result, 4);
      exception
         when Ada.Numerics.Argument_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Unexpected exception raised by EF.Cos function " &
                          "when the Cycle parameter is zero");
      end;

      -- Check that Argument_Error is raised when the value of the Cycle
      -- parameter is negative.

      begin 
         New_Float_Result := GEF.Cos (X => 0.45, Cycle => -2.0*Pi);
         Report.Failed("Argument_Error not raised by GEF.Cos function " &
                       "when the Cycle parameter is negative");
         Dont_Optimize_New_Float(New_Float_Result, 5);
      exception
         when Ada.Numerics.Argument_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Unexpected exception raised by GEF.Cos function " &
                          "when the Cycle parameter is negative");
      end;

      begin 
         The_Result := EF.Cos (X => 0.10, Cycle => -Pi/2.0);
         Report.Failed("Argument_Error not raised by EF.Cos function when " &
                       "the Cycle parameter is negative");
         Dont_Optimize_Float(The_Result, 6);
      exception
         when Ada.Numerics.Argument_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Unexpected exception raised by EF.Cos function " &
                          "when the Cycle parameter is negative");
      end;

      -- Test of Cos with Cycle parameter for prescribed result at zero.

      if GEF.Cos (0.0, 360.0) /= 1.0 or
          EF.Cos (0.0, 360.0) /= 1.0
      then
         Report.Failed("Incorrect value returned from Cos function with " &
                       "cycle parameter for a zero input parameter value");
      end if;


      -- Tests of Cos function with specified Cycle, using various input
      -- parameter values for prescribed results.

      if GEF.Cos(0.0,   360.0) /=  1.0 or 
          EF.Cos(360.0, 360.0) /=  1.0 or 
         GEF.Cos(90.0,  360.0) /=  0.0 or 
          EF.Cos(270.0, 360.0) /=  0.0 or 
         GEF.Cos(180.0, 360.0) /= -1.0 or 
          EF.Cos(540.0, 360.0) /= -1.0    
      then
         Report.Failed("Incorrect result from the Cos function with " &
                       "specified cycle for prescribed results");
      end if;



      -- Testing of Cosh Function, both instantiated and pre-instantiated
      -- version.

      -- Test for Constraint_Error on parameter with large positive magnitude.

      begin

         if New_Float'Machine_Overflows then

            New_Float_Result := GEF.Cosh (New_Float(FXA5A00.Large));
            Report.Failed("Constraint_Error not raised when the GEF.Cosh " &
                          "function is provided a parameter with a large " &
                          "positive value");
            Dont_Optimize_New_Float(New_Float_Result, 9);
         end if;

      exception
         when Constraint_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Constraint_Error not raised when the GEF.Cosh " &
                          "function is provided a parameter with a large " &
                          "positive value");
      end;

      -- Test for Constraint_Error on parameter with large negative magnitude.

      begin

         if Float'Machine_Overflows then
            The_Result := EF.Cosh (FXA5A00.Minus_Large);
            Report.Failed("Constraint_Error not raised when the EF.Cosh " &
                          "function is provided a parameter with a "      &
                          "large negative value");
            Dont_Optimize_Float(The_Result, 10);
         end if;

      exception
         when Constraint_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Constraint_Error not raised when the EF.Cosh " &
                          "function is provided a parameter with a "      &
                          "large negative value");
      end;


      -- Test that no exception occurs when the Cosh function is provided a
      -- very small positive or negative value.

      begin 
         New_Float_Result := GEF.Cosh (New_Float(FXA5A00.Small));
         Dont_Optimize_New_Float(New_Float_Result, 11);
      exception
         when others =>
            Report.Failed("Unexpected exception on GEF.Cosh with a very" &
                          "small positive value");
      end;

      begin 
         The_Result := EF.Cosh (-FXA5A00.Small);
         Dont_Optimize_Float(The_Result, 12);
      exception
         when others =>
            Report.Failed("Unexpected exception on EF.Cosh with a very" &
                          "small negative value");
      end;


      -- Test for prescribed 1.0 result of Function Cosh with 0.0 parameter.

      if GEF.Cosh (0.0) /= 1.0 or
          EF.Cosh (0.0) /= 1.0
      then
         Report.Failed("Incorrect value returned from Cosh(0.0)");
      end if;


      -- Test of Cosh function with various input parameters.

      if not FXA5A00.Result_Within_Range(GEF.Cosh(0.24), 1.029, 0.001) or
         not FXA5A00.Result_Within_Range( EF.Cosh(0.59), 1.179, 0.001) or
         not FXA5A00.Result_Within_Range(GEF.Cosh(1.06), 1.616, 0.001) or
         not FXA5A00.Result_Within_Range( EF.Cosh(1.50), 2.352, 0.001) or
         not FXA5A00.Result_Within_Range(GEF.Cosh(1.84), 3.228, 0.001) or
         not FXA5A00.Result_Within_Range( EF.Cosh(3.40), 14.99, 0.01) 
      then
         Report.Failed("Incorrect result from Cosh function with " &
                       "various input parameters");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA5A02;
