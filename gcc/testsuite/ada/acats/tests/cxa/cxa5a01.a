-- CXA5A01.A
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
--      Check that the functions Sin and Sinh provide correct results.
--
-- TEST DESCRIPTION:
--      This test examines both the version of Sin and Sinh resulting from
--      the instantiation of the Ada.Numerics.Generic_Elementary_Functions 
--      with a type derived from type Float, as well as the preinstantiated 
--      version of this package for type Float.
--      Prescribed results, as well as instances prescribed to raise 
--      exceptions, are examined in the test cases.  In addition, 
--      certain evaluations are performed where the actual function result 
--      is compared with the expected result (within an epsilon range of 
--      accuracy).
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FXA5A00.A   (foundation code)
--         CXA5A01.A
--
--
-- CHANGE HISTORY:
--      06 Mar 95   SAIC    Initial prerelease version.
--      13 Jun 95   SAIC    Incorporated use of Dont_Optimize procedure, and
--                          use of Result_Within_Range function overloaded for
--                          FXA5A00.New_Float_Type.
--      26 Jun 98   EDS     Protected exception tests by first testing
--                          for 'Machine_Overflows
--!

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Generic_Elementary_Functions;
with FXA5A00;
with Report;

procedure CXA5A01 is
begin

   Report.Test ("CXA5A01", "Check that the functions Sin and Sinh provide " &
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

      -- Testing of Sin Function, both instantiated and pre-instantiated
      -- version.
      
      -- Check that no exception occurs on computing the Sin with very
      -- large (positive and negative) input values.

      begin 
         New_Float_Result := GEF.Sin (New_Float(FXA5A00.Large));
         Dont_Optimize_New_Float(New_Float_Result, 1);
      exception
         when others =>
           Report.Failed("Unexpected exception on GEF.Sin with large " &
                         "positive value");
      end;

      begin 
         The_Result := EF.Sin (FXA5A00.Minus_Large);
         Dont_Optimize_Float(The_Result, 2);
      exception
         when others =>
           Report.Failed("Unexpected exception on GEF.Sin with large " &
                         "negative value");
      end;


      -- Test of Sin for prescribed result at zero.

      if GEF.Sin (0.0) /= 0.0 or
          EF.Sin (0.0) /= 0.0
      then
         Report.Failed("Incorrect value returned from Sin(0.0)");
      end if;


      -- Test of Sin with expected result value between 0.0 and 1.0.

      if not (GEF.Sin (Ada.Numerics.Pi/4.0) < 1.0)                    or
         not ( EF.Sin (Ada.Numerics.Pi/4.0) < 1.0)                    or
         not FXA5A00.Result_Within_Range(GEF.Sin(0.35), 0.343, 0.001) or
         not FXA5A00.Result_Within_Range( EF.Sin(1.18), 0.924, 0.001)
      then
         Report.Failed("Incorrect value returned from Sin function when " &
                       "the expected result is between 0.0 and 1.0");
      end if;


      -- Test of Sin with expected result value between -1.0 and 0.0.

      if not (GEF.Sin (-Ada.Numerics.Pi/4.0) > -1.0)                    or
         not ( EF.Sin (-Ada.Numerics.Pi/4.0) > -1.0)                    or
         not FXA5A00.Result_Within_Range(GEF.Sin(-0.24), -0.238, 0.001) or
         not FXA5A00.Result_Within_Range( EF.Sin(-1.00), -0.841, 0.001)
      then
         Report.Failed("Incorrect value returned from Sin function when " &
                       "the expected result is between -1.0 and 0.0");
      end if;


      -- Testing of the Sin function with Cycle parameter.
 
      -- Check that Argument_Error is raised when the value of the Cycle
      -- parameter is zero.

      begin 
         New_Float_Result := GEF.Sin (X => 1.0, Cycle => 0.0);
         Report.Failed("Argument_Error not raised by GEF.Sin function " &
                       "when the Cycle parameter is zero");
         Dont_Optimize_New_Float(New_Float_Result, 3);
      exception
         when Ada.Numerics.Argument_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Unexpected exception raised by GEF.Sin function " &
                          "when the Cycle parameter is zero");
      end;

      begin 
         The_Result := EF.Sin (X => 0.34, Cycle => 0.0);
         Report.Failed("Argument_Error not raised by EF.Sin function when " &
                       "the Cycle parameter is zero");
         Dont_Optimize_Float(The_Result, 4);
      exception
         when Ada.Numerics.Argument_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Unexpected exception raised by EF.Sin function " &
                          "when the Cycle parameter is zero");
      end;

      -- Check that Argument_Error is raised when the value of the Cycle
      -- parameter is negative.

      begin 
         New_Float_Result := GEF.Sin (X => 0.45, Cycle => -1.0);
         Report.Failed("Argument_Error not raised by GEF.Sin function " &
                       "when the Cycle parameter is negative");
         Dont_Optimize_New_Float(New_Float_Result, 5);
      exception
         when Ada.Numerics.Argument_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Unexpected exception raised by GEF.Sin function " &
                          "when the Cycle parameter is negative");
      end;

      begin 
         The_Result := EF.Sin (X => 0.10, Cycle => -4.0);
         Report.Failed("Argument_Error not raised by EF.Sin function when " &
                       "the Cycle parameter is negative");
         Dont_Optimize_Float(The_Result, 6);
      exception
         when Ada.Numerics.Argument_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Unexpected exception raised by EF.Sin function " &
                          "when the Cycle parameter is negative");
      end;


      -- Check that no exception occurs on computing the Sin with very
      -- large (positive and negative) input values and Cycle parameter.

      begin 
         New_Float_Result := GEF.Sin (New_Float(FXA5A00.Large), 360.0);
         Dont_Optimize_New_Float(New_Float_Result, 7);
      exception
         when others =>
           Report.Failed("Unexpected exception on GEF.Sin with large " &
                         "positive value and Cycle parameter");
      end;

      begin 
         The_Result := EF.Sin (FXA5A00.Minus_Large, 720.0);
         Dont_Optimize_Float(The_Result, 8);
      exception
         when others =>
           Report.Failed("Unexpected exception on EF.Sin with large " &
                         "negative value and Cycle parameter");
      end;


      -- Test of Sin with Cycle parameter for prescribed result at zero.

      if GEF.Sin (0.0, 360.0) /= 0.0 or
          EF.Sin (0.0, 180.0) /= 0.0
      then
         Report.Failed("Incorrect value returned from Sin function with " &
                       "cycle parameter for a zero input parameter value");
      end if;


      -- Tests of Sin function with Cycle parameter for prescribed results.

      if GEF.Sin(0.0,   360.0) /=  0.0 or 
          EF.Sin(180.0, 360.0) /=  0.0 or
         GEF.Sin(90.0,  360.0) /=  1.0 or 
          EF.Sin(450.0, 360.0) /=  1.0 or 
         GEF.Sin(270.0, 360.0) /= -1.0 or 
          EF.Sin(630.0, 360.0) /= -1.0    
      then
         Report.Failed("Incorrect result from the Sin function with " &
                       "various cycle values for prescribed results");
      end if;


      -- Testing of Sinh Function, both instantiated and pre-instantiated
      -- version.
      
      -- Test for Constraint_Error on parameter with large positive magnitude.

      begin

         if New_Float'Machine_Overflows then
            New_Float_Result := GEF.Sinh (New_Float(FXA5A00.Large));
            Report.Failed("Constraint_Error not raised when the GEF.Sinh " &
                          "function is provided a parameter with a large " &
                          "positive value");
            Dont_Optimize_New_Float(New_Float_Result, 9);
         end if;

      exception
         when Constraint_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Constraint_Error not raised when the GEF.Sinh " &
                          "function is provided a parameter with a large " &
                          "positive value");
      end;

      -- Test for Constraint_Error on parameter with large negative magnitude.

      begin 

         if Float'Machine_Overflows then
            The_Result := EF.Sinh (FXA5A00.Minus_Large);
            Report.Failed("Constraint_Error not raised when the EF.Sinh " &
                          "function is provided a parameter with a "      &
                          "large negative value");
            Dont_Optimize_Float(The_Result, 10);
         end if;

      exception
         when Constraint_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Constraint_Error not raised when the EF.Sinh " &
                          "function is provided a parameter with a "      &
                          "large negative value");
      end;


      -- Test that no exception occurs when the Sinh function is provided a
      -- very small positive or negative value.

      begin 
         New_Float_Result := GEF.Sinh (New_Float(FXA5A00.Small));
         Dont_Optimize_New_Float(New_Float_Result, 11);
      exception
         when others =>
            Report.Failed("Unexpected exception on GEF.Sinh with a very" &
                          "small positive value");
      end;

      begin 
         The_Result := EF.Sinh (-FXA5A00.Small);
         Dont_Optimize_Float(The_Result, 12);
      exception
         when others =>
            Report.Failed("Unexpected exception on EF.Sinh with a very" &
                          "small negative value");
      end;


      -- Test for prescribed 0.0 result of Function Sinh with 0.0 parameter.

      if GEF.Sinh (0.0) /= 0.0 or
          EF.Sinh (0.0) /= 0.0
      then
         Report.Failed("Incorrect value returned from Sinh(0.0)");
      end if;


      -- Test of Sinh function with various input parameters.

      if not FXA5A00.Result_Within_Range(GEF.Sinh(0.01),  0.010, 0.001) or
         not FXA5A00.Result_Within_Range( EF.Sinh(0.61),  0.649, 0.001) or
         not FXA5A00.Result_Within_Range(GEF.Sinh(1.70),  2.65,  0.01) or
         not FXA5A00.Result_Within_Range( EF.Sinh(3.15), 11.65,  0.01) 
      then 
         Report.Failed("Incorrect result returned from Sinh function " &
                       "with various input parameters");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA5A01;
