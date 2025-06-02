-- CXA5A06.A
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
--      Check that the functions Arccos and Arccosh provide correct 
--      results.
--
-- TEST DESCRIPTION:
--      This test examines both the version of Arccos and Arccosh 
--      the instantiation of the Ada.Numerics.Generic_Elementary_Functions 
--      with a type derived from type Float, as well as the preinstantiated 
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
--         CXA5A06.A
--
--
-- CHANGE HISTORY:
--      27 Mar 95   SAIC    Initial prerelease version.
--      13 Jun 95   SAIC    Incorporated use of Dont_Optimize procedure, and
--                          use of Result_Within_Range function overloaded for
--                          FXA5A00.New_Float_Type.
--      28 Feb 97   PWB.CTA Removed checks with explicit Cycle => 2.0*Pi
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

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Generic_Elementary_Functions;
with FXA5A00;
with Report;

procedure CXA5A06 is
begin

   Report.Test ("CXA5A06", "Check that the functions Arccos and Arccosh " &
                           "provide correct results");

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

      -- Testing of Arccos Function, both instantiated and pre-instantiated
      -- version.

      -- Check that Argument_Error is raised by the Arccos function when the
      -- absolute value of the input parameter is greater than 1.0.

      begin
         New_Float_Result := GEF.Arccos(New_Float(FXA5A00.One_Plus_Delta));
         Report.Failed("Argument_Error not raised by the Arccos function " &
                       "when the input parameter is greater than 1.0");
         Dont_Optimize_New_Float(New_Float_Result, 1);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Arccos "    &
                          "function when the input parameter is greater " &
                          "than 1.0");
      end;

      begin
         The_Result := EF.Arccos(-FXA5A00.Large);
         Report.Failed("Argument_Error not raised by the Arccos function " &
                       "when the input parameter is a large negative value");
         Dont_Optimize_Float(The_Result, 2);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Arccos " &
                          "function when the input parameter is a "    &
                          "large negative value");
      end;


      -- Check the prescribed results of the Arccos function.

      if GEF.Arccos(X => 1.0) /= 0.0 or
          EF.Arccos(1.0)      /= 0.0
      then
         Report.Failed("Incorrect result returned by the Arccos function " &
                       "when provided a parameter value of 0.0");
      end if;


      -- Check the results of the Arccos function with various input 
      -- parameters.
 
      if not Result_Within_Range(GEF.Arccos(0.77),  0.692,  0.001) or
         not Result_Within_Range( EF.Arccos(0.37),  1.19,   0.01)  or
         not Result_Within_Range(GEF.Arccos(0.0),   Pi/2.0, 0.01)  or
         not Result_Within_Range( EF.Arccos(-0.11), 1.68,   0.01)  or
         not Result_Within_Range(GEF.Arccos(-0.67), 2.31,   0.01)  or
         not Result_Within_Range( EF.Arccos(-0.94), 2.79,   0.01)  or
         not Result_Within_Range(GEF.Arccos(-1.0),  Pi,     0.01) 
      then
         Report.Failed("Incorrect result returned from the Arccos " &
                       "function when provided a variety of input " &
                       "parameters");
      end if;


      -- Testing of the Arccos function with specified Cycle parameter.

      -- Check that Argument_Error is raised by the Arccos function, with
      -- specified Cycle parameter, when the absolute value of the input
      -- parameter is greater than 1.0.

      begin
--pwb-math: Next line: Changed 2.0*Pi to 360.0
         New_Float_Result := GEF.Arccos(New_Float(Large), Cycle => 360.0);
         Report.Failed("Argument_Error not raised by the Arccos function " &
                       "with specified Cycle parameter, when the input "   &
                       "parameter is a large positive value");
         Dont_Optimize_New_Float(New_Float_Result, 3);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Arccos "     &
                          "function with specified Cycle parameter, when " &
                          "the input parameter is a large positive value");
      end;

      begin
--pwb-math: Next line: Changed 2.0*Pi to 360.0
         The_Result := EF.Arccos(FXA5A00.Minus_One_Minus_Delta, 360.0);
         Report.Failed("Argument_Error not raised by the Arccos function " &
                       "with specified Cycle parameter, when the input "   &
                       "parameter is less than -1.0");
         Dont_Optimize_Float(The_Result, 4);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Arccos " &
                          "function with specified Cycle parameter, "  &
                          "when the input parameter is less than -1.0");
      end;


      -- Check that Argument_Error is raised by the Arccos function with
      -- specified cycle when the value of the Cycle parameter is zero or
      -- negative.

      begin
         New_Float_Result := GEF.Arccos(X => 1.0, Cycle => 0.0 );
         Report.Failed("Argument_Error not raised by the Arccos function " &
                       "with specified Cycle parameter, when the Cycle "   &
                       "parameter is 0.0");
         Dont_Optimize_New_Float(New_Float_Result, 5);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Arccos "     &
                          "function with specified Cycle parameter, when " &
                          "the Cycle parameter is 0.0");
      end;

      begin
         The_Result := EF.Arccos(1.0, Cycle => -2.0*Pi);
         Report.Failed("Argument_Error not raised by the Arccos function " &
                       "with specified Cycle parameter, when the Cycle "   &
                       "parameter is negative");
         Dont_Optimize_Float(The_Result, 6);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Arccos "     &
                          "function with specified Cycle parameter, when " &
                          "the Cycle parameter is negative");
      end;

      
      -- Check the prescribed result of the Arccos function with specified
      -- Cycle parameter.

--pwb-math: Next two lines: Changed 2.0*Pi to 360.0
      if GEF.Arccos(X => 1.0, Cycle => 360.0) /= 0.0 or
          EF.Arccos(1.0, 360.0)               /= 0.0
      then 
         Report.Failed("Incorrect result from the Arccos function with " &
                       "specified Cycle parameter, when the input "      &
                       "parameter value is 1.0");
      end if;


      -- Check the results of the Arccos function, with specified Cycle
      -- parameter, with various input parameters.
 
      if --pwb-math not Result_Within_Range(GEF.Arccos( 0.04, 2.0*Pi), 1.53, 0.01) or
--pwb-math         not Result_Within_Range( EF.Arccos( 0.14, 2.0*Pi), 1.43, 0.01) or
--pwb-math         not Result_Within_Range(GEF.Arccos( 0.57, 2.0*Pi), 0.96, 0.01) or
--pwb-math         not Result_Within_Range( EF.Arccos( 0.99, 2.0*Pi), 0.14, 0.01) or
         not Result_Within_Range(GEF.Arccos(-1.0, 360.0), 180.0,  0.1)  or
         not Result_Within_Range(GEF.Arccos(-1.0, 100.0),  50.0,  0.1)  or
         not Result_Within_Range(GEF.Arccos( 0.0, 360.0),  90.0,  0.1)  or
         not Result_Within_Range(GEF.Arccos( 0.0, 100.0),  25.0,  0.1)
      then
         Report.Failed("Incorrect result returned from the Arccos " &
                       "function with specified Cycle parameter, "  &
                       "when provided a variety of input parameters");
      end if;



      -- Testing of Arccosh Function, both instantiated and pre-instantiated
      -- version.

      -- Check that Argument_Error is raised by the Arccosh function when
      -- the value of the parameter X is less than 1.0.

      begin 
         New_Float_Result := GEF.Arccosh(New_Float(FXA5A00.One_Minus_Delta));
         Report.Failed("Argument_Error not raised by the Arccosh function " &
                       "when the parameter value is less than 1.0");
         Dont_Optimize_New_Float(New_Float_Result, 7);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Arccosh " &
                          "function when given a parameter value less " &
                          "than 1.0");
      end;
                    
      begin 
         The_Result := EF.Arccosh(0.0);
         Report.Failed("Argument_Error not raised by the Arccosh function " &
                       "when the parameter value is 0.0");
         Dont_Optimize_Float(The_Result, 8);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Arccosh " &
                          "function when given a parameter value of 0.0");
      end;
                    
      begin 
         New_Float_Result := GEF.Arccosh(New_Float(-FXA5A00.Large));
         Report.Failed("Argument_Error not raised by the Arccosh function " &
                       "when the large negative parameter value");
         Dont_Optimize_New_Float(New_Float_Result, 9);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Arccosh "     &
                          "function when given a large negative parameter " & 
                          "value");
      end;
                    

      -- Check the prescribed results of the Arccosh function.

      if GEF.Arccosh(X => 1.0) /= 0.0 or
          EF.Arccosh(1.0)      /= 0.0
      then
         Report.Failed("Incorrect result returned by the Arccosh " &
                       "function when provided a parameter value of 0.0");
      end if;


      -- Check the results of the Arccosh function with various input 
      -- parameters.
 
      if not Result_Within_Range(GEF.Arccosh(1.03), 0.244, 0.001) or
         not Result_Within_Range( EF.Arccosh(1.28), 0.732, 0.001) or
         not Result_Within_Range(GEF.Arccosh(1.50), 0.962, 0.001) or
         not Result_Within_Range( EF.Arccosh(1.77), 1.17,  0.01)  or
         not Result_Within_Range(GEF.Arccosh(2.00), 1.32,  0.01)  or
         not Result_Within_Range( EF.Arccosh(4.30), 2.14,  0.01)  or
         not Result_Within_Range(GEF.Arccosh(6.90), 2.62,  0.01) 
      then
         Report.Failed("Incorrect result returned from the Arccosh " &
                       "function when provided a variety of input "  &
                       "parameters");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA5A06;
