-- CXA5A07.A
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
--      Check that the function Arctan provides correct results.
--
-- TEST DESCRIPTION:
--      This test examines both the version of Arctan resulting from the 
--      instantiation of the Ada.Numerics.Generic_Elementary_Functions with 
--      a type derived from type Float, as well as the preinstantiated 
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
--         CXA5A07.A
--
--
-- CHANGE HISTORY:
--      04 Apr 95   SAIC    Initial prerelease version.
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

procedure CXA5A07 is
begin

   Report.Test ("CXA5A07", "Check that the Arctan function provides " &
                           "correct results");

   Test_Block:
   declare

      use Ada.Numerics;
      use FXA5A00;

      package GEF is new Ada.Numerics.Generic_Elementary_Functions(New_Float);
      package  EF renames Ada.Numerics.Elementary_Functions;

      Float_Result     : Float;
      New_Float_Result : New_Float;

      procedure Dont_Optimize_Float     is new Dont_Optimize(Float);
      procedure Dont_Optimize_New_Float is new Dont_Optimize(New_Float);

   begin

      -- Testing of Arctan Function, both instantiated and pre-instantiated
      -- version.

      -- Check that Argument_Error is raised by the Arctan function when
      -- provided parameter values of 0.0, 0.0.

      begin
         New_Float_Result := GEF.Arctan(Y => 0.0, X => 0.0);
         Report.Failed("Argument_Error not raised when the Arctan " &
                       "function is provided input of 0.0, 0.0");
         Dont_Optimize_New_Float(New_Float_Result, 1);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Incorrect exception raised by the Arctan " &
                          "function when provided 0.0, 0.0 input parameters");
      end;


      -- Check that no exception is raised by the Arctan function when
      -- provided a large positive or negative Y parameter value, when 
      -- using the default value for parameter X.

      begin
         Float_Result := EF.Arctan(Y => FXA5A00.Large);
         Dont_Optimize_Float(Float_Result, 2);
      exception
         when others => 
            Report.Failed("Exception raised when the Arctan function is " &
                          "provided a large positive Y parameter value");
      end;

      begin
         New_Float_Result := GEF.Arctan(Y => New_Float(-FXA5A00.Large));
         Dont_Optimize_New_Float(New_Float_Result, 3);
      exception
         when others => 
            Report.Failed("Exception raised when the Arctan function is " &
                          "provided a large negative Y parameter value");
      end;


      -- Check that no exception is raised by the Arctan function when
      -- provided a small positive or negative Y parameter value, when
      -- using the default value for parameter X.

      begin
         Float_Result := EF.Arctan(Y => FXA5A00.Small);
         Dont_Optimize_Float(Float_Result, 4);
      exception
         when others => 
            Report.Failed("Exception raised when the Arctan function is " &
                          "provided a small positive Y parameter value");
      end;

      begin
         New_Float_Result := GEF.Arctan(Y => New_Float(-FXA5A00.Small));
         Dont_Optimize_New_Float(New_Float_Result, 5);
      exception
         when others => 
            Report.Failed("Exception raised when the Arctan function is " &
                          "provided a small negative Y parameter value");
      end;


      -- Check that no exception is raised by the Arctan function when
      -- provided combinations of large and small positive or negative 
      -- parameter values for both Y and X input parameters. 

      begin
         Float_Result := EF.Arctan(Y => FXA5A00.Large, X => FXA5A00.Large);
         Dont_Optimize_Float(Float_Result, 6);
      exception
         when others => 
            Report.Failed("Exception raised when the Arctan function is " &
                          "provided large positive X and Y parameter values");
      end;

      begin
         New_Float_Result := GEF.Arctan(New_Float(-FXA5A00.Large), 
                                        X => New_Float(FXA5A00.Small));
         Dont_Optimize_New_Float(New_Float_Result, 7);
      exception
         when others => 
            Report.Failed("Exception raised when the Arctan function is " &
                          "provided a large negative Y parameter value "  &
                          "and a small positive X parameter value");
      end;


      begin
         Float_Result := EF.Arctan(Y => FXA5A00.Small, X => FXA5A00.Large);
         Dont_Optimize_Float(Float_Result, 8);
      exception
         when others => 
            Report.Failed("Exception raised when the Arctan function is " &
                          "provided a small positive Y parameter value "  &
                          "and a large positive X parameter value");
      end;

      begin
         New_Float_Result := GEF.Arctan(New_Float(-FXA5A00.Small), 
                                        New_Float(-FXA5A00.Large));
         Dont_Optimize_New_Float(New_Float_Result, 9);
      exception
         when others => 
            Report.Failed("Exception raised when the Arctan function is " &
                          "provided a small negative Y parameter value "  &
                          "and a large negative parameter value");
      end;


      -- Check that when the Arctan function is provided a Y parameter value
      -- of 0.0 and a positive X parameter input value, the prescribed result
      -- of zero is returned.

      if GEF.Arctan(Y => 0.0)                     /= 0.0 or -- Default X value
          EF.Arctan(Y => 0.0, X => FXA5A00.Large) /= 0.0 or
--pwb-math: Next line: changed 2.0*Pi to 360.0
         GEF.Arctan(0.0, 360.0)                  /= 0.0 or
          EF.Arctan(0.0, FXA5A00.Small)           /= 0.0
      then 
         Report.Failed("Incorrect results from the Arctan function when " &
                       "provided a Y parameter value of 0.0 and various " &
                       "positive X parameter values");
      end if;


      -- Check that the Arctan function provides correct results when provided
      -- a variety of Y parameter values.  

      if not FXA5A00.Result_Within_Range(EF.Arctan(Pi),    1.26,  0.01)  or
         not FXA5A00.Result_Within_Range(EF.Arctan(-Pi),  -1.26,  0.01)  or
         not FXA5A00.Result_Within_Range(GEF.Arctan(1.0),  0.785, 0.001) or
         not FXA5A00.Result_Within_Range(EF.Arctan(-1.0), -0.785, 0.001) or
         not FXA5A00.Result_Within_Range(GEF.Arctan(0.25), 0.245, 0.001) or
         not FXA5A00.Result_Within_Range(EF.Arctan(0.92),  0.744, 0.001) 
      then
         Report.Failed("Incorrect results from the Arctan function when " &
                       "provided a variety of Y parameter values");
      end if;



      -- Check the results of the Arctan function with specified cycle
      -- parameter.

      -- Check that the Arctan function with specified Cycle parameter 
      -- raises Argument_Error when the value of the Cycle parameter is zero
      -- or negative.

      begin
         Float_Result := EF.Arctan(Y => Pi, Cycle => 0.0);  -- Default X value
         Report.Failed("Argument_Error not raised by the Arctan function " &
                       "with default X parameter value, when the Cycle "   &
                       "parameter is 0.0");
         Dont_Optimize_Float(Float_Result, 10);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Incorrect exception raised by the Arctan "      &
                          "function with default X parameter value, when " &
                          "provided a 0.0 cycle parameter value");
      end;

      begin
         New_Float_Result := GEF.Arctan(Y => Pi, X => 1.0, Cycle => 0.0);
         Report.Failed("Argument_Error not raised by the Arctan function " &
                       "when the Cycle parameter is 0.0");
         Dont_Optimize_New_Float(New_Float_Result, 11);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Incorrect exception raised by the Arctan "     &
                          "function when provided a 0.0 cycle parameter " &
                          "value");
      end;

      begin
         Float_Result := EF.Arctan(Y => Pi, Cycle => -360.0); 
         Report.Failed("Argument_Error not raised by the Arctan function " &
                       "with a default X parameter value, when the Cycle " &
                       "parameter is -360.0");
         Dont_Optimize_Float(Float_Result, 12);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Incorrect exception raised by the Arctan "        &
                          "function with a default X parameter value, when " & 
                          "provided a -360.0 cycle parameter value");
      end;

      begin
         New_Float_Result := GEF.Arctan(Y => Pi, X => 1.0, Cycle => -Pi);
         Report.Failed("Argument_Error not raised by the Arctan function " &
                       "when the Cycle parameter is -Pi");
         Dont_Optimize_New_Float(New_Float_Result, 13);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Incorrect exception raised by the Arctan "     &
                          "function when provided a -Pi cycle parameter " &
                          "value");
      end;


      -- Check that no exception is raised by the Arctan function with 
      -- specified Cycle parameter, when provided large and small positive
      -- or negative parameter values for both Y and X input parameters. 

      begin
         Float_Result := EF.Arctan(Y     => -FXA5A00.Large, 
                                   X     => -FXA5A00.Large,
--pwb-math: Next line: changed 2.0*Pi to 360.0
                                   Cycle => 360.0);
         Dont_Optimize_Float(Float_Result, 14);
      exception
         when others =>  
            Report.Failed("Exception raised when the Arctan function with " &
                          "specified Cycle parameter, when provided large " & 
                          "negative X and Y parameter values");
      end;


      begin
         New_Float_Result := GEF.Arctan(New_Float(FXA5A00.Large), 
                                        X     => New_Float(-FXA5A00.Small),
--pwb-math: Next line: changed 2.0*Pi to 360.0
                                        Cycle => 360.0);
         Dont_Optimize_New_Float(New_Float_Result, 15);
      exception
         when others => 
            Report.Failed("Exception raised when the Arctan function with "  &
                          "specified Cycle parameter, when provided large "  & 
                          "positive Y parameter value and a small negative " &
                          "X parameter value");
      end;


      begin
         Float_Result := EF.Arctan(Y     => -FXA5A00.Small, 
                                   X     => -FXA5A00.Large,
--pwb-math: Next line: changed 2.0*Pi to 360.0
                                   Cycle => 360.0);
         Dont_Optimize_Float(Float_Result, 16);
      exception
         when others => 
            Report.Failed("Exception raised when the Arctan function with "  &
                          "specified Cycle parameter, when provided large "  & 
                          "negative Y parameter value and a large negative " &
                          "X parameter value");
      end;

      begin
         New_Float_Result := GEF.Arctan(New_Float(FXA5A00.Small), 
                                        New_Float(FXA5A00.Large), 
--pwb-math: Next line: changed 2.0*Pi to 360.0
                                        360.0);
         Dont_Optimize_New_Float(New_Float_Result, 17);
      exception
         when others => 
            Report.Failed("Exception raised when the Arctan function with " &
                          "specified Cycle parameter, when provided a "     & 
                          "small negative Y parameter value and a large "   &
                          "positive X parameter value");
      end;


      -- Check that the Arctan function with specified Cycle parameter 
      -- provides correct results when provided a variety of Y parameter
      -- input values.

--pwb-math      if not FXA5A00.Result_Within_Range(EF.Arctan(Pi,    Cycle => 2.0*Pi),
--pwb-math                                         1.26, 
--pwb-math                                         0.01)                            or
--pwb-math         not FXA5A00.Result_Within_Range(EF.Arctan(-Pi,   Cycle => 2.0*Pi),
--pwb-math                                         -1.26,
--pwb-math                                         0.01)                            or
--pwb-math         not FXA5A00.Result_Within_Range(GEF.Arctan(1.0,  Cycle => 2.0*Pi),
--pwb-math                                         0.785,
--pwb-math                                         0.001)                           or
--pwb-math         not FXA5A00.Result_Within_Range(EF.Arctan(-1.0,  Cycle => 2.0*Pi),
--pwb-math                                         -0.785, 
--pwb-math                                         0.001)                           or
--pwb-math         not FXA5A00.Result_Within_Range(GEF.Arctan(0.16, Cycle => 2.0*Pi),
--pwb-math                                         0.159,
--pwb-math                                         0.001)                           or
--pwb-math         not FXA5A00.Result_Within_Range(EF.Arctan(1.0,   Cycle => 360.0),
--pwb-math                                         45.0,
--pwb-math                                         0.1)                             or
--pwb-math         not FXA5A00.Result_Within_Range(GEF.Arctan(1.0,  Cycle => 100.0),
--pwb-math                                         12.5,
--pwb-math                                         0.1) 

--pwb-math  Next 12 lines are replacements for 21 commented lines above
      if not FXA5A00.Result_Within_Range(GEF.Arctan(1.0,  Cycle => 2.0*180.0),
                                         45.0,
                                         0.001)                           or
         not FXA5A00.Result_Within_Range(EF.Arctan(-1.0,  Cycle => 2.0*180.0),
                                         -45.0, 
                                         0.001)                           or
         not FXA5A00.Result_Within_Range(EF.Arctan(1.0,   Cycle => 360.0),
                                         45.0,
                                         0.1)                             or
         not FXA5A00.Result_Within_Range(GEF.Arctan(1.0,  Cycle => 100.0),
                                         12.5,
                                         0.1) 
      then
         Report.Failed("Incorrect results from the Arctan function with "   &
                       "specified Cycle parameter when provided a variety " &
                       "of Y parameter values");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA5A07;
