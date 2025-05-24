-- CXA5A08.A
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
--      Check that the function Arccot provides correct results.
--
-- TEST DESCRIPTION:
--      This test examines both the version of Arccot resulting from the 
--      instantiation of the Ada.Numerics.Generic_Elementary_Functions 
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
--         CXA5A08.A
--
--
-- CHANGE HISTORY:
--      06 Apr 95   SAIC    Initial prerelease version.
--      13 Jun 95   SAIC    Incorporated use of Dont_Optimize procedure, and
--                          use of Result_Within_Range function overloaded for
--                          FXA5A00.New_Float_Type.
--      18 Apr 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      28 Feb 97   CTA.PWB Removed checks with explicit Cycle => 2.0*Pi
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

with Ada.Exceptions;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Generic_Elementary_Functions;
with FXA5A00;
with Report;

procedure CXA5A08 is
begin

   Report.Test ("CXA5A08", "Check that the Arccot function provides " &
                           "correct results");

   Test_Block:
   declare

      use Ada.Exceptions;
      use Ada.Numerics;
      use FXA5A00;

      package GEF is new Ada.Numerics.Generic_Elementary_Functions(New_Float);
      package  EF renames Ada.Numerics.Elementary_Functions;

      Float_Result      : Float;
      Angle             : Float;
      New_Float_Result  : New_Float;
      New_Float_Angle   : New_Float;
      Incorrect_Inverse : Boolean := False;

      procedure Dont_Optimize_Float     is new Dont_Optimize(Float);
      procedure Dont_Optimize_New_Float is new Dont_Optimize(New_Float);

   begin

      -- Testing of Arccot Function, both instantiated and pre-instantiated
      -- version.

      -- Check that Argument_Error is raised by the Arccot function when
      -- provided parameter values of 0.0, 0.0.

      begin
         New_Float_Result := GEF.Arccot(X => 0.0, Y => 0.0);
         Report.Failed("Argument_Error not raised when the Arccot " &
                       "function is provided input of 0.0, 0.0");
         Dont_Optimize_New_Float(New_Float_Result, 1);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Incorrect exception raised by the Arccot " &
                          "function when provided 0.0, 0.0 input parameters");
      end;


      -- Check that no exception is raised by the Arccot function when
      -- provided a large positive or negative X parameter value, when 
      -- using the default value for parameter Y.

      begin
         Float_Result := EF.Arccot(X => FXA5A00.Large);
         Dont_Optimize_Float(Float_Result, 2);
      exception
         when others => 
            Report.Failed("Exception raised when the Arccot function is " &
                          "provided a large positive X parameter value");
      end;

      begin
         New_Float_Result := GEF.Arccot(X => New_Float(-FXA5A00.Large));
         Dont_Optimize_New_Float(New_Float_Result, 3);
      exception
         when others => 
            Report.Failed("Exception raised when the Arccot function is " &
                          "provided a large negative X parameter value");
      end;


      -- Check that no exception is raised by the Arccot function when
      -- provided a small positive or negative X parameter value, when
      -- using the default value for parameter Y.

      begin
         Float_Result := EF.Arccot(X => FXA5A00.Small);
         Dont_Optimize_Float(Float_Result, 4);
      exception
         when others => 
            Report.Failed("Exception raised when the Arccot function is " &
                          "provided a small positive X parameter value");
      end;

      begin
         New_Float_Result := GEF.Arccot(X => New_Float(-FXA5A00.Small));
         Dont_Optimize_New_Float(New_Float_Result, 5);
      exception
         when others => 
            Report.Failed("Exception raised when the Arccot function is " &
                          "provided a small negative X parameter value");
      end;


      -- Check that no exception is raised by the Arccot function when
      -- provided combinations of large and small positive or negative 
      -- parameter values for both X and Y input parameters. 

      begin
         Float_Result := EF.Arccot(X => FXA5A00.Large, Y => FXA5A00.Large);
         Dont_Optimize_Float(Float_Result, 6);
      exception
         when others => 
            Report.Failed("Exception raised when the Arccot function is " &
                          "provided large positive X and Y parameter values");
      end;

      begin
         New_Float_Result := GEF.Arccot(New_Float(-FXA5A00.Large), 
                                        Y => New_Float(FXA5A00.Small));
         Dont_Optimize_New_Float(New_Float_Result, 7);
      exception
         when others => 
            Report.Failed("Exception raised when the Arccot function is " &
                          "provided a large negative X parameter value "  &
                          "and a small positive Y parameter value");
      end;


      begin
         Float_Result := EF.Arccot(X => FXA5A00.Small, Y => FXA5A00.Large);
         Dont_Optimize_Float(Float_Result, 8);
      exception
         when others => 
            Report.Failed("Exception raised when the Arccot function is " &
                          "provided a small positive X parameter value "  &
                          "and a large positive Y parameter value");
      end;

      begin
         New_Float_Result := GEF.Arccot(New_Float(-FXA5A00.Small), 
                                        New_Float(-FXA5A00.Large));
         Dont_Optimize_New_Float(New_Float_Result, 9);
      exception
         when others => 
            Report.Failed("Exception raised when the Arccot function is " &
                          "provided a small negative X parameter value "  &
                          "and a large negative Y parameter value");
      end;


      -- Check that when the Arccot function is provided a Y parameter value
      -- of 0.0 and a positive X parameter input value, the prescribed result
      -- of zero is returned.

      if  EF.Arccot(X => FXA5A00.Large, Y => 0.0)                 /= 0.0 or
         GEF.Arccot(2.0*Pi, Y => 0.0)                             /= 0.0 or
          EF.Arccot(FXA5A00.Small, 0.0)                           /= 0.0 or
          EF.Arccot(X => FXA5A00.Large, Y => 0.0, Cycle => 360.0) /= 0.0 or
         GEF.Arccot(2.0*Pi, Y => 0.0, Cycle => 360.0)             /= 0.0 or
          EF.Arccot(FXA5A00.Small, 0.0, Cycle => 360.0)           /= 0.0
      then 
         Report.Failed("Incorrect results from the Arccot function when " &
                       "provided a Y parameter value of 0.0 and various " &
                       "positive X parameter values");
      end if;


      -- Check that the Arccot function provides correct results when
      -- provided a variety of X parameter values.  

      if not Result_Within_Range( EF.Arccot( 1.0), Pi/4.0, 0.001)     or
         not Result_Within_Range(GEF.Arccot( 0.0), Pi/2.0, 0.001)     or
         not Result_Within_Range( EF.Arccot(-1.0), 3.0*Pi/4.0, 0.001) 
      then
         Report.Failed("Incorrect results from the Arccot function when " &
                       "provided a variety of Y parameter values");
      end if;


      -- Check the results of the Arccot function with specified cycle
      -- parameter.

      -- Check that the Arccot function with specified Cycle parameter 
      -- raises Argument_Error when the value of the Cycle parameter is zero
      -- or negative.

      begin
         Float_Result := EF.Arccot(X => Pi, Cycle => 0.0);  -- Default Y value
         Report.Failed("Argument_Error not raised by the Arccot function " &
                       "with default Y parameter value, when the Cycle "   &
                       "parameter is 0.0");
         Dont_Optimize_Float(Float_Result, 10);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Incorrect exception raised by the Arccot "      &
                          "function with default Y parameter value, when " &
                          "provided a 0.0 cycle parameter value");
      end;

      begin
         New_Float_Result := GEF.Arccot(X => Pi, Y => 1.0, Cycle => 0.0);
         Report.Failed("Argument_Error not raised by the Arccot function " &
                       "when the Cycle parameter is 0.0");
         Dont_Optimize_New_Float(New_Float_Result, 11);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Incorrect exception raised by the Arccot "     &
                          "function when provided a 0.0 cycle parameter " &
                          "value");
      end;

      begin
         Float_Result := EF.Arccot(X => Pi, Cycle => -360.0); 
         Report.Failed("Argument_Error not raised by the Arccot function " &
                       "with a default Y parameter value, when the Cycle " &
                       "parameter is -360.0");
         Dont_Optimize_Float(Float_Result, 12);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Incorrect exception raised by the Arccot "        &
                          "function with a default Y parameter value, when " & 
                          "provided a -360.0 cycle parameter value");
      end;

      begin
         New_Float_Result := GEF.Arccot(X => Pi, Y => 1.0, Cycle => -Pi);
         Report.Failed("Argument_Error not raised by the Arccot function " &
                       "when the Cycle parameter is -Pi");
         Dont_Optimize_New_Float(New_Float_Result, 13);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Incorrect exception raised by the Arccot "     &
                          "function when provided a -Pi cycle parameter " &
                          "value");
      end;


      -- Check that no exception is raised by the Arccot function with 
      -- specified Cycle parameter, when provided large and small positive
      -- or negative parameter values for both X and Y input parameters. 

      begin
         Float_Result := EF.Arccot(X     => -FXA5A00.Large, 
                                   Y     => -FXA5A00.Large,
--pwb-math  Next line: changed 2.0*Pi to 360.0
                                   Cycle => 360.0);
         Dont_Optimize_Float(Float_Result, 14);
      exception
         when others =>  
            Report.Failed("Exception raised when the Arccot function with " &
                          "specified Cycle parameter, when provided large " & 
                          "negative X and Y parameter values");
      end;


      begin
         New_Float_Result := GEF.Arccot(New_Float(FXA5A00.Large), 
                                        Y     => New_Float(-FXA5A00.Small),
--pwb-math  Next line: changed 2.0*Pi to 360.0
                                        Cycle => 360.0);
         Dont_Optimize_New_Float(New_Float_Result, 15);
      exception
         when others => 
            Report.Failed("Exception raised when the Arccot function with "  &
                          "specified Cycle parameter, when provided large "  & 
                          "positive X parameter value and a small negative " &
                          "Y parameter value");
      end;


      begin
         Float_Result := EF.Arccot(X     => -FXA5A00.Small, 
                                   Y     => -FXA5A00.Large,
--pwb-math  Next line: changed 2.0*Pi to 360.0
                                   Cycle => 360.0);
         Dont_Optimize_Float(Float_Result, 16);
      exception
         when others => 
            Report.Failed("Exception raised when the Arccot function with "  &
                          "specified Cycle parameter, when provided small "  & 
                          "negative X parameter value and a large negative " &
                          "Y parameter value");
      end;

      begin
         New_Float_Result := GEF.Arccot(New_Float(FXA5A00.Small), 
                                        New_Float(FXA5A00.Large), 
--pwb-math  Next line: changed 2.0*Pi to 360.0
                                        360.0);
         Dont_Optimize_New_Float(New_Float_Result, 17);
      exception
         when others => 
            Report.Failed("Exception raised when the Arccot function with " &
                          "specified Cycle parameter, when provided a "     & 
                          "small positive X parameter value and a large "   &
                          "positive Y parameter value");
      end;


      -- Check that the Arccot function with specified Cycle parameter 
      -- provides correct results when provided a variety of X parameter
      -- input values.

      if not FXA5A00.Result_Within_Range(GEF.Arccot( 0.0, Cycle => 360.0),
                                         90.0, 
                                         0.001) or
         not FXA5A00.Result_Within_Range(EF.Arccot( 0.0, Cycle => 100.0),
                                         25.0,
                                         0.001) or
         not FXA5A00.Result_Within_Range(GEF.Arccot( 1.0, Cycle => 360.0),
                                         45.0,
                                         0.001) or
         not FXA5A00.Result_Within_Range(EF.Arccot( 1.0, Cycle => 100.0),
                                         12.5,
                                         0.001) or
         not FXA5A00.Result_Within_Range(GEF.Arccot(-1.0, Cycle => 360.0),
                                         135.0,
                                         0.001) or
         not FXA5A00.Result_Within_Range(EF.Arccot(-1.0, Cycle => 100.0),
                                         37.5,
                                         0.001)
      then
         Report.Failed("Incorrect results from the Arccot function with "   &
                       "specified Cycle parameter when provided a variety " &
                       "of X parameter values");
      end if;


      if not FXA5A00.Result_Within_Range(EF.Arccot(0.2425355, 0.9701420),
                                         EF.Arccot(0.25),
                                         0.01) or
         not FXA5A00.Result_Within_Range(EF.Arccot(0.3162277, 0.9486831),
                                         Ef.Arccot(0.33),
                                         0.01)
      then
         Report.Failed("Incorrect results from the Arccot function with " &
                       "comparison to other Arccot function results");
      end if;


      if not FXA5A00.Result_Within_Range(EF.Cot(EF.Arccot(0.4472135,
                                                          0.8944270)),
                                         0.5,
                                         0.01) or
         not FXA5A00.Result_Within_Range(EF.Cot(EF.Arccot(0.9987380, 
                                                          0.0499369)),
                                         20.0,
                                         0.1)
      then
         Report.Failed("Incorrect results from the Arccot function when " &
                       "used as argument to Cot function");
      end if;


      -- Check that inverse function results are correct.
      -- Default Cycle test.

      Angle := 0.001;
      while Angle < Pi and not Incorrect_Inverse loop
         if not Result_Within_Range(EF.Arccot(EF.Cot(Angle)), Angle, 0.001)
         then
            Incorrect_Inverse := True;
         end if;
         Angle := Angle + 0.001;
      end loop;

      if Incorrect_Inverse then
         Report.Failed("Incorrect results returned from the Inverse "    &
                       "comparison of Cot and Arccot using the default " &
                       "cycle value");
         Incorrect_Inverse := False;
      end if;

      -- Non-Default Cycle test.

      New_Float_Angle := 0.01;
      while New_Float_Angle < 180.0 and not Incorrect_Inverse loop
         if not Result_Within_Range(EF.Arccot(EF.Cot(Float(New_Float_Angle), 
                                                     Cycle => 360.0), 
                                              Cycle => 360.0),
                                    Float(New_Float_Angle),
                                    0.01)  or
            not Result_Within_Range(GEF.Arccot(
                                       New_Float(GEF.Cot(New_Float_Angle, 
                                                         Cycle => 360.0)), 
                                       Cycle => 360.0),
                                    Float(New_Float_Angle),
                                    0.01)  
         then
            Incorrect_Inverse := True;
         end if;
         New_Float_Angle := New_Float_Angle + 0.01;
      end loop;

      if Incorrect_Inverse then
         Report.Failed("Incorrect results returned from the Inverse "    &
                       "comparison of Cot and Arccot using non-default " &
                       "cycle value");
      end if;


   exception
      when The_Error : others => 
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXA5A08;
