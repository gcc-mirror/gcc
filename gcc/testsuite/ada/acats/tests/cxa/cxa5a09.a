-- CXA5A09.A
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
--      Check that the function Log provides correct results.
--
-- TEST DESCRIPTION:
--      This test examines both the version of Log resulting from the 
--      instantiation of the Ada.Numerics.Generic_Elementary_Functions with 
--      with a type derived from type Float,as well as the preinstantiated
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
--         CXA5A09.A
--
--
-- CHANGE HISTORY:
--      11 Apr 95   SAIC    Initial prerelease version.
--      13 Jun 95   SAIC    Incorporated use of Dont_Optimize procedure, and
--                          use of Result_Within_Range function overloaded for
--                          FXA5A00.New_Float_Type.
--      29 Jun 98   EDS     Protected exception tests by first testing
--                          for 'Machine_Overflows
--
--!

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Generic_Elementary_Functions;
with FXA5A00;
with Report;

procedure CXA5A09 is
begin

   Report.Test ("CXA5A09", "Check that the Log function provides " &
                           "correct results");

   Test_Block:
   declare

      use Ada.Numerics;
      use FXA5A00;

      package GEF is new Ada.Numerics.Generic_Elementary_Functions(New_Float);
      package  EF renames Ada.Numerics.Elementary_Functions;

      Arg,
      Float_Result     : Float     := 0.0;
      New_Float_Result : New_Float := 0.0;

      Incorrect_Inverse,
      Incorrect_Inverse_Base_2,
      Incorrect_Inverse_Base_8,
      Incorrect_Inverse_Base_10,
      Incorrect_Inverse_Base_16  : Boolean   := False;

      procedure Dont_Optimize_Float     is new Dont_Optimize(Float);
      procedure Dont_Optimize_New_Float is new Dont_Optimize(New_Float);

   begin

      -- Testing of Log Function, both instantiated and pre-instantiated
      -- version.

      -- Check that Argument_Error is raised when the parameter X is negative.

      begin
         New_Float_Result := GEF.Log(X => -1.0);
         Report.Failed("Argument_Error not raised by the Log function " &
                       "when the input parameter is negative");
         Dont_Optimize_New_Float(New_Float_Result, 1);
      exception
         when Argument_Error => null; -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Log function " &
                          "when the input parameter is negative");
      end;

      begin
         Float_Result := EF.Log(X => -FXA5A00.Large);
         Report.Failed("Argument_Error not raised by the Log function " &
                       "when the input parameter is negative");
         Dont_Optimize_Float(Float_Result, 2);
      exception
         when Argument_Error => null; -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Log function " &
                          "when the input parameter is negative");
      end;


      -- Check that Constraint_Error is raised when the Log function is 
      -- provided an input parameter of zero.

      if New_Float'Machine_Overflows = True then
         begin
            New_Float_Result := GEF.Log(X => 0.0);
            Report.Failed("Constraint_Error not raised by the Log function " &
                          "when the input parameter is zero");
            Dont_Optimize_New_Float(New_Float_Result, 3);
         exception
            when Constraint_Error => null; -- OK, expected exception.
            when others         =>
               Report.Failed("Unexpected exception raised by the Log function "
                             & "when the input parameter is zero");
         end;
      end if;
      

      -- Check for the reference manual prescribed results of the Log function.

      if GEF.Log(X => 1.0) /= 0.0 or
          EF.Log(X => 1.0) /= 0.0
      then
         Report.Failed("Incorrect result from Function Log when provided " &
                       "an input parameter value of 1.0");
      end if;


      -- Check that the Log function provides correct results when provided
      -- a variety of input parameters.

      if not FXA5A00.Result_Within_Range(GEF.Log(0.015),   -4.20,  0.01)  or
         not FXA5A00.Result_Within_Range(GEF.Log(0.592),   -0.524, 0.001) or
         not FXA5A00.Result_Within_Range(GEF.Log(0.997),   -0.003, 0.001) or
         not FXA5A00.Result_Within_Range(GEF.Log(1.341),    0.293, 0.001) or
         not FXA5A00.Result_Within_Range( EF.Log(2.826),    1.04,  0.01)  or
         not FXA5A00.Result_Within_Range( EF.Log(10.052),   2.31,  0.01)  or
         not FXA5A00.Result_Within_Range( EF.Log(2569.143), 7.85,  0.01)
      then
         Report.Failed("Incorrect results from Function Log when provided " &
                       "a variety of input parameter values");
      end if;

      Arg := 0.001;
      while Arg < 1.0 and not Incorrect_Inverse loop
         if not Result_Within_Range(EF."**"(e,EF.Log(Arg)), Arg, 0.001) then
            Incorrect_Inverse := True;
         end if;
         Arg := Arg + 0.001;
      end loop;

      if Incorrect_Inverse then
         Report.Failed("Incorrect inverse result comparing ""**"" and " &
                       "Log function over argument range 0.001..1.0");
         Incorrect_Inverse := False;
      end if;

      Arg := 1.0;
      while Arg < 10.0 and not Incorrect_Inverse loop
         if not Result_Within_Range(EF."**"(e,EF.Log(Arg)), Arg, 0.01) then
            Incorrect_Inverse := True;
         end if;
         Arg := Arg + 0.01;
      end loop;

      if Incorrect_Inverse then
         Report.Failed("Incorrect inverse result comparing ""**"" and " &
                       "Log function over argument range 1.0..10.0");
         Incorrect_Inverse := False;
      end if;

      Arg := 1.0;
      while Arg < 1000.0 and not Incorrect_Inverse loop
         if not Result_Within_Range(EF."**"(e,EF.Log(Arg)), Arg, 0.1) then
            Incorrect_Inverse := True;
         end if;
         Arg := Arg + 1.0;
      end loop;

      if Incorrect_Inverse then
         Report.Failed("Incorrect inverse result comparing ""**"" and " &
                       "Log function over argument range 1.0..1000.0");
      end if;


      -- Testing of Log Function, with specified Base parameter, both 
      -- instantiated and pre-instantiated versions.

      -- Check that Argument_Error is raised by the Log function with 
      -- specified Base parameter, when the X parameter value is negative.

      begin
         New_Float_Result := GEF.Log(X => -1.0, Base => 16.0);
         Report.Failed("Argument_Error not raised by the Log function " &
                       "with Base parameter, when the input parameter " &
                       "value is -1.0");
         Dont_Optimize_New_Float(New_Float_Result, 4);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         => 
            Report.Failed("Unexpected exception raised by the Log function " &
                          "with Base parameter, when the X parameter value " &
                          "is -1.0");
      end;

      begin
         Float_Result := EF.Log(X => -FXA5A00.Large, Base => 8.0);
         Report.Failed("Argument_Error not raised by the Log function " &
                       "with Base parameter, when the X parameter "     &
                       "value is a large negative value");
         Dont_Optimize_Float(Float_Result, 5);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         => 
            Report.Failed("Unexpected exception raised by the Log function " &
                          "with Base parameter, when the X parameter "       &
                          "value is a large negative value");
      end;


      -- Check that Argument_Error is raised by the Log function when 
      -- the specified Base parameter is zero.

      begin
         New_Float_Result := GEF.Log(X => 10.0, Base => 0.0);
         Report.Failed("Argument_Error not raised by the Log function " &
                       "with Base parameter of 0.0");
         Dont_Optimize_New_Float(New_Float_Result, 6);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         => 
            Report.Failed("Unexpected exception raised by the Log function " &
                          "with Base parameter of 0.0");
      end;


      -- Check that Argument_Error is raised by the Log function when 
      -- the specified Base parameter is one.

      begin
         Float_Result := EF.Log(X => 12.3, Base => 1.0);
         Report.Failed("Argument_Error not raised by the Log function " &
                       "with Base parameter of 1.0");
         Dont_Optimize_Float(Float_Result, 7);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         => 
            Report.Failed("Unexpected exception raised by the Log function " &
                          "with Base parameter of 1.0");
      end;


      -- Check that Argument_Error is raised by the Log function when 
      -- the specified Base parameter is negative.

      begin
         New_Float_Result := GEF.Log(X => 12.3, Base => -10.0);
         Report.Failed("Argument_Error not raised by the Log function " &
                       "with negative Base parameter");
         Dont_Optimize_New_Float(New_Float_Result, 8);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         => 
            Report.Failed("Unexpected exception raised by the Log function " &
                          "with negative Base parameter");
      end;


      -- Check that Constraint_Error is raised by the Log function when the
      -- input X parameter value is 0.0.

      if New_Float'Machine_Overflows = True then
         begin
            New_Float_Result := GEF.Log(X => 0.0, Base => 16.0);
            Report.Failed("Constraint_Error not raised by the Log function "  &
                          "with specified Base parameter, when the value of " &
                          "the parameter X is 0.0");
            Dont_Optimize_New_Float(New_Float_Result, 9);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Unexpected exception raised by Function Log"    &
                             "with specified Base parameter, when the value " &
                             "of the parameter X is 0.0");
         end;
      end if;

      -- Check for the prescribed results of the Log function with specified
      -- Base parameter.

      if GEF.Log(X => 1.0, Base => 16.0) /= 0.0 or
          EF.Log(X => 1.0, Base => 10.0) /= 0.0 or
         GEF.Log(1.0, Base => 8.0)       /= 0.0 or
          EF.Log(1.0, 2.0)               /= 0.0
      then
         Report.Failed("Incorrect result from Function Log with specified " &
                       "Base parameter when provided an parameter X input " &
                       "value of 1.0");
      end if;


      -- Check that the Log function with specified Base parameter provides 
      -- correct results when provided a variety of input parameters.

      if not Result_Within_Range(GEF.Log( 10.0,  e),     2.30,  0.01)  or
         not Result_Within_Range( EF.Log(  8.0,  2.0),   3.0,   0.01)  or
         not Result_Within_Range(GEF.Log(256.0,  2.0),   8.0,   0.01)  or
         not Result_Within_Range( EF.Log(512.0,  8.0),   3.0,   0.01)  or
         not Result_Within_Range(GEF.Log(0.5649, e),    -0.57,  0.01)  or
         not Result_Within_Range( EF.Log(1.7714, e),     0.57,  0.01)  or
         not Result_Within_Range(GEF.Log(0.5718, 10.0), -0.243, 0.001) or
         not Result_Within_Range( EF.Log(466.25, 10.0),  2.67,  0.01) 
      then
         Report.Failed("Incorrect results from Function Log with specified " &
                       "Base parameter, when provided a variety of input "   &
                       "parameter values");
      end if;


      Arg := 1.0;
      while Arg < 1000.0 and 
            not (Incorrect_Inverse_Base_2  and Incorrect_Inverse_Base_8 and
                 Incorrect_Inverse_Base_10 and Incorrect_Inverse_Base_16) 
      loop
         if not FXA5A00.Result_Within_Range(EF."**"(2.0,EF.Log(Arg,2.0)),
                                            Arg,
                                            0.001) 
         then
            Incorrect_Inverse_Base_2 := True;
         end if;
         if not FXA5A00.Result_Within_Range(EF."**"(8.0,EF.Log(Arg,8.0)),
                                            Arg,
                                            0.001) 
         then
            Incorrect_Inverse_Base_8 := True;
         end if;
         if not FXA5A00.Result_Within_Range(EF."**"(10.0,EF.Log(Arg,10.0)),
                                            Arg,
                                            0.001) 
         then
            Incorrect_Inverse_Base_10 := True;
         end if;
         if not FXA5A00.Result_Within_Range(EF."**"(16.0,EF.Log(Arg,16.0)), 
                                            Arg,
                                            0.001) 
         then
            Incorrect_Inverse_Base_16 := True;
         end if;
         Arg := Arg + 1.0;
      end loop;

      if Incorrect_Inverse_Base_2 then
         Report.Failed("Incorrect inverse result comparing ""**"" and " &
                       "Log function for Base 2");
      end if;

      if Incorrect_Inverse_Base_8 then
         Report.Failed("Incorrect inverse result comparing ""**"" and " &
                       "Log function for Base 8");
      end if;

      if Incorrect_Inverse_Base_10 then
         Report.Failed("Incorrect inverse result comparing ""**"" and " &
                       "Log function for Base 10");
      end if;

      if Incorrect_Inverse_Base_16 then
         Report.Failed("Incorrect inverse result comparing ""**"" and " &
                       "Log function for Base 16");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA5A09;
