-- CXA5A10.A
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
--      Check that the functions Exp and Sqrt, and the exponentiation
--      operator "**" provide correct results.
--
-- TEST DESCRIPTION:
--      This test examines both the versions of Exp, Sqrt, and "**"
--      resulting from the instantiation of the
--      Ada.Numerics.Generic_Elementary_Functions with a type derived from
--      type Float, as well as the preinstantiated version of this package
--      for type Float.
--      Prescribed results (stated as such in the reference manual),
--      including instances prescribed to raise exceptions, are examined
--      in the test cases.  In addition, certain evaluations are performed
--      for the preinstantiated package where the actual function result is
--      compared with the expected result (within an epsilon range of
--      accuracy).
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FXA5A00.A   (foundation code)
--         CXA5A10.A
--
--
-- CHANGE HISTORY:
--      17 Apr 95   SAIC    Initial prerelease version.
--      13 Jun 95   SAIC    Incorporated use of Dont_Optimize procedure, and
--                          use of Result_Within_Range function overloaded for
--                          FXA5A00.New_Float_Type.
--      18 Apr 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      01 Oct 01   RLB     Protected Constraint_Error exception tests by
--                          first testing for 'Machine_Overflows.
--
--!

with Ada.Exceptions;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Generic_Elementary_Functions;
with FXA5A00;
with Report;

procedure CXA5A10 is
begin

   Report.Test ("CXA5A10", "Check that Exp, Sqrt, and the ""**"" operator " &
                           "provide correct results");

   Test_Block:
   declare

      use FXA5A00, Ada.Numerics;
      use Ada.Exceptions;

      package GEF is new Ada.Numerics.Generic_Elementary_Functions(New_Float);
      package  EF renames Ada.Numerics.Elementary_Functions;

      use GEF, EF;

      Arg,
      Float_Result     : Float;
      New_Float_Result : New_Float;

      Flag_1, Flag_2, Flag_3, Flag_4,
      Incorrect_Inverse_Base_e,
      Incorrect_Inverse_Base_2,
      Incorrect_Inverse_Base_8,
      Incorrect_Inverse_Base_10,
      Incorrect_Inverse_Base_16        : Boolean := False;

      procedure Dont_Optimize_Float     is new Dont_Optimize(Float);
      procedure Dont_Optimize_New_Float is new Dont_Optimize(New_Float);

   begin

      -- Testing of the "**" operator, both instantiated and pre-instantiated
      -- version.

      -- Check that Argument_Error is raised by the exponentiation operator
      -- when the value of the Left parameter (operand) is negative.

      begin
         New_Float_Result := GEF."**"(Left  => -10.0,
                                      Right =>   2.0);
         Report.Failed("Argument_Error not raised by the instantiated "   &
                       "version of the exponentiation operator when the " &
                       "value of the Left parameter is negative");
         Dont_Optimize_New_Float(New_Float_Result, 1);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the "            &
                          "instantiated version of the exponentiation "    &
                          "operator when the value of the Left parameter " &
                          "is negative");
      end;

      begin
         Float_Result := (-FXA5A00.Small) ** 4.0;
         Report.Failed("Argument_Error not raised by the preinstantiated " &
                       "version of the exponentiation operator when the "  &
                       "value of the Left parameter is negative");
         Dont_Optimize_Float(Float_Result, 2);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the "            &
                          "preinstantiated version of the exponentiation " &
                          "operator when the value of the Left parameter " &
                          "is negative");
      end;


      -- Check that Argument_Error is raised by the exponentiation operator
      -- when both parameters (operands) have the value 0.0.

      begin
         New_Float_Result := GEF."**"(0.0, Right => 0.0);
         Report.Failed("Argument_Error not raised by the instantiated " &
                       "version of the exponentiation operator when "   &
                       "both operands are zero");
         Dont_Optimize_New_Float(New_Float_Result, 3);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the "         &
                          "instantiated version of the exponentiation " &
                          "operator when both operands are zero");
      end;

      begin
         Float_Result := 0.0**0.0;
         Report.Failed("Argument_Error not raised by the preinstantiated " &
                       "version of the exponentiation operator when both " &
                       "operands are zero");
         Dont_Optimize_Float(Float_Result, 4);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the "            &
                          "preinstantiated version of the exponentiation " &
                          "operator when both operands are zero");
      end;


      -- Check that Constraint_Error is raised by the exponentiation
      -- operator when the value of the left parameter (operand) is zero,
      -- and the value of the right parameter (exponent) is negative.
      -- This check applies only if Machine_Overflows is true [A.5.1(28, 30)].

      if New_Float'Machine_Overflows = True then
         begin
            New_Float_Result := GEF."**"(0.0, Right => -2.0);
            Report.Failed("Constraint_Error not raised by the instantiated " &
                          "version of the exponentiation operator when "     &
                          "the left parameter is 0.0, and the right "        &
                          "parameter is negative");
            Dont_Optimize_New_Float(New_Float_Result, 5);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Unexpected exception raised by the "         &
                             "instantiated version of the exponentiation " &
                             "operator when the left parameter is 0.0, "   &
                             "and the right parameter is negative");
         end;
      end if;

      if Float'Machine_Overflows = True then
         begin
            Float_Result := 0.0 ** (-FXA5A00.Small);
            Report.Failed("Constraint_Error not raised by the " &
                          "preinstantiated version of the exponentiation " &
                          "operator when the left parameter is 0.0, and the " &
                          "right parameter is negative");
            Dont_Optimize_Float(Float_Result, 6);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Unexpected exception raised by the "            &
                             "preinstantiated version of the exponentiation " &
                             "operator when the left parameter is 0.0, and "  &
                             "the right parameter is negative");
         end;
      end if;

      -- Prescribed results.
      -- Check that exponentiation by a 0.0 exponent yields the value one.

      if GEF."**"(Left => 10.0,  Right => 0.0) /= 1.0 or
          EF."**"(FXA5A00.Large, Right => 0.0) /= 1.0 or
         GEF."**"(3.0, 0.0)                    /= 1.0 or
          FXA5A00.Small ** 0.0                 /= 1.0
      then
         Report.Failed("Incorrect results returned from the ""**"" " &
                       "operator when the value of the exponent is 0.0");
      end if;


      -- Check that exponentiation by a unit exponent yields the value
      -- of the left operand.

      if GEF."**"(Left => 50.0,  Right => 1.0) /= 50.0          or
          EF."**"(FXA5A00.Large, Right => 1.0) /= FXA5A00.Large or
         GEF."**"(6.0, 1.0)                    /= 6.0           or
          FXA5A00.Small ** 1.0                 /= FXA5A00.Small
      then
         Report.Failed("Incorrect results returned from the ""**"" " &
                       "operator when the value of the exponent is 1.0");
      end if;


      -- Check that exponentiation of the value 1.0 yields the value 1.0.

      if GEF."**"(Left => 1.0, Right => 16.0)   /=  1.0 or
          EF."**"(1.0, Right => FXA5A00.Large)  /=  1.0 or
         GEF."**"(1.0, 3.0)                     /=  1.0 or
          1.0 ** FXA5A00.Small                  /=  1.0
      then
         Report.Failed("Incorrect results returned from the ""**"" " &
                       "operator when the value of the operand is 1.0");
      end if;


      -- Check that exponentiation of the value 0.0 yields the value 0.0.

      if GEF."**"(Left => 0.0, Right => 10.0)   /=  0.0 or
          EF."**"(0.0, Right => FXA5A00.Large)  /=  0.0 or
         GEF."**"(0.0, 4.0)                     /=  0.0 or
          0.0 ** FXA5A00.Small                  /=  0.0
      then
         Report.Failed("Incorrect results returned from the ""**"" " &
                       "operator when the value of the operand is 0.0");
      end if;


      -- Check that exponentiation of various operands with a variety of
      -- of exponent values yield correct results.

      if not Result_Within_Range(GEF."**"(5.0,   2.0),  25.0,   0.01)  or
         not Result_Within_Range(GEF."**"(1.225, 1.5),   1.36,  0.01)  or
         not Result_Within_Range(GEF."**"(0.26,  2.0),   0.068, 0.001) or
         not Result_Within_Range( EF."**"(e,     5.0), 148.4,   0.1)   or
         not Result_Within_Range( EF."**"(10.0,  e),   522.7,   0.1)   or
         not Result_Within_Range( EF."**"(e,   (-3.0)),  0.050, 0.001) or
         not Result_Within_Range(GEF."**"(10.0,(-2.0)),  0.010, 0.001)
      then
         Report.Failed("Incorrect results returned from the ""**"" "       &
                       "operator with a variety of  operand and exponent " &
                       "values");
      end if;


      -- Use the following loops to check for internal consistency between
      -- inverse functions.

      declare
         -- Use the relative error value to account for non-exact
         -- computations.
         TC_Relative_Error: Float := 0.005;
      begin
         for i in 1..5 loop
            for j in 0..5 loop
               if not Incorrect_Inverse_Base_e and
                  not FXA5A00.Result_Within_Range
                        (Float(i)**Float(j),
                         e**(Float(j)*EF.Log(Float(i))),
                         TC_Relative_Error)
               then
                  Incorrect_Inverse_Base_e := True;
                  Report.Failed("Incorrect Log-** Inverse calc for Base e " &
                                "with i= " & Integer'Image(i) & "  and j= " &
                                Integer'Image(j));
               end if;
               if not Incorrect_Inverse_Base_2 and
                  not FXA5A00.Result_Within_Range
                        (Float(i)**Float(j),
                         2.0**(Float(j)*EF.Log(Float(i),2.0)),
                         TC_Relative_Error)
               then
                  Incorrect_Inverse_Base_2 := True;
                  Report.Failed("Incorrect Log-** Inverse calc for Base 2 " &
                                "with i= " & Integer'Image(i) & "  and j= " &
                                Integer'Image(j));
               end if;
               if not Incorrect_Inverse_Base_8 and
                  not FXA5A00.Result_Within_Range
                        (Float(i)**Float(j),
                         8.0**(Float(j)*EF.Log(Float(i),8.0)),
                         TC_Relative_Error)
               then
                  Incorrect_Inverse_Base_8 := True;
                  Report.Failed("Incorrect Log-** Inverse calc for Base 8 " &
                                "with i= " & Integer'Image(i) & "  and j= " &
                                Integer'Image(j));
               end if;
               if not Incorrect_Inverse_Base_10 and
                  not FXA5A00.Result_Within_Range
                        (Float(i)**Float(j),
                         10.0**(Float(j)*EF.Log(Float(i),10.0)),
                         TC_Relative_Error)
               then
                  Incorrect_Inverse_Base_10 := True;
                  Report.Failed("Incorrect Log-** Inverse calc for Base 10 " &
                                "with i= " & Integer'Image(i) & "   and j= " &
                                Integer'Image(j));
               end if;
               if not Incorrect_Inverse_Base_16 and
                  not FXA5A00.Result_Within_Range
                        (Float(i)**Float(j),
                         16.0**(Float(j)*EF.Log(Float(i),16.0)),
                         TC_Relative_Error)
               then
                  Incorrect_Inverse_Base_16 := True;
                  Report.Failed("Incorrect Log-** Inverse calc for Base 16 " &
                                "with i= " & Integer'Image(i) & "   and j= " &
                                Integer'Image(j));
               end if;
            end loop;
         end loop;
      end;

      -- Reset Flags.
      Incorrect_Inverse_Base_e  := False;
      Incorrect_Inverse_Base_2  := False;
      Incorrect_Inverse_Base_8  := False;
      Incorrect_Inverse_Base_10 := False;
      Incorrect_Inverse_Base_16 := False;


      -- Testing of Exp Function, both instantiated and pre-instantiated
      -- version.

      -- Check that the result of the Exp Function, when provided an X
      -- parameter value of 0.0, is 1.0.

      if GEF.Exp(X => 0.0) /= 1.0 or
          EF.Exp(0.0)      /= 1.0
      then
         Report.Failed("Incorrect result returned by Function Exp when " &
                       "given a parameter value of 0.0");
      end if;


      -- Check that the Exp Function provides correct results when provided
      -- a variety of input parameter values.

      if not Result_Within_Range(GEF.Exp(0.001),    1.01,  0.01)  or
         not Result_Within_Range( EF.Exp(0.1),      1.11,  0.01)  or
         not Result_Within_Range(GEF.Exp(1.2697),   3.56,  0.01)  or
         not Result_Within_Range( EF.Exp(3.2525),  25.9,   0.1)   or
         not Result_Within_Range(GEF.Exp(-0.2198),  0.803, 0.001) or
         not Result_Within_Range( EF.Exp(-1.6621),  0.190, 0.001) or
         not Result_Within_Range(GEF.Exp(-2.3888),  0.092, 0.001) or
         not Result_Within_Range( EF.Exp(-5.4415),  0.004, 0.001)
      then
         Report.Failed("Incorrect result from Function Exp when provided " &
                       "a variety of input parameter values");
      end if;

      -- Use the following loops to check for internal consistency between
      -- inverse functions.

      Arg := 0.01;
      while Arg < 10.0 loop
         if not Incorrect_Inverse_Base_e and
            FXA5A00.Result_Within_Range(EF.Exp(Arg),
                                        e**(Arg*EF.Log(Arg)),
                                        0.001)
         then
            Incorrect_Inverse_Base_e := True;
            Report.Failed("Incorrect Exp-** Inverse calc for Base e");
         end if;
         if not Incorrect_Inverse_Base_2 and
            FXA5A00.Result_Within_Range(EF.Exp(Arg),
                                        2.0**(Arg*EF.Log(Arg,2.0)),
                                        0.001)
         then
            Incorrect_Inverse_Base_2 := True;
            Report.Failed("Incorrect Exp-** Inverse calc for Base 2");
         end if;
         if not Incorrect_Inverse_Base_8 and
            FXA5A00.Result_Within_Range(EF.Exp(Arg),
                                        8.0**(Arg*EF.Log(Arg,8.0)),
                                        0.001)
         then
            Incorrect_Inverse_Base_8 := True;
            Report.Failed("Incorrect Exp-** Inverse calc for Base 8");
         end if;
         if not Incorrect_Inverse_Base_10 and
            FXA5A00.Result_Within_Range(EF.Exp(Arg),
                                        10.0**(Arg*EF.Log(Arg,10.0)),
                                        0.001)
         then
            Incorrect_Inverse_Base_10 := True;
            Report.Failed("Incorrect Exp-** Inverse calc for Base 10");
         end if;
         if not Incorrect_Inverse_Base_16 and
            FXA5A00.Result_Within_Range(EF.Exp(Arg),
                                        16.0**(Arg*EF.Log(Arg,16.0)),
                                        0.001)
         then
            Incorrect_Inverse_Base_16 := True;
            Report.Failed("Incorrect Exp-** Inverse calc for Base 16");
         end if;
         Arg := Arg + 0.01;
      end loop;


      -- Testing of Sqrt Function, both instantiated and pre-instantiated
      -- version.

      -- Check that Argument_Error is raised by the Sqrt Function when
      -- the value of the input parameter X is negative.

      begin
         Float_Result := EF.Sqrt(X => -FXA5A00.Small);
         Report.Failed("Argument_Error not raised by Function Sqrt "     &
                       "when provided a small negative input parameter " &
                       "value");
         Dont_Optimize_Float(Float_Result, 7);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by Function Sqrt "   &
                          "when provided a small negative input parameter " &
                          "value");
      end;

      begin
         New_Float_Result := GEF.Sqrt(X => -64.0);
         Report.Failed("Argument_Error not raised by Function Sqrt "     &
                       "when provided a large negative input parameter " &
                       "value");
         Dont_Optimize_New_Float(New_Float_Result, 8);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by Function Sqrt "   &
                          "when provided a large negative input parameter " &
                          "value");
      end;


      -- Check that the Sqrt Function, when given an X parameter value of 0.0,
      -- returns a result of 0.0.

      if GEF.Sqrt(X => 0.0) /= 0.0 or
          EF.Sqrt(0.0)      /= 0.0
      then
         Report.Failed("Incorrect result from Function Sqrt when provided " &
                       "an input parameter value of 0.0");
      end if;


      -- Check that the Sqrt Function, when given an X parameter input value
      -- of 1.0, returns a result of 1.0.

      if GEF.Sqrt(X => 1.0) /= 1.0 or
          EF.Sqrt(1.0)      /= 1.0
      then
         Report.Failed("Incorrect result from Function Sqrt when provided " &
                       "an input parameter value of 1.0");
      end if;


      -- Check that the Sqrt Function provides correct results when provided
      -- a variety of input parameter values.

      if not FXA5A00.Result_Within_Range(GEF.Sqrt(0.0327),     0.181, 0.001) or
         not FXA5A00.Result_Within_Range( EF.Sqrt(0.1808),     0.425, 0.001) or
         not FXA5A00.Result_Within_Range(GEF.Sqrt(1.0556),     1.03,  0.01)  or
         not FXA5A00.Result_Within_Range( EF.Sqrt(32.8208),    5.73,  0.01)  or
         not FXA5A00.Result_Within_Range( EF.Sqrt(27851.0),  166.9,   0.1)   or
         not FXA5A00.Result_Within_Range( EF.Sqrt(61203.4),  247.4,   0.1)   or
         not FXA5A00.Result_Within_Range( EF.Sqrt(655891.0), 809.9,   0.1)
      then
         Report.Failed("Incorrect result from Function Sqrt when provided " &
                       "a variety of input parameter values");
      end if;

      -- Check internal consistency between functions.

      Arg := 0.01;
      while Arg < 10.0 loop
         if not Flag_1 and
            not FXA5A00.Result_Within_Range(Arg,
                                            EF.Sqrt(Arg)*EF.Sqrt(Arg),
                                            0.01)
         then
            Report.Failed("Inconsistency found in Case 1");
            Flag_1 := True;
         end if;
         if not Flag_2 and
            not FXA5A00.Result_Within_Range(Arg, EF.Sqrt(Arg)**2.0, 0.01)
         then
            Report.Failed("Inconsistency found in Case 2");
            Flag_2 := True;
         end if;
         if not Flag_3 and
            not FXA5A00.Result_Within_Range(EF.Log(Arg),
                                            EF.Log(Sqrt(Arg)**2.0), 0.01)
         then
            Report.Failed("Inconsistency found in Case 3");
            Flag_3 := True;
         end if;
         if not Flag_4 and
            not FXA5A00.Result_Within_Range(EF.Log(Arg),
                                            2.00*EF.Log(EF.Sqrt(Arg)),
                                            0.01)
         then
            Report.Failed("Inconsistency found in Case 4");
            Flag_4 := True;
         end if;
         Arg := Arg + 1.0;
      end loop;


   exception
      when The_Error : others =>
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXA5A10;
