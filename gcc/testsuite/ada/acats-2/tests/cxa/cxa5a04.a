-- CXA5A04.A
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
--      Check that the functions Cot, Coth, and Arccoth provide correct 
--      results.
--
-- TEST DESCRIPTION:
--      This test examines both the version of Cot, Coth, and Arccoth 
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
--         CXA5A04.A
--
--
-- CHANGE HISTORY:
--      15 Mar 95   SAIC    Initial prerelease version.
--      07 Apr 95   SAIC    Corrected errors in context clause reference, 
--                          added trigonometric relationship checks.
--      13 Jun 95   SAIC    Incorporated use of Dont_Optimize procedure, and
--                          use of Result_Within_Range function overloaded for
--                          FXA5A00.New_Float_Type.
--      18 Apr 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      28 Feb 97   PWB.CTA Removed checks with explicit Cycle => 2.0*Pi
--      29 Jun 98   EDS     Protected exception tests by first testing
--                          for 'Machine_Overflows
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

procedure CXA5A04 is
begin

   Report.Test ("CXA5A04", "Check that the functions Cot, Coth, and " &
                           "Arccoth provide correct results");

   Test_Block:
   declare

      use Ada.Exceptions;
      use Ada.Numerics;
      use FXA5A00;

      package GEF is new Ada.Numerics.Generic_Elementary_Functions(New_Float);
      package  EF renames Ada.Numerics.Elementary_Functions;

      The_Result       : Float;
      New_Float_Result : New_Float;

      procedure Dont_Optimize_Float     is new Dont_Optimize(Float);
      procedure Dont_Optimize_New_Float is new Dont_Optimize(New_Float);

   begin

      -- Testing of Cot Function, both instantiated and pre-instantiated
      -- version.

      -- Check that Constraint_Error is raised with the Cot function is
      -- given a parameter input value of 0.0.

      if New_Float'Machine_Overflows = True then
         begin
            New_Float_Result := GEF.Cot (0.0);
            Report.Failed("Constraint_Error not raised by Function Cot " &
                          "when provided a zero input parameter value");
            Dont_Optimize_New_Float(New_Float_Result, 1);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           => 
            Report.Failed("Unexpected exception raised by Function Cot " &
                          "when provided a zero input parameter value");
         end;
      end if;

      -- Check that no exception occurs on computing the Cot with very
      -- large (positive and negative) input values.

      begin 
         New_Float_Result := GEF.Cot (New_Float(FXA5A00.Large));
         Dont_Optimize_New_Float(New_Float_Result, 2);
      exception
         when others =>
           Report.Failed("Unexpected exception on GEF.Cot with large " &
                         "positive value");
      end;

      begin 
         The_Result := EF.Cot (FXA5A00.Minus_Large);
         Dont_Optimize_Float(The_Result, 3);
      exception
         when others =>
           Report.Failed("Unexpected exception on EF.Cot with large " &
                         "negative value");
      end;


      -- Check the results of the Cot function with various input parameters.

      if not (FXA5A00.Result_Within_Range(GEF.Cot(Pi/4.0),     1.0, 0.001) and
              FXA5A00.Result_Within_Range( EF.Cot(Pi/2.0),     0.0, 0.001) and
              FXA5A00.Result_Within_Range(GEF.Cot(3.0*Pi/4.0),-1.0, 0.001) and
              FXA5A00.Result_Within_Range( EF.Cot(3.0*Pi/2.0), 0.0, 0.001))
      then
         Report.Failed("Incorrect result from Cot function with various " &
                       "input parameters");
      end if;

 
      -- Check the results of the Cot function against the results of 
      -- various trigonometric relationships.

      if not FXA5A00.Result_Within_Range(GEF.Cot(New_Float(Pi/4.0)),
                                         1.0/EF.Tan(Pi/4.0),
                                         0.001)           or
         not FXA5A00.Result_Within_Range(EF.Cot(Pi/4.0),
                                         EF.Cos(Pi/4.0)/EF.Sin(Pi/4.0),
                                         0.001) or
         not FXA5A00.Result_Within_Range(EF.Cot(EF.Arccot(Pi/4.0)),
                                         Pi/4.0,
                                         0.001) 
      then
         Report.Failed("Incorrect result from Cot function with respect " &
                       "to various trigonometric relationship expected "  &
                       "results");
      end if;


      -- Testing of Cot with Cycle parameter.

      -- Check that Argument_Error is raised by the Cot function when the
      -- value of the Cycle parameter is zero or negative.

      begin
         New_Float_Result := GEF.Cot (1.0, Cycle => 0.0);
         Report.Failed("Argument_Error not raised by the Cot Function " &
                       "with a specified cycle value of 0.0");
         Dont_Optimize_New_Float(New_Float_Result, 4);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed
              ("Unexpected exception raised by the Cot Function with " &
               "a specified cycle value of 0.0");
      end;

      begin
         The_Result := EF.Cot (X => 1.0, Cycle => -360.0);
         Report.Failed("Argument_Error not raised by the Cot Function " &
                       "with a specified cycle value of -360.0");
         Dont_Optimize_Float(The_Result, 5);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed
              ("Unexpected exception raised by the Cot Function with " &
               "a specified cycle value of -360.0");
      end;


      -- Check that Constraint_Error is raised by the Cot Function with
      -- specified cycle, when the value of the parameter X is 0.0.

      if New_Float'Machine_Overflows = True then
         begin
            New_Float_Result := GEF.Cot (0.0, 360.0);
            Report.Failed("Constraint_Error not raised by Function Cot "   &
                          "with specified cycle, when value of parameter " &
                          "X is 0.0");
            Dont_Optimize_New_Float(New_Float_Result, 6);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Unexpected exception raised by Function Cot "   &
                             "with specified cycle, when value of parameter " &
                             "X is 0.0");
         end;
      end if;

      -- Check that Constraint_Error is raised by the Cot Function with
      -- specified cycle, when the value of the parameter X is a multiple
      -- of the half cycle.

      if New_Float'Machine_Overflows = True then
         begin
            New_Float_Result := GEF.Cot (180.0, 360.0);
            Report.Failed("Constraint_Error not raised by Function Cot "   &
                          "with specified cycle, when value of parameter " &
                          "X is a multiple of the half cycle (180.0, 360.0)");
            Dont_Optimize_New_Float(New_Float_Result, 7);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Unexpected exception raised by Function Cot "   &
                             "with specified cycle, when value of parameter " &
                             "X is a multiple of the half cycle" & 
                             " (180.0, 360.0)");
         end;
      end if;

      if Float'Machine_Overflows = True then
         begin
            The_Result := EF.Cot (540.0, 360.0);
            Report.Failed("Constraint_Error not raised by Function Cot "   &
                          "with specified cycle, when value of parameter " &
                          "X is a multiple of the half cycle (540.0, 360.0)");
            Dont_Optimize_Float(The_Result, 8);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Unexpected exception raised by Function Cot "   &
                             "with specified cycle, when value of parameter " &
                             "X is a multiple of the half cycle (540.0, 360.0)");
         end;
      end if;      
      
--pwb-math      -- Check that no exception occurs on computing the Cot with very
--pwb-math      -- large (positive and negative) input values.
--pwb-math
--pwb-math      begin 
--pwb-math         New_Float_Result := GEF.Cot (New_Float(FXA5A00.Large), 2.0*Pi);
--pwb-math         Dont_Optimize_New_Float(New_Float_Result, 9);
--pwb-math      exception
--pwb-math         when others =>
--pwb-math           Report.Failed("Unexpected exception on GEF.Cot with large " &
--pwb-math                         "positive value");
--pwb-math      end;
--pwb-math
--pwb-math      begin 
--pwb-math         The_Result := EF.Cot (FXA5A00.Minus_Large, Cycle => 2.0*Pi);
--pwb-math         Dont_Optimize_Float(The_Result, 10);
--pwb-math      exception
--pwb-math         when others =>
--pwb-math           Report.Failed("Unexpected exception on EF.Cot with large " &
--pwb-math                         "negative value");
--pwb-math      end;
--pwb-math
--pwb-math
--pwb-math      -- Check prescribed result from Cot function with Cycle parameter.  
--pwb-math
--pwb-math      if not FXA5A00.Result_Within_Range
--pwb-math               (GEF.Cot(New_Float(FXA5A00.Half_Pi), 2.0*Pi), 0.0, 0.001) or
--pwb-math         not FXA5A00.Result_Within_Range
--pwb-math               (EF.Cot(3.0*Pi/2.0, Cycle => 2.0*Pi), 0.0, 0.001)
--pwb-math      then
--pwb-math         Report.Failed("Incorrect result from Cot function with cycle " &
--pwb-math                       "parameter, using a multiple of Pi/2 as the "    &
--pwb-math                       "input parameter");
--pwb-math      end if;


      -- Testing of Coth Function, both instantiated and pre-instantiated
      -- version.

      -- Check that no exception occurs on computing the Coth with very
      -- large (positive and negative) input values.

      begin 
         The_Result := EF.Coth (FXA5A00.Large);
         if The_Result > 1.0 then
            Report.Failed("Result of Coth function with large positive " &
                          "value greater than 1.0");
         end if;
      exception
         when others =>
           Report.Failed("Unexpected exception on EF.Coth with large " &
                         "positive value");
      end;

      begin 
         The_Result := EF.Coth (FXA5A00.Minus_Large);
         if The_Result < -1.0 then
            Report.Failed("Result of Coth function with large negative " &
                          "value less than -1.0");
         end if;
      exception
         when others =>
           Report.Failed("Unexpected exception on EF.Coth with large " &
                         "negative value");
      end;


      -- Check that Constraint_Error is raised by the Coth function, when
      -- the value of the parameter X is 0.0.

      if New_Float'Machine_Overflows = True then
         begin
            New_Float_Result := GEF.Coth (X => 0.0);
            Report.Failed("Constraint_Error not raised by the Coth function " &
                          "when the value of parameter X is 0.0");
            Dont_Optimize_New_Float(New_Float_Result, 11);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Unexpected exception raised by the Coth " &
                             "function when the value of parameter X is 0.0");
         end;
      end if;


      -- Testing of Arccoth Function, both instantiated and pre-instantiated
      -- version.

      -- Check that Constraint_Error is raised by the Arccoth function
      -- when the absolute value of the parameter X is 1.0.

      if New_Float'Machine_Overflows = True then
         begin
            New_Float_Result := GEF.Arccoth (X => 1.0);
            Report.Failed("Constraint_Error not raised by the Arccoth " &
                          "function when the value of parameter X is 1.0");
            Dont_Optimize_New_Float(New_Float_Result, 12);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Unexpected exception raised by the Arccoth " &
                             "function when the value of parameter X is 1.0");
         end;
      end if;
      
      if Float'Machine_Overflows = True then
         begin
            The_Result := EF.Arccoth (-1.0);
            Report.Failed("Constraint_Error not raised by the Arccoth " &
                          "function when the value of parameter X is -1.0");
            Dont_Optimize_Float(The_Result, 13);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Unexpected exception raised by the Arccoth " &
                             "function when the value of parameter X is -1.0");
         end;
      end if;

      -- Check that Argument_Error is raised by the Arccoth function when
      -- the absolute value of the parameter X is less than 1.0.

      begin
         New_Float_Result := GEF.Arccoth (X => New_Float(One_Minus_Delta));
         Report.Failed("Argument_Error not raised by the Arccoth " &
                       "function with parameter value less than 1.0");
         Dont_Optimize_New_Float(New_Float_Result, 14);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Arccoth " &
                          "function with parameter value less than 1.0");
      end;

      begin
         The_Result := EF.Arccoth (X => FXA5A00.Minus_One_Plus_Delta);
         Report.Failed("Argument_Error not raised by the Arccoth function " &
                       "with parameter value between 0.0 and -1.0");
         Dont_Optimize_Float(The_Result, 15);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by the Arccoth " &
                          "function with parameter value between 0.0 "  &
                          "and -1.0");
      end;


      -- Check the results of the Arccoth function with various input 
      -- parameters.

      if not (Result_Within_Range(GEF.Arccoth(1.01), 2.652, 0.01)  and
              Result_Within_Range( EF.Arccoth(1.25), 1.099, 0.01)  and
              Result_Within_Range(GEF.Arccoth(1.56), 0.760, 0.001) and
              Result_Within_Range( EF.Arccoth(1.97), 0.560, 0.001) and
              Result_Within_Range(GEF.Arccoth(2.40), 0.444, 0.001) and
              Result_Within_Range( EF.Arccoth(4.30), 0.237, 0.001) and
              Result_Within_Range(GEF.Arccoth(5.80), 0.174, 0.001) and
              Result_Within_Range( EF.Arccoth(7.00), 0.144, 0.001))
      then
         Report.Failed("Incorrect result from Arccoth function with various " &
                       "input parameters");
      end if;


   exception
      when The_Error : others => 
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXA5A04;
