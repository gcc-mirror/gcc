-- CXA5A03.A
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
--      Check that the functions Tan, Tanh, and Arctanh provide correct 
--      results.
--
-- TEST DESCRIPTION:
--      This test examines both the version of Tan, Tanh, and Arctanh 
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
--         CXA5A03.A
--
--
-- CHANGE HISTORY:
--      14 Mar 95   SAIC    Initial prerelease version.
--      06 Apr 95   SAIC    Corrected errors in context clause references
--                          and usage of Cycle parameter.
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

procedure CXA5A03 is
begin

   Report.Test ("CXA5A03", "Check that the functions Tan, Tanh, and " &
                           "Arctanh provide correct results");

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

      -- Testing of Tan Function, both instantiated and pre-instantiated
      -- version.

      -- Check that no exception occurs on computing the Tan with very
      -- large (positive and negative) input values.

      begin 
         New_Float_Result := GEF.Tan (New_Float(FXA5A00.Large));
         Dont_Optimize_New_Float(New_Float_Result, 1);
      exception
         when others =>
           Report.Failed("Unexpected exception on GEF.Tan with large " &
                         "positive value");
      end;

      begin 
         The_Result := EF.Tan (FXA5A00.Minus_Large);
         Dont_Optimize_Float(The_Result, 2);
      exception
         when others =>
           Report.Failed("Unexpected exception on EF.Tan with large " &
                         "negative value");
      end;


      -- Check that no exception occurs on computing the Tan with very
      -- small (positive and negative) input values.

      begin 
         New_Float_Result := GEF.Tan (New_Float(FXA5A00.Small));
         Dont_Optimize_New_Float(New_Float_Result, 3);
      exception
         when others =>
           Report.Failed("Unexpected exception on GEF.Tan with small " &
                         "positive value");
      end;

      begin 
         The_Result := EF.Tan (-FXA5A00.Small);
         Dont_Optimize_Float(The_Result, 4);
      exception
         when others =>
           Report.Failed("Unexpected exception on EF.Tan with small " &
                         "negative value");
      end;


      -- Check prescribed result from Tan function.  When the parameter X 
      -- has the value zero, the Tan function yields a result of zero.

      if GEF.Tan(0.0) /= 0.0 or
          EF.Tan(0.0) /= 0.0 
      then
         Report.Failed("Incorrect result from Tan function with zero " &
                       "value input parameter");
      end if;

 
      -- Check the results of the Tan function with various input parameters.

      if not (Result_Within_Range(GEF.Tan(0.7854), 1.0,   0.001)  and
              Result_Within_Range(GEF.Tan(0.8436), 1.124, 0.001)  and
              Result_Within_Range( EF.Tan(Pi),     0.0,   0.001)  and
              Result_Within_Range( EF.Tan(-Pi),    0.0,   0.001)  and 
              Result_Within_Range(GEF.Tan(0.5381), 0.597, 0.001)  and
              Result_Within_Range( EF.Tan(0.1978), 0.200, 0.001))
      then
         Report.Failed("Incorrect result from Tan function with various " &
                       "input parameters");
      end if;

 
      -- Testing of Tan function with cycle parameter.

      -- Check that Constraint_Error is raised by the Tan function with 
      -- specified cycle, when the value of the parameter X is an odd
      -- multiple of the quarter cycle.

      if New_Float'Machine_Overflows = True then
         begin
            New_Float_Result := GEF.Tan(270.0, 360.0);                  
            Report.Failed("Constraint_Error not raised by GEF.Tan on odd " &
                          "multiple of the quarter cycle");
            Dont_Optimize_New_Float(New_Float_Result, 5);
         exception
            when Constraint_Error => null; -- OK, expected exception.
            when others =>
            Report.Failed("Unexpected exception raised by GEF.Tan on odd " &
                          "multiple of the quarter cycle");
         end;
      end if;

      -- Check that the exception Numerics.Argument_Error is raised, when
      -- the value of the parameter Cycle is zero or negative.

      begin
         New_Float_Result := GEF.Tan(X => 1.0, Cycle => -360.0);
         Report.Failed("Argument_Error not raised by GEF.Tan when Cycle " &
                       "parameter has negative value");
         Dont_Optimize_New_Float(New_Float_Result, 6);
      exception
         when Argument_Error => null; -- OK, expected exception.
         when others =>
         Report.Failed("Unexpected exception raised by GEF.Tan when Cycle " &
                       "parameter has negative value");
      end;

      begin
         The_Result := EF.Tan(1.0, Cycle => 0.0);                 
         Report.Failed("Argument_Error not raised by GEF.Tan when Cycle " &
                       "parameter has a zero value");
         Dont_Optimize_Float(The_Result, 7);
      exception
         when Argument_Error => null; -- OK, expected exception.
         when others =>
         Report.Failed("Unexpected exception raised by EF.Tan when Cycle " &
                       "parameter has a zero value");
      end;


      -- Check that no exception occurs on computing the Tan with very
      -- large (positive and negative) input values.

      begin 
         New_Float_Result := GEF.Tan (New_Float(FXA5A00.Large), 360.0);
         Dont_Optimize_New_Float(New_Float_Result, 8);
      exception
         when others =>
           Report.Failed("Unexpected exception on GEF.Tan with large " &
                         "positive value");
      end;

      begin 
         The_Result := EF.Tan (FXA5A00.Minus_Large, Cycle => 360.0);
         Dont_Optimize_Float(The_Result, 9);
      exception
         when others =>
           Report.Failed("Unexpected exception on EF.Tan with large " &
                         "negative value");
      end;


      -- Check prescribed result from Tan function with Cycle parameter.  

      if GEF.Tan(0.0, 360.0)          /= 0.0 or
          EF.Tan(0.0, Cycle => 360.0) /= 0.0 
      then
         Report.Failed("Incorrect result from Tan function with cycle " &
                       "parameter, using a zero value input parameter");
      end if;


      -- Check the Tan function, with specified Cycle parameter, with a 
      -- variety of input parameters.

      if not Result_Within_Range(GEF.Tan(30.0,  360.0),  0.577, 0.001) or 
         not Result_Within_Range( EF.Tan(57.0,  360.0),  1.540, 0.001) or 
         not Result_Within_Range(GEF.Tan(115.0, 360.0), -2.145, 0.001) or 
         not Result_Within_Range( EF.Tan(299.0, 360.0), -1.804, 0.001) or 
         not Result_Within_Range(GEF.Tan(390.0, 360.0),  0.577, 0.001) or 
         not Result_Within_Range( EF.Tan(520.0, 360.0), -0.364, 0.001)
      then
         Report.Failed("Incorrect result from the Tan function with "   &
                       "cycle parameter, with various input parameter " &
                       "values");
      end if;

      

      -- Testing of Tanh Function, both instantiated and pre-instantiated
      -- version.

      -- Check that no exception occurs on computing the Tan with very
      -- large (positive and negative) input values.

      begin 
         New_Float_Result := GEF.Tanh (New_Float(FXA5A00.Large));
         Dont_Optimize_New_Float(New_Float_Result, 10);
      exception
         when others =>
           Report.Failed("Unexpected exception on GEF.Tanh with large " &
                         "positive value");
      end;

      begin 
         The_Result := EF.Tanh (FXA5A00.Minus_Large);
         Dont_Optimize_Float(The_Result, 11);
      exception
         when others =>
           Report.Failed("Unexpected exception on EF.Tanh with large " &
                         "negative value");
      end;


      -- Check for prescribed result of Tanh with zero value input parameter.   

      if GEF.Tanh (0.0) /= 0.0 or
          EF.Tanh (0.0) /= 0.0
      then 
         Report.Failed("Incorrect result from Tanh with zero parameter");
      end if;


      -- Check the results of the Tanh function with various input 
      -- parameters.

      if not (FXA5A00.Result_Within_Range(GEF.Tanh(2.99),   0.995, 0.001)  and
              FXA5A00.Result_Within_Range(GEF.Tanh(0.130),  0.129, 0.001)  and
              FXA5A00.Result_Within_Range( EF.Tanh(Pi),     0.996, 0.001)  and
              FXA5A00.Result_Within_Range( EF.Tanh(-Pi),   -0.996, 0.001)  and
              FXA5A00.Result_Within_Range(GEF.Tanh(0.60),   0.537, 0.001)  and
              FXA5A00.Result_Within_Range( EF.Tanh(1.04),   0.778, 0.001)  and
              FXA5A00.Result_Within_Range(GEF.Tanh(1.55),   0.914, 0.001)  and
              FXA5A00.Result_Within_Range( EF.Tanh(-2.14), -0.973, 0.001))
      then
         Report.Failed("Incorrect result from Tanh function with various " &
                       "input parameters");
      end if;

 

      -- Testing of Arctanh Function, both instantiated and pre-instantiated
      -- version.

      -- Check that Constraint_Error is raised by the Arctanh function
      -- when the absolute value of the parameter X is one.

      if New_Float'Machine_Overflows = True then
         begin
            New_Float_Result := GEF.Arctanh(X =>  1.0);
            Report.Failed("Constraint_Error not raised by Function Arctanh " &
                          "when provided a parameter value of 1.0");
            Dont_Optimize_New_Float(New_Float_Result, 12);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Unexpected exception raised by Function Arctanh "
                             & "when provided a parameter value of 1.0");
         end;
      end if;

      if Float'Machine_Overflows = True then
         begin
            The_Result := EF.Arctanh(-1.0);
            Report.Failed("Constraint_Error not raised by Function Arctanh " &
                          "when provided a parameter value of -1.0");
            Dont_Optimize_Float(The_Result, 13);
         exception
            when Constraint_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Unexpected exception raised by Function Arctanh "
                             & "when provided a parameter value of -1.0");
         end;
      end if;

      -- Check that Function Arctanh raises Argument_Error when the absolute
      -- value of the parameter X exceeds one.

      begin
         New_Float_Result := GEF.Arctanh(New_Float(FXA5A00.One_Plus_Delta));
         Report.Failed("Argument_Error not raised by Function Arctanh " &
                       "when provided a parameter value greater than 1.0");
         Dont_Optimize_New_Float(New_Float_Result, 14);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Unexpected exception raised by Function Arctanh " &
                          "when provided a parameter value greater than 1.0");
      end;

 
      begin
         The_Result := EF.Arctanh(FXA5A00.Minus_One_Minus_Delta);
         Report.Failed("Argument_Error not raised by Function Arctanh " &
                       "when provided a parameter value less than -1.0");
         Dont_Optimize_Float(The_Result, 15);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Unexpected exception raised by Function Arctanh " &
                          "when provided a parameter value less than -1.0");
      end;


      begin
         New_Float_Result := GEF.Arctanh(New_Float(FXA5A00.Large));
         Report.Failed("Argument_Error not raised by Function Arctanh " &
                       "when provided a large positive parameter value");
         Dont_Optimize_New_Float(New_Float_Result, 16);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Unexpected exception raised by Function Arctanh " &
                          "when provided a large positive parameter value");
      end;

 
      begin
         The_Result := EF.Arctanh(FXA5A00.Minus_Large);
         Report.Failed("Argument_Error not raised by Function Arctanh " &
                       "when provided a large negative parameter value");
         Dont_Optimize_Float(The_Result, 17);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Unexpected exception raised by Function Arctanh " &
                          "when provided a large negative parameter value");
      end;


      -- Prescribed results for Function Arctanh with zero input value.

      if GEF.Arctanh(0.0) /= 0.0 or
          EF.Arctanh(0.0) /= 0.0
      then
         Report.Failed("Incorrect result from Function Arctanh with a " &
                       "parameter value of zero");
      end if;

 
      -- Check the results of the Arctanh function with various input 
      -- parameters.

      if not (Result_Within_Range(GEF.Arctanh(0.15), 0.151, 0.001)  and
              Result_Within_Range( EF.Arctanh(0.44), 0.472, 0.001)  and
              Result_Within_Range(GEF.Arctanh(0.81), 1.127, 0.001)  and
              Result_Within_Range( EF.Arctanh(0.99), 2.647, 0.001))
      then
         Report.Failed("Incorrect result from Arctanh function with " &
                       "various input parameters");
      end if;

   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA5A03;
