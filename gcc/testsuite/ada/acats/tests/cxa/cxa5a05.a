-- CXA5A05.A
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
--      Check that the functions Arcsin and Arcsinh provide correct 
--      results.
--
-- TEST DESCRIPTION:
--      This test examines both the version of Arcsin and Arcsinh 
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
--         CXA5A05.A
--
--
-- CHANGE HISTORY:
--      20 Mar 95   SAIC    Initial prerelease version.
--      06 Apr 95   SAIC    Corrected errors in context clause reference and
--                          use of Cycle parameter.
--      13 Jun 95   SAIC    Incorporated use of Dont_Optimize procedure, and
--                          use of Result_Within_Range function overloaded for
--                          FXA5A00.New_Float_Type.
--      28 Feb 97   PWB.CTA Removed checks with explict Cycle => 2.0*Pi
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

procedure CXA5A05 is
begin

   Report.Test ("CXA5A05", "Check that the functions Arcsin and Arcsinh " &
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

      -- Testing of Function Arcsin, both instantiated and pre-instantiated
      -- versions.

      -- Check that Argument_Error is raised by the Arcsin function when
      -- the absolute value of the parameter X is greater than 1.0.

      begin
         New_Float_Result := GEF.Arcsin(New_Float(FXA5A00.One_Plus_Delta));
         Report.Failed("Argument_Error not raised by Arcsin function " &
                       "when provided a parameter value larger than 1.0");
         Dont_Optimize_New_Float(New_Float_Result, 1);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by Arcsin function " &
                          "when provided a parameter value larger than 1.0");
      end;

      begin
         The_Result := EF.Arcsin(FXA5A00.Minus_Large);
         Report.Failed("Argument_Error not raised by Arcsin function " &
                       "when provided a large negative parameter value");
         Dont_Optimize_Float(The_Result, 2);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by Arcsin function " &
                          "when provided a large negative parameter value");
      end;


      -- Check the prescribed result of function Arcsin with parameter 0.0.

      if GEF.Arcsin(X => 0.0) /= 0.0 or
          EF.Arcsin(0.0)      /= 0.0
      then
         Report.Failed("Incorrect result from Function Arcsin when the " &
                       "value of the parameter X is 0.0");
      end if;


      -- Check the results of the Arcsin function with various input 
      -- parameters.

      if not Result_Within_Range(GEF.Arcsin(1.0),    1.571, 0.001) or
         not Result_Within_Range( EF.Arcsin(0.62),   0.669, 0.001) or
         not Result_Within_Range(GEF.Arcsin(0.01),   0.010, 0.001) or
         not Result_Within_Range( EF.Arcsin(-0.29), -0.294, 0.001) or
         not Result_Within_Range(GEF.Arcsin(-0.50), -0.524, 0.001) or
         not Result_Within_Range( EF.Arcsin(-1.0),  -1.571, 0.001)
      then
         Report.Failed("Incorrect result from Function Arcsin with " &
                       "various input parameters");
      end if;


      -- Testing of Function Arcsin with specified Cycle parameter.

--pwb-math      -- Check that Argument_Error is raised by the Arcsin function with 
--pwb-math      -- specified cycle, whenever the absolute value of the parameter X 
--pwb-math      -- is greater than 1.0.
--pwb-math
--pwb-math      begin
--pwb-math         New_Float_Result := GEF.Arcsin(New_Float(FXA5A00.Large), 2.0*Pi);
--pwb-math         Report.Failed("Argument_Error not raised by Function Arcsin " &
--pwb-math                       "with specified cycle, when provided a large "  &
--pwb-math                       "positive input parameter");
--pwb-math         Dont_Optimize_New_Float(New_Float_Result, 3);
--pwb-math      exception
--pwb-math         when Argument_Error => null;  -- OK, expected exception.
--pwb-math         when others         =>
--pwb-math            Report.Failed("Unexpected exception raised by Function Arcsin " &
--pwb-math                          "with specified cycle, when provided a large "    &
--pwb-math                          "positive input parameter");
--pwb-math      end;
--pwb-math
--pwb-math      begin
--pwb-math         The_Result := EF.Arcsin(FXA5A00.Minus_One_Minus_Delta, 2.0*Pi);
--pwb-math         Report.Failed("Argument_Error not raised by Function Arcsin " &
--pwb-math                       "with specified cycle, when provided an input " &
--pwb-math                       "parameter less than -1.0");
--pwb-math         Dont_Optimize_Float(The_Result, 4);
--pwb-math      exception
--pwb-math         when Argument_Error => null;  -- OK, expected exception.
--pwb-math         when others         =>
--pwb-math            Report.Failed("Unexpected exception raised by Function Arcsin " &
--pwb-math                          "with specified cycle, when provided an input "   &
--pwb-math                          "parameter less than -1.0");
--pwb-math      end;
--pwb-math
      -- Check that Argument_Error is raised by the Arcsin function with 
      -- specified cycle, whenever the Cycle parameter is zero or negative.

      begin
         New_Float_Result := GEF.Arcsin(2.0, 0.0);
         Report.Failed("Argument_Error not raised by Function Arcsin " &
                       "with specified cycle of 0.0");
         Dont_Optimize_New_Float(New_Float_Result, 5);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by Function Arcsin " &
                          "with specified cycle of 0.0");
      end;

      begin
         The_Result := EF.Arcsin(2.0, -2.0*Pi);
         Report.Failed("Argument_Error not raised by Function Arcsin " &
                       "with specified negative cycle parameter");
         Dont_Optimize_Float(The_Result, 6);
      exception
         when Argument_Error => null;  -- OK, expected exception.
         when others         =>
            Report.Failed("Unexpected exception raised by Function Arcsin " &
                          "with specified negative cycle parameter");
      end;


--pwb-math      -- Check the prescribed result of function Arcsin with specified Cycle
--pwb-math      -- parameter, when the value of parameter X is 0.0.
--pwb-math
--pwb-math      if GEF.Arcsin(X => 0.0, Cycle => 2.0*Pi) /= 0.0 or
--pwb-math          EF.Arcsin(0.0, 2.0*Pi)               /= 0.0
--pwb-math      then
--pwb-math         Report.Failed("Incorrect result from Function Arcsin with " &
--pwb-math                       "specified Cycle parameter, when the value "  &
--pwb-math                       "of parameter X is 0.0");
--pwb-math      end if;
--pwb-math
--pwb-math
--pwb-math      -- Test of the Arcsin function with specified Cycle parameter with 
--pwb-math      -- various input parameters.
--pwb-math
--pwb-math      if not FXA5A00.Result_Within_Range(GEF.Arcsin( 0.01, 2.0*Pi),  
--pwb-math                                         0.010,
--pwb-math                                         0.001)                    or
--pwb-math         not FXA5A00.Result_Within_Range( EF.Arcsin( 0.14, 2.0*Pi),  
--pwb-math                                         0.141,
--pwb-math                                         0.001)                    or
--pwb-math         not FXA5A00.Result_Within_Range(GEF.Arcsin( 0.37, 2.0*Pi),  
--pwb-math                                         0.379,
--pwb-math                                         0.001)                    or
--pwb-math         not FXA5A00.Result_Within_Range( EF.Arcsin( 0.55, 2.0*Pi),  
--pwb-math                                         0.582,
--pwb-math                                         0.001)                    or
--pwb-math         not FXA5A00.Result_Within_Range(GEF.Arcsin(-0.22, 2.0*Pi), 
--pwb-math                                         -0.222,
--pwb-math                                         0.001)                    or
--pwb-math         not FXA5A00.Result_Within_Range( EF.Arcsin(-0.99, 2.0*Pi), 
--pwb-math                                         -1.43,
--pwb-math                                          0.01)                    or
--pwb-math         not FXA5A00.Result_Within_Range( EF.Arcsin(1.0, 360.0), 
--pwb-math                                         90.0,
--pwb-math                                         0.1)                      or
--pwb-math         not FXA5A00.Result_Within_Range( EF.Arcsin(1.0, 100.0), 
--pwb-math                                         25.0,
--pwb-math                                         0.1)
--pwb-math      then
--pwb-math         Report.Failed("Incorrect result from Arcsin with specified " &
--pwb-math                       "cycle parameter with various input parameters");
--pwb-math      end if;

      -- Testing of Arcsinh Function, both instantiated and pre-instantiated
      -- version.

      -- Check that no exception occurs on computing the Arcsinh with very
      -- large (positive and negative) input values.

      begin 
         New_Float_Result := GEF.Arcsinh(New_Float(FXA5A00.Large));
         Dont_Optimize_New_Float(New_Float_Result, 7);
      exception
         when others =>
           Report.Failed("Unexpected exception on Arcsinh with large " &
                         "positive value");
      end;

      begin 
         The_Result := EF.Arcsinh(FXA5A00.Minus_Large);
         Dont_Optimize_Float(The_Result, 8);
      exception
         when others =>
           Report.Failed("Unexpected exception on Arcsinh with large " &
                         "negative value");
      end;

 
      -- Check that no exception occurs on computing the Arcsinh with very
      -- small (positive and negative) input values.

      begin 
         New_Float_Result := GEF.Arcsinh(New_Float(FXA5A00.Small));
         Dont_Optimize_New_Float(New_Float_Result, 9);
      exception
         when others =>
           Report.Failed("Unexpected exception on Arcsinh with small " &
                         "positive value");
      end;

      begin 
         The_Result := EF.Arcsinh(-FXA5A00.Small);
         Dont_Optimize_Float(The_Result, 10);
      exception
         when others =>
           Report.Failed("Unexpected exception on Arcsinh with small " &
                         "negative value");
      end;

 
      -- Check function Arcsinh for prescribed result with parameter 0.0.

      if GEF.Arcsinh(X => 0.0) /= 0.0 or
          EF.Arcsinh(X => 0.0) /= 0.0
      then
         Report.Failed("Incorrect result from Function Arcsinh when " &
                       "provided a 0.0 input parameter");
      end if;

 
      -- Check the results of the Arcsinh function with various input 
      -- parameters.

      if not Result_Within_Range(GEF.Arcsinh(0.15),  0.149, 0.001) or
         not Result_Within_Range( EF.Arcsinh(0.82),  0.748, 0.001) or
         not Result_Within_Range(GEF.Arcsinh(1.44),  1.161, 0.001) or
         not Result_Within_Range(GEF.Arcsinh(6.70),  2.601, 0.001) or
         not Result_Within_Range( EF.Arcsinh(Pi),    1.862, 0.001) or
         not Result_Within_Range( EF.Arcsinh(-Pi),  -1.862, 0.001) or
         not Result_Within_Range(GEF.Arcsinh(-1.0), -0.881, 0.001) or
         not Result_Within_Range( EF.Arcsinh(-5.5), -2.406, 0.001)
      then
         Report.Failed("Incorrect result from Function Arcsin with " &
                       "various input parameters");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA5A05;
