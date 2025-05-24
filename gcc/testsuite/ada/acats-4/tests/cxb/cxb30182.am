-- CXB30182.AM
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
--      Check that exported Ada subprograms and objects can be used from
--      a C function. Part 2: Using Export aspects in aspect_specifications.
--
-- TEST DESCRIPTION:
--      This test checks that Ada subprograms and objects can be exported
--      and used from a C language program.
--
--      As we want to ensure the normal ACATS test control, we use an
--      Ada main subprogram that calls an imported C language function
--      CXB30180 rather than directly using C language main subprogram.
--
--      CXB30180 is imported using pragma Import. It will call an Ada procedure
--      in package CXB30181 named Ada_Doubler, which is exported using
--      pragma Export. This routine doubles all of it's parameters.
--      The C function then takes the result of the first parameter and
--      returns it adding the value of Ada object CXB30181.Global. All
--      of the results are then verified.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      packages Interfaces.C and convention C. If an
--      implementation provides package Interfaces.C and convention C,
--      this test must compile, execute, and report "PASSED".
--
--      Exception: This test presumes that the Implementation Advice in
--      B.3 is followed. In particular, the test assumes that B.3(66),
--      B.3(68), and B.3(71.2) are followed for elementary types. If following
--      this Advice is impractical, this test is not applicable to the
--      implementation. The implementer needs to advise their ACAL and the
--      ACAA if this is the case as it cannot be detected by inspecting
--      the test results; a test run that does not report PASSED or otherwise
--      fails will be graded as FAILED unless the ACAL and ACAA is advised
--      of the issue.
--
-- SPECIAL REQUIREMENTS:
--      The files CXB30180.C must be compiled with a C
--      compiler.  Implementation dialects of C may require alteration of
--      the C program syntax (see individual C files).
--
--      Note that the compiled C code must be bound with the compiled Ada
--      code to create an executable image.  An implementation must provide
--      the necessary commands to accomplish this.
--
--      Note that the C code included in CXB30180.C conforms
--      to ANSI-C.  Modifications to these files may be required for other
--      C compilers.  An implementation must provide the necessary
--      modifications to satisfy the function requirements.
--
-- TEST FILES:
--      This test consists of the following files:
--         CXB30180.C
--         CXB30181.A
--      -> CXB30182.AM
--
--
-- CHANGE HISTORY:
--      26 Mar 14   RLB    Created test.
--
--!

with Report;
with Impdef;
with Interfaces.C;                                            -- N/A => ERROR
with CXB30181;
procedure CXB30182 is
begin

   Report.Test ("CXB3018", "Check that exported Ada subprogram and object " &
                           "can be used from a C function. Part 2: " &
                           "Using aspect Export in aspect specifications");

   Test_Block:
   declare

      use type Interfaces.C.int;
      use type Interfaces.C.short;
      use type Interfaces.C.C_float;
      use type Interfaces.C.double;

      TC_Default_int      : Interfaces.C.int             :=   92;
      TC_Default_short    : Interfaces.C.short           :=   12;
      TC_Default_float    : Interfaces.C.C_float         :=   80.0;
      TC_Default_double   : Interfaces.C.double          :=  831.0;

      An_Int_Value        : Interfaces.C.int             := TC_Default_int;
      A_Short_Value       : aliased Interfaces.C.short   := TC_Default_short;
      A_Float_Value       : aliased Interfaces.C.C_float := TC_Default_float;
      A_Double_Value      : aliased Interfaces.C.double  := TC_Default_double;

      TC_Return_int       : Interfaces.C.int := 0;

      -- The Do_It function doubles the designated value of each of the access
      -- parameters and returns the double of The_Int plus the value of
      -- CXB30181.Global.

      function Do_It (The_Int    : in Interfaces.C.int;
                      The_Short  : access Interfaces.C.Short;
                      The_Float  : access Interfaces.C.C_Float;
                      The_Double : access Interfaces.C.Double)
                                    return Interfaces.C.int
         with Import => True, Convention => C,                -- N/A => ERROR
              External_Name => Impdef.CXB30180_External_Name;

   begin

      -- Change the value of the global:
      CXB30181.Global := Interfaces.C.Int(Report.Ident_Int(10));

      -- Check that the imported version of C function CXB30180 produces
      -- the correct results.

      TC_Return_int := Do_It (The_Int    => An_Int_Value,
                              The_Short  => A_Short_Value'access,
                              The_Float  => A_Float_Value'access,
                              The_Double => A_Double_Value'access);

      -- Compare the results with the expected results.  Note that in the
      -- case of the three "pointer" parameters, the objects being pointed
      -- to have been modified as a result of the function.

      if TC_Return_int           /= An_Int_Value * 2 + 10 then
         Report.Failed ("Incorrect result returned from function Do_It");
      end if;
      if A_Short_Value           /= TC_Default_short  * 2 or else
         A_Float_Value           /= TC_Default_float  * 2.0 or else
         A_Double_Value          /= TC_Default_double * 2.0
      then
         Report.Failed ("Incorrect parameter values after call to function Do_It");
      end if;
      if An_Int_Value /= TC_Default_Int then
         Report.Failed ("In parameter modified by function Do_It");
      end if;
      if CXB30181.Global /= 10 then
         Report.Failed ("Global modified by function Do_It");
      end if;

      -- Change the value of the global again:
      CXB30181.Global := Interfaces.C.Int(Report.Ident_Int(14));

      -- Check that the imported version of C function CXB30180 produces
      -- the correct results on a subsequent call.

      TC_Return_int := Do_It (The_Int    => An_Int_Value,
                              The_Short  => A_Short_Value'access,
                              The_Float  => A_Float_Value'access,
                              The_Double => A_Double_Value'access);

      -- Compare the results with the expected results.  Note that in the
      -- case of the three "pointer" parameters, the objects being pointed
      -- to have been modified as a result of the function.

      if TC_Return_int           /= An_Int_Value * 2 + 14 then
         Report.Failed ("Incorrect result returned from second call to " &
                        "function Do_It");
      end if;
      if A_Short_Value           /= TC_Default_short  * 4 or else
         A_Float_Value           /= TC_Default_float  * 4.0 or else
         A_Double_Value          /= TC_Default_double * 4.0
      then
         Report.Failed( "Incorrect parameter values after second call " &
                        "to function Do_It");
      end if;
      if An_Int_Value /= TC_Default_Int then
         Report.Failed ("In parameter modified by second call to function Do_It");
      end if;
      if CXB30181.Global /= 14 then
         Report.Failed("Global modified by second call to function Do_It");
      end if;

   end Test_Block;

   Report.Result;

end CXB30182;
