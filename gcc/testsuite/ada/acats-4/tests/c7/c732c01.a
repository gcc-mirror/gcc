-- C732C01.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
-- OBJECTIVE:
--      Check that Assertion_Error is raised when the policy requires
--      checks and there is an object for which the invariant check fails,
--      but not when the invariant check passes - tagged type with unknown
--      discriminants version.
--
--      Check that an invariant check is never applied to a default-initialized
--      object of a type to which invariant expressions apply whose partial
--      view has unknown discriminants.

-- TEST DESCRIPTION:
--      This test is based on a program for generating "Diagrams" for
--      bellringing.  The type invariant is that each bell occurs once and only
--      once.
--
--      An invariant check fails if any enabled invariant expression for the
--      type evaluates to False. For this test, there is only one invariant
--      expression (given in the foundation) and it is enabled. (It does not
--      matter if invariants are enabled in this client subprogram; it is
--      the state for the place where the invariant is defined that matters.)
--
--      In this test, we check invariant checks that fail for:
--         (1) A return object of the type.
--         (2) An in out parameter of the type after a procedure call.
--      And that default-initialized objects do not have an invariant check,
--      as AI12-0133-1 says that the check does not occur when the partial
--      view has unknown discriminants. (In that case, any default-initialized
--      objects have to occur within the defining package, F732C00 in this
--      test.)
--
-- CHANGE HISTORY:
--      15 Jan 15   RLB     Created test from C732A01.
--      13 Mar 15   RLB     Eliminated overlong lines.
--
--!
with Ada.Assertions;
with F732C00;
with Report;

procedure C732C01 is

   pragma Assertion_Policy (Check);

   package Bells renames F732C00;


   procedure Test_Invalid_Function_Result is

      procedure Do_Invalid_Change is

         My_Invalid_Change : Bells.Change_Tagged_Type :=
           Bells.Invalid_Change;

      begin

         if Bells.Each_Bell_Occurs_Once (My_Invalid_Change) then
            Report.Failed
              ("Incorrect explicit initialization");
         end if;


      end Do_Invalid_Change;

   begin

      declare
      begin

         Do_Invalid_Change;

         Report.Failed
           ("Invalid change should have raised Assertion_Error");

      exception

         when Ada.Assertions.Assertion_Error =>

            null; -- Invariant check failed, as expected.

      end;

   end Test_Invalid_Function_Result;


   procedure Test_Valid_Function_Result is
   begin

      declare
         My_Change : Bells.Change_Tagged_Type := Bells.Rounds;

      begin

         if not Bells.Each_Bell_Occurs_Once (My_Change) then
            -- The invariant should have failed if this is True.
            Report.Failed
               ("Incorrect function return");
          end if;
      end;

   exception

      when Ada.Assertions.Assertion_Error =>

         Report.Failed
           ("Correct initialization raised Assertion_Error");
         Report.Comment
           ("Likely that default initialization failed check that should " &
            "not be performed by AI12-0133-1");

   end Test_Valid_Function_Result;


   procedure Test_Invalid_In_Out_Parameter is

      procedure Do_Invalid_In_Out_Parameter is

         My_Broken_Grandsire_Change : Bells.Change_Tagged_Type :=
                                                       Bells.Rounds;

      begin

         Bells.Broken_Grandsire_Change (My_Broken_Grandsire_Change);

         if Bells.Each_Bell_Occurs_Once (My_Broken_Grandsire_Change) then
            Report.Failed
                ("Incorrect broken Grandsire change");
          end if;

      end Do_Invalid_In_Out_Parameter;

   begin

      declare
      begin

         Do_Invalid_In_Out_Parameter;

         Report.Failed
           ("Invalid Change In Out parameter should have raised " &
            "Assertion_Error");

      exception

         when Ada.Assertions.Assertion_Error =>

            null; -- Invariant check failed, as expected.

      end;

   end Test_Invalid_In_Out_Parameter;


   procedure Test_Valid_In_Out_Parameter is

   begin

      declare
         My_Grandsire_Change : Bells.Change_Tagged_Type := Bells.Rounds;
      begin

         Bells.Grandsire_Change (My_Grandsire_Change);

         if not Bells.Each_Bell_Occurs_Once (My_Grandsire_Change) then
            -- The invariant should have failed if this is True.
            Report.Failed
               ("Incorrect Grandsire change");
         end if;
      end;

   exception

      when Ada.Assertions.Assertion_Error =>

         Report.Failed
           ("Correct parameter passing raised Assertion_Error");

   end Test_Valid_In_Out_Parameter;

begin

   Report.Test
     ("C732C01",
      "Check that Assertion_Error is raised when the policy requires " &
      "checks and there is an object for which the invariant check fails, " &
      "but not when the invariant check passes - tagged type with unknown " &
      "discriminants array version");

   Test_Invalid_Function_Result;

   Test_Valid_Function_Result;

   Test_Invalid_In_Out_Parameter;

   Test_Valid_In_Out_Parameter;

   Report.Result;

end C732C01;
