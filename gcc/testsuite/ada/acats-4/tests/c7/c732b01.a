-- C732B01.A
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
--      but not when the invariant check passes - discriminated record version.
--
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
--         (1) A default initialized object of the type (both here
--             and in the foundation package).
--         (2) A return object of the type.
--         (3) An in out parameter of the type after a procedure call.
--
-- CHANGE HISTORY:
--      25 Dec 14   JAC     Initial pre-release version.
--      20 Jan 15   JAC     Second pre-release version.
--      10 Apr 15   RLB     Cleanup for issuance.
--
--!
with Ada.Assertions;
with F732B00;
with Report;

procedure C732B01 is

   pragma Assertion_Policy (Check);

   package Bells renames F732B00;

   procedure Test_Default_Initialisation is

      procedure Do_Default_Initialisation_Inside_The_Abstraction is

         Default_Initialised_Change : constant Bells.Change_Discriminated_Type
           := Bells.Rounds_With_Default_Initialisation;

      begin

         if Bells.Each_Bell_Occurs_Once (Default_Initialised_Change) then
            Report.Failed ("Incorrect default initialisation - Inside");
            -- Mainly to use the object so it isn't subject to dead object
            -- elimination.
         end if;

      end Do_Default_Initialisation_Inside_The_Abstraction;

      procedure Do_Default_Initialisation_Here is

         Default_Initialised_Change : Bells.Change_Discriminated_Type
                                                           (Bells.No_Of_Bells);

      begin

         if Bells.Each_Bell_Occurs_Once (Default_Initialised_Change) then
            Report.Failed ("Incorrect default initialisation - Outside");
         end if;

      end Do_Default_Initialisation_Here;

   begin

      declare
      begin

         Do_Default_Initialisation_Inside_The_Abstraction;

         Report.Failed
          ("Inside default initialisation should have raised Assertion_Error");

      exception

         when Ada.Assertions.Assertion_Error =>

            null; -- Invariant check failed, as expected.

      end;

      declare
      begin

         Do_Default_Initialisation_Here;

         Report.Failed
           ("Outside default initialisation should have raised " &
            "Assertion_Error");

      exception

         when Ada.Assertions.Assertion_Error =>

            null; -- Invariant check failed, as expected.

      end;

   end Test_Default_Initialisation;


   procedure Test_Invalid_Change is

      procedure Do_Invalid_Change is

         My_Invalid_Change : constant Bells.Change_Discriminated_Type :=
           Bells.Invalid_Change;

      begin

         if Bells.Each_Bell_Occurs_Once (My_Invalid_Change) then
            Report.Failed ("Incorrect explicit initialisation");
         end if;


      end Do_Invalid_Change;

   begin

      declare
      begin

         Do_Invalid_Change;

         Report.Failed ("Invalid change should have raised Assertion_Error");

      exception

         when Ada.Assertions.Assertion_Error =>

            null; -- Invariant check failed, as expected.

      end;

   end Test_Invalid_Change;


   procedure Test_Valid_Change is

      My_Change : constant Bells.Change_Discriminated_Type := Bells.Rounds;

   begin

      if not Bells.Each_Bell_Occurs_Once (My_Change) then
         -- The invariant should have failed if this is True.
         Report.Failed ("Incorrect function return");
      end if;

   end Test_Valid_Change;

   procedure Test_Invalid_In_Out_Parameter is

      procedure Do_Invalid_In_Out_Parameter is

         My_Invalid_Grandsire_Change : Bells.Change_Discriminated_Type :=
                                                       Bells.Rounds;

      begin

         Bells.Invalid_Grandsire_Change (My_Invalid_Grandsire_Change);

         if Bells.Each_Bell_Occurs_Once (My_Invalid_Grandsire_Change) then
            Report.Failed ("Incorrect broken Grandsire change");
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

      My_Grandsire_Change : Bells.Change_Discriminated_Type := Bells.Rounds;

   begin

      Bells.Grandsire_Change (My_Grandsire_Change);

      if not Bells.Each_Bell_Occurs_Once (My_Grandsire_Change) then
         -- The invariant should have failed if this is True.
         Report.Failed ("Incorrect Grandsire change");
      end if;

   end Test_Valid_In_Out_Parameter;

begin

   Report.Test
     ("C732B01",
      "Check that Assertion_Error is raised when the policy requires " &
      "checks and there is an object for which the invariant check fails, " &
      "but not when the invariant check passes - " &
      "discriminated record version");

   Test_Default_Initialisation;

   Test_Invalid_Change;

   Test_Valid_Change;

   Test_Invalid_In_Out_Parameter;

   Test_Valid_In_Out_Parameter;

   Report.Result;

end C732B01;

