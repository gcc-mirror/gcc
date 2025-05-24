-- C452A03.A
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
-- OBJECTIVES:
--     Check that "=" for all language-defined nonlimited private types
--     behaves as if it is predefined for composition and for
--     formal types - Part 3: Annex C types.
--
-- TEST DESCRIPTION:
--     This test also checks for reemergence of some other operation in a
--     generic, and for use in the equality for an array type.
--
--     The foundation is used to do the actual checks; this unit just has
--     to set up appropriate values.
--
--     Notes: We only need to test private types here, as language-defined
--     scalar type equality is tested elsewhere.
--
--     This test could pass even if the type is incorrectly implemented if
--     the predefined equality and the user-defined equality happen to get
--     the same result for the values used in this test. With the function
--     being tested returning only two distinct values, that is definitely
--     possible.
--
--     This test is less likely to fail for a correct Ada 2012 implementation
--     than for an implementation for earlier versions of Ada, as Ada 2012
--     requires all record types to compose properly (this was only true of
--     tagged types in earlier versions).
--
-- APPLICABILITY CRITERIA:
--     All implementations must attempt to compile this test.
--
--     For implementations validating against Systems Programming Annex (C):
--        this test must execute and report PASSED.
--
--     or implementations not validating against Annex C:
--        this test may report compile time errors at one or more points
--        indicated by "-- ANX-C RQMT", in which case it may be graded as
--        inapplicable. Otherwise, the test must execute and report PASSED.
--
-- CHANGE HISTORY:
--     25 JAN 2001   PHL   Initial version.
--     19 Dec 2018   RLB   Created test from submitted version.
--     20 Dec 2018   RLB   Added Null_Task_Id subtest. Revised to use Impdef
--                         values for delays.

with Ada.Task_Identification;                                -- ANX-C RQMT. {1}

with Report;
use Report;
with Impdef;
with F452A00;
procedure C452A03 is
begin
    Test ("C452A03",
          "Check that ""="" for all language-defined nonlimited types " &
          "behaves as if it is predefined for composition and for " &
          "formal types - Part 3: Annex C types");

    Task_Id:
    declare
        package Ati renames Ada.Task_Identification;

        task T1;
        task body T1 is
        begin
            delay Impdef.Switch_To_New_Task*2;
        end T1;

        task T2;
        task body T2 is
        begin
            delay Impdef.Switch_To_New_Task;
        end T2;

        C1 : constant Ati.Task_Id := T1'Identity;            -- ANX-C RQMT. {1}
        C2 : Ati.Task_Id;
        C3 : Ati.Task_Id;

        package Inst1 is new F452A00 ("Task_Id_1", Ati.Task_Id, C1, C2, C3);

        package Inst2 is new F452A00 ("Task_Id_2", Ati.Task_Id,
                                      Ati.Null_Task_Id, C2, C3);
    begin
        for Subtest in 1 .. 2 loop
            if Equal (Subtest, 1) then -- Optimization blocker.
                C2 := T1'Identity;
                C3 := T2'Identity;
            else
                C2 := Ati.Null_Task_Id;
                C3 := T1'Identity;
            end if;
            if Equal (Subtest, 2) then -- Optimization blocker.
                Inst2.Check;
            else
                Inst1.Check;
            end if;
        end loop;
    end Task_Id;

    Result;
end C452A03;

