-- CA200030.A
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
--
--*
--
-- OBJECTIVE:
--      Check that a package that is needed only because it is referenced in a
--      limited with clause is included in a partition.
--
-- TEST DESCRIPTION:
--
--      A reusable subsystem CA20003_L is developed, including a type
--      Debug_Info and an operation Display for it. A library package
--      CA20003_0 is created which takes an access to Debug_Info, but
--      does not display it at this time and defaults it to null.
--      A main program CA200033 is compiled that tests CA20003_0.
--
--      In order to determine which various units are included in the
--      partition, we also compile a unit CA20003_TC.
--
--      In this example, the subsystem accessed by limited with is used
--      only peripherally in the subsystem being tested. We expect this
--      situation to be fairly common in unit tests.
--
--      Units referenced only by a limited with are nevertheless
--      "needed" by a partition, as explained in 10.2(6.1/2), and thus are
--      included in the partition.
--
-- SPECIAL REQUIREMENTS:
--      To build this test:
--         1) Compile the file CA200030 (and include the results in the
--            environment). This is the reusable subsystem.
--            Any additional steps needed to add the limited view of the
--            reusable subsystem to the environment also must be
--            performed at this time.
--         2) Compile the file CA200031 (and include the results in the
--            environment). This is the subprogram and test code.
--         3) Build an executable image and run it.
--
-- TEST FILES:
--      This test consists of the following files:
--      -> CA200030.A
--         CA200031.AM
--
-- CHANGE HISTORY:
--     09 Apr 07   RLB     Initial test; created from LA20002.
--!

package CA20003_TC is
   -- Test state for this test.
   INITIAL : constant Natural := 2;
   FIRST_VERSION : constant Natural := 3;
   SECOND_VERSION : constant Natural := 5;
   BAD_VERSION : constant Natural := 13;

   TC_Debug_Subsystem : Natural := INITIAL;

   TC_Test_Subsystem : Natural := INITIAL;

end CA20003_TC;

package CA20003_L is

   type Debug_Info is null record; -- No info now.

   procedure Display (Dbg : in Debug_Info);

end CA20003_L;

with CA20003_TC;
package body CA20003_L is

   procedure Display (Dbg : in Debug_Info) is
   begin
      CA20003_TC.TC_Debug_Subsystem := CA20003_TC.BAD_VERSION;
   end Display;

begin
   CA20003_TC.TC_Debug_Subsystem := CA20003_TC.FIRST_VERSION;
end CA20003_L;

