-- CC510101.A
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18017 and any applicable ACAA procedures.
--
--*
--
-- OBJECTIVE
--     See CC510100.A.
--
-- TEST DESCRIPTION
--     See CC510100.A.
--
-- SPECIAL REQUIREMENTS
--     See CC510100.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         CC510100.A
--      -> CC510101.A
--         CC510102.A
--         CC510103.AM
--
-- CHANGE HISTORY:
--      06 Jan 2015   RLB   Split test into individual files.
--      17 Mar 2015   RLB   Added access to actual to instance since
--                          Elem'Unchecked_Access is illegal when Elem
--                          is incomplete. Moved body of CC51010_2 to
--                          file CC5510103.am so it didn't compile before
--                          a package specification that it depended on.
--      19 Mar 2015   RLB   Made parameter in out.
--
--!

with CC51010_0;
limited with CC51010_1;
package CC51010_2 is

   --  Instantiation with incomplete view from limited with.

   function Access_To (Obj : in out CC51010_1.Set) return access CC51010_1.Set;

   package A_Map_of_Sets
      is new CC51010_0.Incomplete_Map (Key_Type => Integer,
                                       "=" => "=",
                                       Element_Type => CC51010_1.Set,
                                       Access_To    => Access_To);

   procedure Do_Test;

end CC51010_2;


