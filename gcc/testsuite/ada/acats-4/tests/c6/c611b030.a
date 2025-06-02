-- C611B030.A
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
--
-- OBJECTIVE:
--      For X'Old given in the postcondition for a subprogram S, check that
--      X'Old has the same tag as X when X is a parameter P of S, even if the
--      tag of X is different than the nominal subtype of P. (Part 3:
--      Post'Class of an interface)
--
-- TEST DESCRIPTION:
--      We declare an event counter type and an extension thereof. We include
--      function calls that are bound based on the type of the prefix, and
--      check which are called.
--
--      To check the objective, we directly check the tag. It would have been
--      more usage-oriented to test dispatching instead, but that would have
--      added more complication to the test.
--
-- TEST FILES:
--      This test consists of the following files:
--      -> C611B030.A
--         C611B031.A
--         C611B032.A
--         C611B033.AM
--
-- CHANGE HISTORY:
--      27 Dec 16   JAC     Initial pre-release version.
--      18 Jan 17   RLB     Updated the objective to be different than C611B02.
--      24 Mar 17   RLB     Fixed too long lines.
--
--!
package C611B030 is

   pragma Assertion_Policy (Check);

   type Event_Record_IF is interface;

   procedure Event_Occurred (Event : in out Event_Record_IF) is abstract
      with Post'Class => Event.Count1 = Event'Old.Count2 + 1;

   function  Count1         (Event : in     Event_Record_IF)
                                            return Integer is abstract;
   function  Count2         (Event : in     Event_Record_IF)
                                            return Integer is abstract;

end C611B030;
