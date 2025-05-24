-- CC510102.A
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
--         CC510101.A
--      -> CC510102.A
--         CC510103.AM
--
-- CHANGE HISTORY:
--      06 Jan 2015   RLB   Split test into individual files.
--
--!


package CC51010_1 is

   type Set is tagged private;

   procedure Add (Elem : Integer; To_Set : in out Set);

   function Union (Left, Right : Set) return Set;

   function Intersection (Left, Right : Set) return Set;

   function "=" (Left, Right : Set) return Boolean;

private

   type Set_Range is range 0 .. 20;

   type Integer_Array is
      array (Set_Range range 1 .. Set_Range'Last) of Integer;

   type Set is tagged record
      Last_Integer : Set_Range := 0;
      Integers     : Integer_Array;
   end record;

end CC51010_1;


