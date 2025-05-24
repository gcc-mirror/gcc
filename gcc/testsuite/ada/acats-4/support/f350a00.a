-- F350A00.A
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
-- FOUNDATION DESCRIPTION:
--     This foundation provides a number of discrete types with default
--     values. We assume that the default values are necessary to ensure
--     an expected initial condition.
--
--     We use default values that are non-zero to minimize the chance that
--     we'll accidentally properly initialize objects (some targets initialize
--     new memory to zero). For the Integer and Modular types, we use defaults
--     that are similar to the values used when pragma Normalize_Scalars is
--     in effect.

-- CHANGE HISTORY:
--     29 Jun 2020   RLB   Created foundation.

package F350A00 is

    type Status is (Raw, Bound, Solved, Unknown)
        with Default_Value => Unknown;
      
    Unused : constant := -16#6789#;
    type Small_Integer is range -16#8000#..16#7FFF#
        with Default_Value => Unused;
    subtype Small_Natural is Small_Integer range 0 .. Small_Integer'Last;
      
    type Word_16 is mod 2**16
        with Default_Value => 16#DEAD#;
   
end F350A00;

