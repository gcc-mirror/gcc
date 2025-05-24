-- F350B00.A
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
--     This foundation provides a number of real types with default
--     values. We assume that the default values are necessary to ensure
--     an expected initial condition.

-- CHANGE HISTORY:
--     30 Jun 2020   RLB   Created foundation.

package F350B00 is

    Flt_Value : constant := 2.25;

    type Small_Float is digits 3
        with Default_Value => Flt_Value;

    Fix_Value : constant := 3.75;

    type Small_Fixed is delta 0.125 range -4.0 .. 4.0
        with Small => 0.125, Default_Value => Fix_Value;

    Dec_Value : constant := -1.00;

    type Small_Dec is delta 0.01 digits 4 -- Range -10.00 .. 10.00
        with Default_Value => Dec_Value;

end F350B00;

