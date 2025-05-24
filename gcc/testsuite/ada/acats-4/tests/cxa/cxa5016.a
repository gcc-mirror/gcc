-- CXA5016.A
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
--      Check that the Machine_Rounding attribute is available and produces
--      the correct results.
--
-- TEST DESCRIPTION:
--      This test checks that the Machine_Rounding attribute (added by
--      AI95-00267-01) exists and is implemented properly. Since the
--      rounding behavior when the value lies exactly halfway between two
--      integers is unspecified, both results are allowed in that case.
--      We try both stand-alone uses of the attribute as well as within
--      an integer type conversion (an expected usage, and one that may
--      generate special code).
--
--      This test is based on the previous test (CXA5015) for rounding
--      attributes.
--
-- CHANGE HISTORY:
--      30 Dec 14   RLB     Created test from CXA5015.
--      13 Mar 15   RLB     Eliminate overlong lines.
--!

with Report;
procedure CXA5016 is

   subtype Float_Subtype   is Float range -10.0..10.0;
   type    Derived_Float_1 is digits  8;
   type    Derived_Float_2 is new Derived_Float_1 range -10.0..10.0E10;

   use type Float, Float_Subtype, Derived_Float_1, Derived_Float_2;

   TC_Integer   : Integer;
   TC_Float     : Float;
   TC_SFloat    : Float_Subtype;
   TC_DFloat_1  : Derived_Float_1;
   TC_DFloat_2  : Derived_Float_2;

begin

   Report.Test ("CXA5016", "Check that the Machine_Rounding attribute is " &
                           "available and produces the correct results");

   -- Check the S'Machine_Rounding attribute.

   TC_Float    :=  0.49;
   TC_SFloat   :=  1.00;
   TC_DFloat_1 :=  2.50;
   TC_DFloat_2 := -2.50;

   if Float'Machine_Rounding(TC_Float)               /=  0.0  or
      Float_Subtype'Machine_Rounding(TC_SFloat)      /=  1.0  then
      Report.Failed ("Incorrect result from the 'Machine_Rounding " &
                     "attribute (1)");
   end if;

   TC_DFloat_1 := Derived_Float_1'Machine_Rounding(TC_DFloat_1);

   if TC_DFloat_1 /= 3.0 and TC_DFloat_1 /= 2.0 then -- Either result allowed
      Report.Failed ("Incorrect result from the 'Machine_Rounding " &
                     "attribute (2)");
   end if;

   TC_DFloat_2 := Derived_Float_2'Machine_Rounding(TC_DFloat_2);

   if TC_DFloat_2 /= -3.0 and TC_DFloat_2 /= -2.0 then -- Either result allowed
      Report.Failed ("Incorrect result from the 'Machine_Rounding " &
                     "attribute (3)");
   end if;

   -- Expected usage within an integer type conversion:

   TC_Float    :=  0.50;
   TC_SFloat   :=  1.51;

   TC_Integer := Integer(Float'Machine_Rounding(TC_Float));

   if TC_Integer /= 0 and TC_Integer /= 1 then -- Either result allowed
      Report.Failed ("Incorrect result from the 'Machine_Rounding " &
                     "attribute (4)");
   end if;

   TC_Integer := Integer(Float_Subtype'Machine_Rounding(TC_SFloat));

   if TC_Integer /= 2 then
      Report.Failed ("Incorrect result from the 'Machine_Rounding " &
                     "attribute (5)");
   end if;

   Report.Result;

end CXA5016;
