-- C452002.A
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
--     Check that Ada 2012 memberships can be resolved even when there are
--     many choices that are overloaded.
--
-- TEST DESCRIPTION:
--     We declare three enumeration types with overlapping sets of literals,
--     and try various membership operations where the tested expression is
--     an overloaded function call. The enumeration types are the classic
--     example of enumeration literal overloading; experience shows that
--     such overloading is quite common in practice.
--
--     This test concentrates on sets of enumeration literals, one of the
--     most important uses of the new membership forms, and one of the most
--     likely to involve overloading. While the overloaded parameterless
--     tested expression isn't likely to occur in practice, similar overloading
--     of more complex expressions is possible, and the rest of the membership
--     is common enough that the combination certainly will appear in real
--     programs.
--
-- CHANGE HISTORY:
--    24 Mar 2017   RLB   Created test from overnight fever dream. :-)
--    27 Mar 2017   RLB   Fixed subtest 7, which had reversed the condition.
--    13 Dec 2017   RLB   Corrected the test number passed to Report.
--
--!

with Report;
procedure C452002 is

   type Month is (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
   type Radix is (Bin, Oct, Dec, Hex);
   type Shape is (Tri, Sqr, Pnt, Hex, Oct);
      -- Oct is defined for all three types; Dec for all but Shape; and Hex for
      -- all but Month.

   -- Three identical functions, one for each type. These provide no
   -- overloading information at all.
   function Item return Month is
   begin
      return Aug;
   end Item;

   function Item return Radix is
   begin
      return Dec;
   end Item;

   function Item return Shape is
   begin
      return Hex;
   end Item;


begin

   Report.Test
     ("C452002",
      "Check that Ada 2012 memberships can be resolved even when there are " &
      "many choices that are overloaded");

   -- No overloading in the choices:
   if Item in Jan .. Mar then -- type Month
      Report.Failed ("Wrong result - no choice overloading (1)");
   end if;

   if Item in Tri | Sqr | Pnt then -- type Radix
      Report.Failed ("Wrong result - no choice overloading (2)");
   end if;

   -- A single overloaded choice:
   if Item not in May .. Oct then -- type Month
      Report.Failed ("Wrong result - single overloaded choice (3)");
   end if;

   if Item not in Bin | Dec then -- type Radix
      Report.Failed ("Wrong result - single overloaded choice (4)");
   end if;

   if Item not in Tri | Sqr | Hex then -- type Shape
      Report.Failed ("Wrong result - single overloaded choice (5)");
   end if;

   -- At least one choice without overloading:
   if Item in Jan | Oct .. Dec then -- type Month
      Report.Failed ("Wrong result - a non-overloaded choice (6)");
   end if;

   if Item in Hex | Oct | Bin then -- type Radix
      Report.Failed ("Wrong result - a non-overloaded choice (7)");
   end if;

   if Item not in Oct | Sqr | Hex then -- type Shape
      Report.Failed ("Wrong result - a non-overloaded choice (8)");
   end if;

   if Item not in Oct | Sqr | Hex | Tri then -- type Shape
      Report.Failed ("Wrong result - a non-overloaded choice (9)");
   end if;

   if Item not in Dec | Hex | Oct | Bin then -- type Radix
      Report.Failed ("Wrong result - a non-overloaded choice (10)");
   end if;

   -- The ultimate: everything is overloaded, but there still is only
   -- one possible solution.
   if Item not in Oct | Dec | Hex then -- type Radix
      Report.Failed ("Wrong result - everything overloaded (11)");
   end if;

   Report.Result;

end C452002;
