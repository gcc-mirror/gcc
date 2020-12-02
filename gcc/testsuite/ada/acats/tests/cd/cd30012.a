-- CD30012.A

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
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
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
-- OBJECTIVE:
--     Check that a size specification can be given by an attribute definition
--     clause for an integer type:
--        * in the visible or private part of a package for a type declared
--          in the visible part;
--        * for a derived integer type;
--        * for a derived private type whose full declaration is an
--          integer type.
--
-- TEST DESCRIPTION:
--     This test was created from legacy tests CD1009B and CD2A31C. The
--     objective of CD1009B was also an objective of CD30012; the tests
--     were merged to eliminate duplication and add appropriate applicability
--     criteria.
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
--      17 Jun 87  PWB  Created original test CD2A31C.
--      09 Sep 87  VCL  Created original test CD1009A.
--      06 Apr 89  DHH  Changed extension from '.DEP' TO '.ADA', changed
--                      size clause value to 9, and added representation
--                      clause check and included test for for integer in a
--                      generic unit.
--      27 Mar 92  JRL  Removed testing of nonobjective types.
--      17 Jun 92  DTN  Removed the length clause for type Private_Int.
--      29 Mar 17  RLB  Created test from CD2A31C and CD1009A; reformatted
--                      to "modern" standards, added applicability criteria,
--                      removed nonobjective packed array.

with Report; use Report;
with Length_Check;                      -- Contains a call to 'Failed'.
procedure CD30012 is

   type Basic_Int is range -60 .. 80;
   Specified_Size : constant := 9;

   type Derived_Int is new Basic_Int;
   for Derived_Int'Size use Specified_Size;                   -- ANX-C RQMT.

   package P is
      type Int_in_P is range -125 .. 125;
      for Int_in_P'Size use Specified_Size;                   -- ANX-C RQMT.
      type Private_Int is private;
      type Alt_Int_in_P is range -125 .. 125;
   private
      type Private_Int is range -125 .. 125;
      for Alt_Int_in_P'Size use Specified_Size;               -- ANX-C RQMT.
   end P;

   use P;
   type Derived_Private_Int is new Private_Int;
   for Derived_Private_Int'Size use Specified_Size;           -- ANX-C RQMT.
   Minimum_Size : Integer := Report.Ident_Int (Specified_Size);

   -- Size specification given in a generic procedure:

   generic
   procedure Genproc;

   procedure Genproc is
      type Check_Int is range -125 .. 125;
      for Check_Int'Size use Specified_Size;                  -- ANX-C RQMT.

      procedure Check_4 is new Length_Check (Check_Int);

   begin

      if Check_Int'Size /= Minimum_Size then
         Failed ("Generic Check_Int'Size is incorrect");
      end if;
      Check_4 (-60, 9, "generic Check_Int");

   end Genproc;

   procedure Newproc is new Genproc;

   procedure Check_1 is new Length_Check (Derived_Int);
   procedure Check_2 is new Length_Check (Int_in_P);
   procedure Check_3 is new Length_Check (Alt_Int_in_P);

   Obj1 : Int_in_P := 92;
   Obj2 : Alt_Int_in_P := 52;

begin

   Report.Test ("CD30012", "Check that 'Size attribute definition clauses " &
                           "can be given in the visible or private part " &
                           "of a package for integer types declared " &
                           "declared in the visible part, and for derived " &
                           "integer types and derived private types " &
                           "whose full declarations are as integer types");

   Check_1 (-60, 9, "Derived_Int");
   Check_2 (-60, 9, "Int_in_P");
   Check_3 (-60, 9, "Alt_Int_in_P");
   Check_2 (Obj1, 9, "Int_in_P");
   Check_3 (Obj2, 9, "Alt_Int_in_P");

   Newproc;

   if Derived_Int'Size /= Minimum_Size then
        Failed ("Derived_Int'Size incorrect");
   end if;

   if Int_in_P'Size /= Minimum_Size then
        Failed ("Int_in_P'Size incorrect");
   end if;

   if Alt_Int_in_P'Size /= Minimum_Size then
        Failed ("Alt_Int_in_P'Size incorrect");
   end if;

   if Derived_Private_Int'Size /= Minimum_Size then
        Failed ("Derived_Private_Int'Size incorrect");
   end if;

   if Obj1'Size < Specified_Size then
      Failed ("Object'Size is too small --" &
              Int_in_P'Image (Obj1));
   end if;

   if Obj2'Size < Specified_Size then
      Failed ("Object'Size is too small --" &
              Alt_Int_in_P'Image (Obj2));
   end if;

   Report.Result;

end CD30012;
