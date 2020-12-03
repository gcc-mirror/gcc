-- CD30011.A

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
--     clause for an enumeration type:
--        * in the visible or private part of a package for a type declared
--          in the visible part;
--        * for a derived enumeration type;
--        * for a derived private type whose full declaration is an
--          enumeration type.
--
-- TEST DESCRIPTION:
--     This test was created from legacy tests CD1009B and CD2A31C. The
--     objective of CD1009B was also an objective of CD2A31C; the tests
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
--      17 Jun 87  PWB  Created original test CD2A21C.
--      07 Oct 87  VCL  Created original test CD1009B.
--      17 Apr 89  DHH  Changed extension from '.DEP' TO '.ADA', changed
--                      operators on 'Size tests, and added check on
--                      representation clause.
--      26 Mar 92  JRL  Removed testing of nonobjective types.
--      29 Mar 17  RLB  Created test from CD2A21C and CD1009B; reformatted
--                      to "modern" standards, added applicability criteria.

with Report; use Report;
with Length_Check;                      -- CONTAINS A CALL TO 'Failed'.
procedure CD30011 is

   type Basic_Enum is (A, B, C, D, E);
   Specified_Size : constant := Basic_Enum'Size;

   Minimum_Size : Integer := Report.Ident_Int (Specified_Size);

   type Derived_Enum is new Basic_Enum;
   for Derived_Enum'Size use Specified_Size;                  -- ANX-C RQMT.

   package P is
      type Enum_in_P is (A1, B1, C1, D1, E1, F1, G1);
      for Enum_in_P'Size use Specified_Size;                  -- ANX-C RQMT.
      type private_Enum is private;
      type Alt_Enum_in_P is (A2, B2, C2, D2, E2, F2, G2);
   private
      type private_Enum is (A3, B3, C3, D3, E3, F3, G3);
      for Alt_Enum_in_P'Size use Specified_Size;              -- ANX-C RQMT.
   end P;

   type Derived_Private_Enum is new P.Private_Enum;
   for Derived_Private_Enum'Size use Specified_Size;          -- ANX-C RQMT.

   use P;

   procedure Check_1 is new Length_Check (Derived_Enum);
   procedure Check_2 is new Length_Check (Enum_in_P);
   procedure Check_3 is new Length_Check (Alt_Enum_in_P);

   X : Enum_in_P := A1;
   Y : Alt_Enum_in_P := A2;

begin

   Report.Test ("CD30011", "Check that 'Size attribute definition clauses " &
                           "can be given in the visible or private part " &
                           "of a package for enumeration types declared " &
                           "declared in the visible part, and for derived " &
                           "enumeration types and derived private types " &
                           "whose full declarations are as enumeration types");

   Check_1 (C,  Specified_Size, "Derived_Enum");
   Check_2 (C1, Specified_Size, "Enum_in_P");
   Check_3 (C2, Specified_Size, "Alt_Enum_in_P");

   if Derived_Enum'Size /= Minimum_Size then
      Failed ("Derived_Enum'Size should not be greater than" &
              Integer'Image (Minimum_Size) & ". Actual Size is" &
              Integer'Image (Derived_Enum'Size));
   end if;

   if Enum_in_P'Size /= Minimum_Size then
      Failed ("Enum_in_P'Size should not be greater than" &
              Integer'Image (Minimum_Size) & ". Actual Size is" &
              Integer'Image (Enum_in_P'Size));
   end if;

   if Alt_Enum_in_P'Size /= Minimum_Size then
      Failed ("Alt_Enum_in_P'Size should not be greater than" &
              Integer'Image (Minimum_Size) & ". Actual Size is" &
              Integer'Image (Alt_Enum_in_P'Size));
   end if;

   if Derived_Private_Enum'Size /= Minimum_Size then

      Failed ("Derived_Private_Enum'Size should not be greater " &
              "than " & Integer'Image (Minimum_Size) & ". Actual Size is" &
              Integer'Image (Derived_Private_Enum'Size));
   end if;

   if X'Size < Specified_Size then
      Failed ("Object'Size is too small --" &
              Enum_in_P'Image (X));
   end if;

   if Y'Size < Specified_Size then
      Failed ("Object'Size is too small --" &
              Alt_Enum_in_P'Image (Y));
   end if;

   Report.Result;

end CD30011;
