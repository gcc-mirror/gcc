-- CDE0002.A
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
--    ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
--
-- OBJECTIVE:
--      Check the use of an entity in an aspect specification does not cause
--      freezing to happen too early.
--
--      Check the use of an entity in the expression of an expression function
--      freezing to happen too early.
--
-- APPLICABILITY CRITERIA:
--      All implementations must attempt to compile this test.
--
--      For implementations validating against Systems Programming Annex (C):
--        this test must execute and report PASSED.
--
--      For implementations not validating against Annex C:
--        this test may report compile time errors at one or more points
--        indicated by "-- ANX-C RQMT", in which case it may be graded as
--        inapplicable. Otherwise, the test must execute and report PASSED.
--
-- CHANGE HISTORY:
--      15 Nov 13   GRB     Initial version.
--      27 Nov 13   RLB     Added headers and ANX-C RQMT.
--      18 Apr 14   RLB     Renamed for ACATS 4.0.
--      03 Apr 17   RLB     Redid aspect cases to use Storage_Size, so the
--                          static freezing rule of 13.14(7.2/3) is not
--                          triggered.
--!

with Report;
with ImpDef;
procedure CDE0002 is

   package Aspects is
      X : constant Integer;

      type A is new Natural
         with Size => Integer'Size;         -- ANX-C RQMT {1:7;1}

      type A_Ptr is access all A
         with Storage_Size => X;            -- OK, doesn't freeze A or X.

   private
      X : constant Integer := A'Size * 16;  -- OK, not yet frozen (freezes A).
      P : A_Ptr := new A'(12);              -- Freezes A_Ptr & X.
   end Aspects;

   generic function G return Integer;

   function RG return Integer;

   C : constant := 42;

   package Expression_Functions is
      X : constant Integer;

      function FG return Integer is (RG);
      function FX return Integer is (X);    -- OK, does not freeze X
      function FA return access function return Integer is (FG'Access);

   private
      X : constant Integer := C;            -- OK not yet frozen.
   end Expression_Functions;                -- End of unit freezes everything
                                            -- inside.

   function G return Integer is
   begin
      return C;
   end G;

   function NG is new G;

   function RG return Integer renames NG;

   procedure Check_Expression_Functions is
      use Expression_Functions;
   begin
      if X /= C then
         Report.Failed ("Wrong value for deferred constant");
      end if;

      if FG /= C or else FX /= C or else FA.all /= C then
         Report.Failed ("Wrong value for expression function call");
      end if;
   exception
      when others =>
         Report.Failed ("Unexpected exception in expression function call");
   end Check_Expression_Functions;

begin
   Report.Test ("CDE0002", "Check that the use of an entity in an aspect "  &
                "specification or function expression does not cause " &
                "freezing to happen too early");

   if not (Aspects.A'Size = Integer'Size) then
      Report.Failed ("Wrong size for type with explicit Size aspect");
   end if;

   if Aspects.A_Ptr'Storage_Size not in Aspects.A'Size * 16 ..
                                        Aspects.A'Size * 16 +
                       ImpDef.Maximum_Adjustment_To_Specified_Storage_Size then
      Report.Failed ("Wrong storage size for access type with explicit " &
                     "Storage_Size aspect");
   end if;

   Check_Expression_Functions;

   Report.Result;
end CDE0002;
