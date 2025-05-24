-- C3A0016.A
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
-- OBJECTIVE:
--     Check that a constant access-to-variable value can be used to
--     modify the designated object.
--
-- TEST DESCRIPTION:
--     Create various constant objects of various access-to-variable types
--     (named pool-specific, named general, anonymous), and modify the
--     designated object.
--
-- CHANGE HISTORY:
--     26 Mar 2008 RLB Created test to replace C38006A.
--
with Report;
procedure C3A0016 is

   type AI is access Integer;

   CAI : constant AI := new Integer'(0);

   FO : aliased Float := 1.0;

   type Rec is record
      AF_Comp : access Float := FO'access;
      Val : Natural := 1;
   end record;

   RO : aliased Rec;

   CR : constant Rec := (others => <>);

   type GAR is access all Rec;

   CAR : constant GAR := RO'access;

   BO : aliased Boolean := False;

   CAB : constant access Boolean := BO'access;
      -- Note: "access constant" is something very different!

begin
   Report.Test ("C3A0016", "Check that a constant access-to-variable value " &
                           "can be used to modify the designated object");

   for I in 1 .. 10 loop
      CAI.all := CAI.all + 1;
      CAB.all := not CAB.all;
      CAR.Val := Report.Ident_Int(I);
      CR.AF_Comp.all := Float(CAR.Val);
      if CAI.all /= I then
         Report.Failed ("Integer object accessed thru constant not changed");
         exit;
      end if;
      if CAB.all /= (I mod 2 /= 0) then
         Report.Failed ("Boolean object accessed thru constant not changed");
         exit;
      end if;
      if CAR.Val /= I then
         Report.Failed ("Record component accessed thru constant not changed");
         exit;
      end if;
      if CR.AF_Comp.all /= Float(I) then
         Report.Failed ("Float object accessed thru constant record component " &
                        "not changed");
         exit;
      end if;
   end loop;

   Report.Result;

end C3A0016;
