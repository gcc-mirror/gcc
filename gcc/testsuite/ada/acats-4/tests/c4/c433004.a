-- C433004.A
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
--  OBJECTIVE:
--     Check that for each association with a <> in an array aggregate, the
--     component is initialized by default. (Array types with
--     Default_Component_Values.)
--
--  TEST DESCRIPTION:
--     This test tries various aggregates containing <> for a number
--     of interesting types. We try component types that have Default_Values
--     to ensure that those aren't used instead (AI12-0084-1).
--
-- CHANGE HISTORY:
--     21 Nov 2014  RLB  Created test based on C433003.
--     13 Mar 2015  RLB  Corrected expected result for O6. Eliminated overlong
--                       lines.
--
--!
with Report;
with Ada.Finalization;
procedure C433004 is
   type Color_Type is (Unknown, Red, Orange, Yellow, Green, Blue,
                       Indigo, Violet)
      with Default_Value => Unknown;

   type Default_to_Zero_Type is range -10000 .. 10000
      with Default_Value => 0;

   type Half_Type is digits 4
      with Default_Value => 0.5;

   type TA is array (1 .. 3) of Color_Type
      with Default_Component_Value => Green;
   type TA2 is array (1 .. 2, 1 .. 4) of Color_Type
      with Default_Component_Value => Violet;

   type TB is array (1 .. 3) of Default_to_Zero_Type
      with Default_Component_Value => 12;
   type TB2 is array (1 .. 2, 1 .. 4) of Default_to_Zero_Type
      with Default_Component_Value => 52;
   type TBS is array (1 .. 2) of TB;
   type TC is array (1 .. 5) of Half_Type
      with Default_Component_Value => 2.5;


begin
   Report.Test ("C433004", "Check that for each association with a <> in an " &
                           "array aggregate, the component is initialized " &
                           "by default. (Array types with " &
                           "Default_Component_Values.)");

   declare
      O1 : TA := (others => <>);
      Result : TA := (others => Green);
   begin
      if O1 /= Result then
         if O1 = TA'(others => Unknown) then
            Report.Failed ("Wrong value - used Default_Value rather than " &
                           "Default_Component_Value: O1");
         else
            Report.Failed ("Wrong value: O1");
         end if;
      end if;
   end;

   declare
      O2 : TA := TA'(1 => Red, 2 | 3 => <>);
      Result : TA := (Red, others => Green);
   begin
      if O2 /= Result then
         if O2(3) = Unknown then
            Report.Failed ("Wrong value - used Default_Value rather than " &
                           "Default_Component_Value: O2");
         else
            Report.Failed ("Wrong value: O2");
         end if;
      end if;
   end;

   declare
      O3 : TA := (1 .. 2 => <>, 3 => Green);
      Result : TA := (others => Green);
   begin
      if O3 /= Result then
         Report.Failed ("Wrong value: O3");
      end if;
   end;

   declare
      O4 : TA := TA'(2 => <>, 1 | 3 => Blue);
      Result : TA := (Blue, Green, Blue);
   begin
      if O4 /= Result then
         Report.Failed ("Wrong value: O4");
      end if;
   end;

   declare
      O5 : TA2 := (others => (others => <>));
   begin
      if O5 /= (TA2'Range(1) => (TA2'Range(2) => Violet)) then
         Report.Failed ("Wrong value: O5");
      end if;
   end;

   declare
      O6 : TA2 := (1 => (1 .. 4 => <>), 2 => (others => Unknown));
   begin
      if O6 /= (2 => (TA2'Range(2) => Unknown), 1 => (TA2'Range(2) => Violet))
         then
         Report.Failed ("Wrong value: O6");
      end if;
   end;

   declare
      O7 : TA2 := TA2'(1 .. 2 => (1 | 3 => <>, 2 | 4 => Violet));
   begin
      if O7 /= (TA2'Range(1) => (TA2'Range(2) => Violet)) then
         Report.Failed ("Wrong value: O7");
      end if;
   end;

   declare
      O11 : TB;
      O12 : TB;
      O13 : TB;
      O14 : TB;
      O15 : TB2;
      O16 : TB2;
      O17 : TB2;
      O18 : TBS;
      O19 : TBS;
   begin
      O11 := (others => <>);
      O12 := TB'(1 => 42, 2 | 3 => <>);
      O13 := (1 .. 2 => <>, 3 => 87);
      O14 := TB'(2 => <>, 1 | 3 => 52);
      if O11 /= TB'(others => 12) then
         Report.Failed ("Wrong value: O11");
      end if;
      if O12(2) /= 12 or O12(3) /= 12 then
         Report.Failed ("Wrong value: O12");
      end if;
      if O13(1) /= 12 or O13(2) /= 12 then
         Report.Failed ("Wrong value: O13");
      end if;
      if O14 /= (52, 12, 52) then
         Report.Failed ("Wrong value: O14");
      end if;
      O15:= (others => (others => <>));
      O16:= TB2'(1 => (1 .. 4 => <>), 2 => (others => 52));
      O17:= (1 .. 2 => (1 | 3 => <>, 2 | 4 => 52));
      if O15 /= (TB2'Range(1) => (TB2'Range(2) => 52)) then
         Report.Failed ("Wrong value: O15");
      end if;
      if O16 /= (TB2'Range(1) => (TB2'Range(2) => 52)) then
         Report.Failed ("Wrong value: O16");
      end if;
      if O17 /= (TB2'Range(1) => (TB2'Range(2) => 52)) then
         Report.Failed ("Wrong value: O17");
      end if;
      O18:= (others => <>);
      O19:= TBS'(1 => <>, 2 => (others => 12));
      if O18 /= (TBS'Range => (TB'Range => 12)) then
         Report.Failed ("Wrong value: O18");
      end if;
      if O19 /= (TBS'Range => (TB'Range => 12)) then
         Report.Failed ("Wrong value: O19");
      end if;
   end;

   declare
      O31 : TC;
      O32 : TC;
      O33 : TC;
      O34 : TC;
      O35 : TC;
   begin
      O31 := (others => <>);
      O32 := TC'(1 => 0.25, 2 .. 5 => <>);
      O33 := (1 | 3 | 5 => <>, 2 | 4 => 0.75);
      O34 := TC'(1 => <>, 2 .. 5 => 2.25);
      O35 := (1 .. 5 => <>);
      if O31 /= (1 .. 5 => 2.5) then
         Report.Failed ("Wrong value: O31");
      end if;
      if O32 /= TC'(0.25, others => 2.5) then
         Report.Failed ("Wrong value: O32");
      end if;
      if O33 /= (2.5, 0.75, 2.5, 0.75, 2.5) then
         Report.Failed ("Wrong value: O33");
      end if;
      if O34 /= (2.5, 2.25, 2.25, 2.25, 2.25) then
         Report.Failed ("Wrong value: O34");
      end if;
      if O35 /= (1 .. 5 => 2.5) then
         Report.Failed ("Wrong value: O35");
      end if;
   end;

   Report.Result;
end C433004;
