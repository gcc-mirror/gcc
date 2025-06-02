-- C433003.A
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
--     component is initialized by default. (Scalar types with Default_Values.)
--
--  TEST DESCRIPTION:
--     This test tries various aggregates containing <> for a number
--     of interesting types.
--
-- CHANGE HISTORY:
--     21 Nov 2014  RLB  Created test based on ideas from C433A04.
--     13 Mar 2015  RLB  Eliminate overlong lines.
--
--!
with Report;
with Ada.Finalization;
procedure C433003 is
   type Color_Type is (Unknown, Red, Orange, Yellow, Green, Blue,
      Indigo, Violet)
      with Default_Value => Unknown;

   type Default_to_Zero_Type is range -10000 .. 10000
      with Default_Value => 0;

   type Half_Type is digits 4
      with Default_Value => 0.5;

   type TA is array (1 .. 3) of Color_Type;
   type TA2 is array (1 .. 2, 1 .. 4) of Color_Type;

   type TB is array (1 .. 3) of Default_to_Zero_Type;
   type TB2 is array (1 .. 2, 1 .. 4) of Default_to_Zero_Type;
   type TBS is array (1 .. 2) of TB;
   type TC is array (1 .. 5) of Half_Type;

begin
   Report.Test ("C433003", "Check that for each association with a <> in an " &
                           "array aggregate, the component is initialized " &
                           "by default. (Scalar types with Default_Values.)");

   declare
      O1 : TA := (others => <>);
      Result : TA := (others => Unknown);
   begin
      if O1 /= Result then
         Report.Failed ("Wrong value: O1");
      end if;
   end;

   declare
      O2 : TA := TA'(1 => Green, 2 | 3 => <>);
      Result : TA := (Green, others => Unknown);
   begin
      if O2 /= Result then
         Report.Failed ("Wrong value: O2");
      end if;
   end;

   declare
      O3 : TA := (1 .. 2 => <>, 3 => Unknown);
      Result : TA := (others => Unknown);
   begin
      if O3 /= Result then
         Report.Failed ("Wrong value: O3");
      end if;
   end;

   declare
      O4 : TA := TA'(2 => <>, 1 | 3 => Blue);
      Result : TA := (Blue, Unknown, Blue);
   begin
      if O4 /= Result then
         Report.Failed ("Wrong value: O4");
      end if;
   end;

   declare
      O5 : TA2 := (others => (others => <>));
   begin
      if O5 /= (TA2'Range(1) => (TA2'Range(2) => Unknown)) then
         Report.Failed ("Wrong value: O5");
      end if;
   end;

   declare
      O6 : TA2 := (1 => (1 .. 4 => <>), 2 => (others => Unknown));
   begin
      if O6 /= (TA2'Range(1) => (TA2'Range(2) => Unknown)) then
         Report.Failed ("Wrong value: O6");
      end if;
   end;

   declare
      O7 : TA2 := TA2'(1 .. 2 => (1 | 3 => <>, 2 | 4 => Unknown));
   begin
      if O7 /= (TA2'Range(1) => (TA2'Range(2) => Unknown)) then
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
      O12 := TB'(1 => 12, 2 | 3 => <>);
      O13 := (1 .. 2 => <>, 3 => 87);
      O14 := TB'(2 => <>, 1 | 3 => 52);
      if O11 /= TB'(others => 0) then
         Report.Failed ("Wrong value: O11");
      end if;
      if O12(2) /= 0 or O12(3) /= 0 then
         Report.Failed ("Wrong value: O12");
      end if;
      if O13(1) /= 0 or O13(2) /= 0 then
         Report.Failed ("Wrong value: O13");
      end if;
      if O14(2) /= 0 then
         Report.Failed ("Wrong value: O14");
      end if;
      O15:= (others => (others => <>));
      O16:= TB2'(1 => (1 .. 4 => <>), 2 => (others => 0));
      O17:= (1 .. 2 => (1 | 3 => <>, 2 | 4 => 0));
      if O15 /= (TB2'Range(1) => (TB2'Range(2) => 0)) then
         Report.Failed ("Wrong value: O15");
      end if;
      if O16 /= (TB2'Range(1) => (TB2'Range(2) => 0)) then
         Report.Failed ("Wrong value: O16");
      end if;
      if O17 /= (TB2'Range(1) => (TB2'Range(2) => 0)) then
         Report.Failed ("Wrong value: O17");
      end if;
      O18:= (others => <>);
      O19:= TBS'(1 => <>, 2 => (others => 0));
      if O18 /= (TBS'Range => (TB'Range => 0)) then
         Report.Failed ("Wrong value: O18");
      end if;
      if O19 /= (TBS'Range => (TB'Range => 0)) then
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
      O33 := (1 | 2 | 5 => <>, 3 | 4 => 0.75);
      O34 := TC'(1 => <>, 2 .. 5 => 2.25);
      O35 := (1 .. 5 => <>);
      if O31 /= (1 .. 5 => 0.5) then
         Report.Failed ("Wrong value: O31");
      end if;
      if O32 /= TC'(0.25, others => 0.5) then
         Report.Failed ("Wrong value: O32");
      end if;
      if O33 /= (0.5, 0.5, 0.75, 0.75, 0.5) then
         Report.Failed ("Wrong value: O33");
      end if;
      if O34 /= (0.5, 2.25, 2.25, 2.25, 2.25) then
         Report.Failed ("Wrong value: O34");
      end if;
      if O35 /= (1 .. 5 => 0.5) then
         Report.Failed ("Wrong value: O35");
      end if;
   end;

   Report.Result;
end C433003;
