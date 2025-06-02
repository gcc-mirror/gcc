-- C431A02.A
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
--      Check that if a <> in a record or extension aggregate has
--      multiple associated components with the same type and
--      default expression, the default expression is evaluated for
--      each one.
--
--  CHANGE HISTORY:
--     26 Sep 2007 RLB Created test.
--
--!
with Report;
with F431A00; use F431A00;
with Ada.Finalization;
procedure C431A03 is
   type Root is tagged limited record
       X, Y : F431A00.Ctrl_Rec :=
           (Ada.Finalization.Controlled with Info => F431A00.Init_Func (80));
   end record;
   type Child1 is new Root with record
       A, B, C : Integer := F431A00.Init_Func (51);
   end record;

begin
   Report.Test ("C431A03", "Check that if a <> in a record or extension aggregate " &
                           "has multiple associated components with the same type " &
                           "and default expression, the default expression is " &
                           "evaluated for each one");

   F431A00.Reset_Init;
   declare
      O1 : Root := (X | Y => <>);
   begin
      F431A00.Check_Init_Count (Expected => 2, Message => "O1");
      if O1.X.Info /= 80 then
         Report.Failed ("Wrong initialization value for O1.X");
      end if;
      if O1.Y.Info /= 80 then
         Report.Failed ("Wrong initialization value for O1.Y");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O2 : Root := (others => <>);
   begin
      F431A00.Check_Init_Count (Expected => 2, Message => "O2");
      if O2.X.Info /= 80 then
         Report.Failed ("Wrong initialization value for O2.X");
      end if;
      if O2.Y.Info /= 80 then
         Report.Failed ("Wrong initialization value for O2.Y");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O3 : Child1 := (X | Y => <>, A | B | C => <>);
   begin
      F431A00.Check_Init_Count (Expected => 5, Message => "O3");
      if O3.X.Info /= 80 then
         Report.Failed ("Wrong initialization value for O3.X");
      end if;
      if O3.Y.Info /= 80 then
         Report.Failed ("Wrong initialization value for O3.Y");
      end if;
      if O3.A /= 51 then
         Report.Failed ("Wrong initialization value for O3.A");
      end if;
      if O3.B /= 51 then
         Report.Failed ("Wrong initialization value for O3.B");
      end if;
      if O3.C /= 51 then
         Report.Failed ("Wrong initialization value for O3.C");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O4 : Child1 := (X | Y => <>, others => <>);
   begin
      F431A00.Check_Init_Count (Expected => 5, Message => "O4");
      if O4.X.Info /= 80 then
         Report.Failed ("Wrong initialization value for O4.X");
      end if;
      if O4.Y.Info /= 80 then
         Report.Failed ("Wrong initialization value for O4.Y");
      end if;
      if O4.A /= 51 then
         Report.Failed ("Wrong initialization value for O4.A");
      end if;
      if O4.B /= 51 then
         Report.Failed ("Wrong initialization value for O4.B");
      end if;
      if O4.C /= 51 then
         Report.Failed ("Wrong initialization value for O4.C");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O5 : Child1 := (Root with A | B | C => <>);
   begin
      F431A00.Check_Init_Count (Expected => 5, Message => "O5");
      if O5.X.Info /= 80 then
         Report.Failed ("Wrong initialization value for O5.X");
      end if;
      if O5.Y.Info /= 80 then
         Report.Failed ("Wrong initialization value for O5.Y");
      end if;
      if O5.A /= 51 then
         Report.Failed ("Wrong initialization value for O5.A");
      end if;
      if O5.B /= 51 then
         Report.Failed ("Wrong initialization value for O5.B");
      end if;
      if O5.C /= 51 then
         Report.Failed ("Wrong initialization value for O5.C");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O6 : Child1 := (Root with others => <>);
   begin
      F431A00.Check_Init_Count (Expected => 5, Message => "O6");
      if O6.X.Info /= 80 then
         Report.Failed ("Wrong initialization value for O6.X");
      end if;
      if O6.Y.Info /= 80 then
         Report.Failed ("Wrong initialization value for O6.Y");
      end if;
      if O6.A /= 51 then
         Report.Failed ("Wrong initialization value for O6.A");
      end if;
      if O6.B /= 51 then
         Report.Failed ("Wrong initialization value for O6.B");
      end if;
      if O6.C /= 51 then
         Report.Failed ("Wrong initialization value for O6.C");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O7 : Child1 := (Root with B | C => <>, A => 10);
   begin
      F431A00.Check_Init_Count (Expected => 4, Message => "O7");
      if O7.X.Info /= 80 then
         Report.Failed ("Wrong initialization value for O7.X");
      end if;
      if O7.Y.Info /= 80 then
         Report.Failed ("Wrong initialization value for O7.Y");
      end if;
      if O7.A /= 10 then
         Report.Failed ("Wrong initialization value for O7.A");
      end if;
      if O7.B /= 51 then
         Report.Failed ("Wrong initialization value for O7.B");
      end if;
      if O7.C /= 51 then
         Report.Failed ("Wrong initialization value for O7.C");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O8 : Child1 := (Root with 20, others => <>);
   begin
      F431A00.Check_Init_Count (Expected => 4, Message => "O8");
      if O8.X.Info /= 80 then
         Report.Failed ("Wrong initialization value for O8.X");
      end if;
      if O8.Y.Info /= 80 then
         Report.Failed ("Wrong initialization value for O8.Y");
      end if;
      if O8.A /= 20 then
         Report.Failed ("Wrong initialization value for O8.A");
      end if;
      if O8.B /= 51 then
         Report.Failed ("Wrong initialization value for O8.B");
      end if;
      if O8.C /= 51 then
         Report.Failed ("Wrong initialization value for O8.C");
      end if;
   end;

   Report.Result;
end C431A03;
