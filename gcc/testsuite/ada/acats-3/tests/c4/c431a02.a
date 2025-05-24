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
--      Check that for each association with a <> in a record aggregate or
--      extension aggregate, if the associated component has a default
--      expression, that expression is used and not the default
--      initialization of the type of the component.
--
--      Check that for each association with a <> in a record aggregate or
--      extension aggregate, if the associated component does not have a
--      default expression, the component is initialized by default.
--
--  TEST DESCRIPTION:
--      This test declares objects of a set of limited tagged extensions,
--      using component types declared in the foundation. The foundation
--      provides routines for checking that complex components are properly
--      initialized.
--
--      For initialized by default, we test record types, access types,
--      and array types of them. Scalar types are left uninitialized when
--      initialized by default, and thus are untestable.
--
-- CHANGE HISTORY:
--     25 Sep 2007 RLB Created test.
--     27 Sep 2007 RLB Corrected test errors noted by Gary Dismukes.
--
--!
with Report;
with F431A00; use F431A00;
with Ada.Finalization;
with TcTouch;
procedure C431A02 is
   type Root is tagged limited record
       R : F431A00.My_Rec;
   end record;
   type Child1 is new Root with record
       C : F431A00.Ctrl_Rec := (Ada.Finalization.Controlled with Info => 80);
   end record;
   type Child2 is new Root with record
       C : F431A00.Ctrl_Rec;
   end record;
   type Child3 is new Child1 with record
       I : Integer := F431A00.Init_Func (51);
       S : String(1..10) := "1234567890";
   end record;
   type Child4 is new Child1 with record
       A : F431A00.Acc_NLRec;
       B : F431A00.Arr_NLRec;
   end record;
   type Arr_Acc is array (1..2) of F431A00.Acc_NLRec;
   type Child5 is new Child1 with record
       A : access Integer := new Integer'(92);
       B : Arr_Acc;
   end record;
   type Child6 is new Child1 with record
       A : F431A00.Arr_NLRec :=
           (others => (Ada.Finalization.Controlled with Info => 31));
   end record;

begin
   Report.Test ("C431A02", "Check that for each association with a <> in " &
                           "a record or extension aggregate, that the " &
                           "component is initialized with the component's " &
                           "default expression if one exists, and is " &
                           "initialized by default otherwise");

   TcTouch.Flush;
   F431A00.Reset_Init;
   declare
      O1 : Child1 := (R => <>, C => <>);
   begin
      F431A00.Check_Init_Count (Expected => 1, Message => "O1");
      TcTouch.Validate ("", Message => "O1");
      F431A00.Check (O1.R, 4, "O1.R");
      if O1.C.Info = 33 then
         Report.Failed ("Used default initialization for O1 rather than " &
                        "default expreesion");
      elsif O1.C.Info /= 80 then
         Report.Failed ("Wrong initialization value for O1");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O2 : Child1 := (Root with C => <>);
   begin
      F431A00.Check_Init_Count (Expected => 1, Message => "O2");
      TcTouch.Validate ("", Message => "O2");
      F431A00.Check (O2.R, 4, "O2.R");
      if O2.C.Info = 33 then
         Report.Failed ("Used default initialization for O2 rather than " &
                        "default expreesion");
      elsif O2.C.Info /= 80 then
         Report.Failed ("Wrong initialization value for O2");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O3 : Child2 := (R => <>, C => <>);
   begin
      F431A00.Check_Init_Count (Expected => 1, Message => "O3");
      TcTouch.Validate ("i", Message => "O3");
      F431A00.Check (O3.R, 4, "O3.R");
      if O3.C.Info /= 33 then
         Report.Failed ("Wrong initialization value for O3");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O4 : Child2 := (Root with C => <>);
   begin
      F431A00.Check_Init_Count (Expected => 1, Message => "O4");
      TcTouch.Validate ("i", Message => "O4");
      F431A00.Check (O4.R, 4, "O4.R");
      if O4.C.Info /= 33 then
         Report.Failed ("Wrong initialization value for O4");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O5 : Child3 := (R => <>, C => <>, I => <>, S => <>);
   begin
      F431A00.Check_Init_Count (Expected => 2, Message => "O5");
      TcTouch.Validate ("", Message => "O5");
      F431A00.Check (O5.R, 4, "O5.R");
      if O5.C.Info /= 80 then
         Report.Failed ("Wrong initialization value (C) for O5");
      end if;
      if O5.I /= 51 then
         Report.Failed ("Wrong initialization value (I) for O5");
      end if;
      if O5.S /= "1234567890" then
         Report.Failed ("Wrong initialization value (S) for O5");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O6 : Child3 := (Root with C => <>, I => <>, S => <>);
   begin
      F431A00.Check_Init_Count (Expected => 2, Message => "O6");
      TcTouch.Validate ("", Message => "O6");
      F431A00.Check (O6.R, 4, "O6.R");
      if O6.C.Info /= 80 then
         Report.Failed ("Wrong initialization value (C) for O6");
      end if;
      if O6.I /= 51 then
         Report.Failed ("Wrong initialization value (I) for O6");
      end if;
      if O6.S /= "1234567890" then
         Report.Failed ("Wrong initialization value (S) for O6");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O7 : Child3 := (Child1 with I => <>, S => <>);
   begin
      F431A00.Check_Init_Count (Expected => 2, Message => "O7");
      TcTouch.Validate ("", Message => "O7");
      F431A00.Check (O7.R, 4, "O7.R");
      if O7.C.Info /= 80 then
         Report.Failed ("Wrong initialization value (C) for O7");
      end if;
      if O7.I /= 51 then
         Report.Failed ("Wrong initialization value (I) for O7");
      end if;
      if O7.S /= "1234567890" then
         Report.Failed ("Wrong initialization value (S) for O7");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O8 : Child4 := (R => <>, C => <>, A => <>, B => <>);
   begin
      F431A00.Check_Init_Count (Expected => 1, Message => "O8");
      TcTouch.Validate ("iiii", Message => "O8");
      F431A00.Check (O8.R, 4, "O8.R");
      if O8.C.Info /= 80 then
         Report.Failed ("Wrong initialization value (C) for O8");
      end if;
      if O8.A /= null then
         Report.Failed ("Wrong initialization value (A) for O8");
      end if;
      if O8.B(1).Info /= 33 then
         Report.Failed ("Wrong initialization value (B) for O8");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O9 : Child4 := (Child1 with A => <>, B => <>);
   begin
      F431A00.Check_Init_Count (Expected => 1, Message => "O9");
      TcTouch.Validate ("iiii", Message => "O9");
      F431A00.Check (O9.R, 4, "O9.R");
      if O9.C.Info /= 80 then
         Report.Failed ("Wrong initialization value (C) for O9");
      end if;
      if O9.A /= null then
         Report.Failed ("Wrong initialization value (A) for O9");
      end if;
      if O9.B(1).Info /= 33 then
         Report.Failed ("Wrong initialization value (B) for O9");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O10 : Child5 := (R => <>, C => <>, A => <>, B => <>);
   begin
      F431A00.Check_Init_Count (Expected => 1, Message => "O10");
      TcTouch.Validate ("", Message => "O10");
      F431A00.Check (O10.R, 4, "O10.R");
      if O10.C.Info /= 80 then
         Report.Failed ("Wrong initialization value (C) for O10");
      end if;
      if O10.A = null then
         Report.Failed ("Wrong initialization value (A) for O10");
      end if;
      if O10.B(1) /= null then
         Report.Failed ("Wrong initialization value (B) for O10");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O11 : Child5 := (Child1 with A => <>, B => <>);
   begin
      F431A00.Check_Init_Count (Expected => 1, Message => "O11");
      TcTouch.Validate ("", Message => "O11");
      F431A00.Check (O11.R, 4, "O11.R");
      if O11.C.Info /= 80 then
         Report.Failed ("Wrong initialization value (C) for O11");
      end if;
      if O11.A = null then
         Report.Failed ("Wrong initialization value (A) for O11");
      end if;
      if O11.B(1) /= null then
         Report.Failed ("Wrong initialization value (B) for O11");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O12 : Child6 := (R => <>, C => <>, A => <>);
   begin
      F431A00.Check_Init_Count (Expected => 1, Message => "O12");
      TcTouch.Validate ("", Message => "O12");
      F431A00.Check (O12.R, 4, "O12.R");
      if O12.C.Info /= 80 then
         Report.Failed ("Wrong initialization value (C) for O12");
      end if;
      if O12.A(1).Info = 33 then
         Report.Failed ("Used default initialization rather than " &
                        "default expression for O12");
      elsif O12.A(1).Info /= 31 then
         Report.Failed ("Wrong initialization value (A) for O12");
      end if;
   end;
   F431A00.Reset_Init;
   declare
      O13 : Child6 := (Child1 with A => <>);
   begin
      F431A00.Check_Init_Count (Expected => 1, Message => "O13");
      TcTouch.Validate ("", Message => "O13");
      F431A00.Check (O13.R, 4, "O13.R");
      if O13.C.Info /= 80 then
         Report.Failed ("Wrong initialization value (C) for O13");
      end if;
      if O13.A(1).Info = 33 then
         Report.Failed ("Used default initialization rather than " &
                        "default expression for O13");
      elsif O13.A(1).Info /= 31 then
         Report.Failed ("Wrong initialization value (A) for O13");
      end if;
   end;

   Report.Result;
end C431A02;
