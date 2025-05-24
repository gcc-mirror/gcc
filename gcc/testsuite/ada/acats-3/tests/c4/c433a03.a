-- C433A03.A
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
--     Check that for a <> in an array aggregate with multiple associated
--     components, each associated component is default initialized
--     individually.
--
--  TEST DESCRIPTION:
--     This test tries various cases where <> represents multiple components,
--     and uses the foundation to ensure that it is evaluated the correct
--     number of times. The test cases are based on the old ACATS tests
--     C43207D, C43208A, C43208B, and C43210A.
--     This behavior can be important if the initialization provides
--     different values each time it is evaluated (imagine a random number
--     generator). We don't test this specific case because the fact
--     that order of evaluation is not defined by Ada means that many
--     different results would be correct.
--
-- CHANGE HISTORY:
--     20 Sep 2007 RLB Created test from similar Ada 83 test cases.
--
--!
with Report;
with F433A00;
procedure C433A03 is

   type Our_Rec is record
      A : Integer := F433A00.Init_Func;
      C : Character := 'Z';
   end record;

   Org : constant Our_Rec := (others => <>);

begin
   Report.Test ("C433A03", "Check that for a <> in an array aggregate with " &
                           "multiple associated components, each associated " &
                           "component is default initialized individually");

   declare

      type T1 is array (1 .. 10) of Our_Rec;
      type T2 is array (1 .. 8, 1 .. 2) of Our_Rec;
      type T3 is array (1 .. 2, 1 .. 8) of Our_Rec;
      type T4 is array (1 .. 8, 1 .. 8) of Our_Rec;

      A1 : T1;
      A2 : T2;
      A3 : T3;
      A4 : T4;

   begin

Case_A : begin
         F433A00.Reset_Init;
         A1 := T1'(4 .. 5 => <>, 6 .. 8 => <>, others => Org);
         F433A00.Check_Init_Count (Expected => 5, Message => "Subtest A");
      end Case_A;

Case_B :  begin
         F433A00.Reset_Init;
         A1 := T1'(1 | 4 .. 6 | 3 | 2 => <>, others => Org);
         F433A00.Check_Init_Count (Expected => 6, Message => "Subtest B");
      end Case_B;

Case_C : begin
         F433A00.Reset_Init;
         A1 := T1'(1 | 3 | 5 | 7 .. 9 => Org, others => <>);
         F433A00.Check_Init_Count (Expected => 4, Message => "Subtest C");
      end Case_C;

Case_D : begin
         F433A00.Reset_Init;
         A2 := T2'(4 .. 6 | 8 | 2 .. 3 => (1 .. 2 => <>),
                   others => (1 .. 2 => Org));
         F433A00.Check_Init_Count (Expected => 12, Message => "Subtest D");
      end Case_D;

Case_E : begin
         F433A00.Reset_Init;
         A3 := T3'(1 .. 2 => (2 | 4 | 6 .. 8 => <>, others => Org));
         F433A00.Check_Init_Count (Expected => 10, Message => "Subtest E");
      end Case_E;

Case_F : begin
         F433A00.Reset_Init;
         A4 := T4'(7 .. 8 | 3 .. 5 =>
                     (1 | 2 | 4 | 6 .. 8 => <>, others => Org),
                   others => (others => Org));
         F433A00.Check_Init_Count (Expected => 30, Message => "Subtest F");
      end Case_F;

Case_G : begin
         F433A00.Reset_Init;
         A4 := T4'(5 .. 8 | 3 | 1 => (7 | 1 .. 5 | 8 => Org,
                                      others => <>),
                   others => (others => <>));
         F433A00.Check_Init_Count (Expected => 22, Message => "Subtest G");
      end Case_G;

   end;

   declare
      type T1 is array (Integer range <>) of Our_Rec;
      type T2 is array (Integer range <>, Integer range <>) of Our_Rec;
   begin

Case_H1 : declare
         H1 : T2(8 .. 4, 5 .. 1);
      begin
         F433A00.Reset_Init;
         H1 := (8 .. 4 => (5 .. 1 => <>));
         F433A00.Check_Init_Count (Expected => 0, Message => "Subtest H1");
      exception
         when others =>
            Report.Failed ("Case H1 : Exception raised");
      end Case_H1;

Case_H2 : declare
         H2 : T2(8 .. 4, 5 .. 1);
      begin
         F433A00.Reset_Init;
         H2 := (Report.Ident_Int(8) .. Report.Ident_Int(4) =>
                  (Report.Ident_Int(5) .. Report.Ident_Int(1) => <>));
         F433A00.Check_Init_Count (Expected => 0, Message => "Subtest H2");
      exception
         when others =>
            Report.Failed ("Case H2 : Exception raised");
      end Case_H2;

Case_H3 : declare
         H3 : T2(3 .. 5, 1 .. 2);
      begin
         F433A00.Reset_Init;
         H3 := (3 .. 5 => (1 .. 2 => <>));
         F433A00.Check_Init_Count (Expected => 6, Message => "Subtest H3");
      exception
         when others =>
            Report.Failed ("Case H3 : Exception raised");
      end Case_H3;

Case_H4 : declare
         H4 : T2(1 .. 2, 5 .. 7);
      begin
         F433A00.Reset_Init;
         H4 := (Report.Ident_Int(1) .. Report.Ident_Int(2) =>
                  (Report.Ident_Int(5) .. Report.Ident_Int(7) => <>));
         F433A00.Check_Init_Count (Expected => 6, Message => "Subtest H4");
      exception
         when others =>
            Report.Failed ("Case H4 : Exception raised");
      end Case_H4;

Case_J1 : declare
         J1 : array (4 .. 2) of T1(1 .. 2);
      begin
         F433A00.Reset_Init;
         J1 := (4 .. 2 =>
                (F433A00.Init_Func .. F433A00.Init_Func => <>));
         F433A00.Check_Init_Count (Expected => 0, Message => "Subtest J1");
      exception
         when others =>
            Report.Failed ("Case J1 : Exception raised");
      end Case_J1;

Case_J2 : declare
         J2 : array (4 .. 2) of T1(1 .. 2);
      begin
         F433A00.Reset_Init;
         J2 := (Report.Ident_Int(4) .. Report.Ident_Int(2) =>
                (F433A00.Init_Func .. F433A00.Init_Func => <>));
         F433A00.Check_Init_Count (Expected => 0, Message => "Subtest J2");
      exception
         when others =>
            Report.Failed ("Case J2 : Exception raised");
      end Case_J2;

Case_K1 : declare
         K1 : array (2 .. 3) of T1(1 .. 2);
      begin
         F433A00.Reset_Init;
         K1 := (2 .. 3 =>
                  (Report.Ident_Int(1) .. Report.Ident_Int(2) => <>));
         F433A00.Check_Init_Count (Expected => 4, Message => "Subtest K1");
      exception
         when others =>
            Report.Failed ("Case K1 : Exception raised");
      end Case_K1;

Case_K2 : declare
         K2 : array (2 .. 3) of T1(9 .. 10);
      begin
         F433A00.Reset_Init;
         K2 := (Report.Ident_Int(2) .. Report.Ident_Int(3) =>
                  (Report.Ident_Int(9) .. Report.Ident_Int(10) => <>));
         F433A00.Check_Init_Count (Expected => 4, Message => "Subtest K2");
      exception
         when others =>
            Report.Failed ("Case K2 : Exception raised");
      end Case_K2;

Case_K3 : declare
         K3 : array (2 .. 3) of T1(2 .. 1);
      begin
         F433A00.Reset_Init;
         K3 := (2 .. 3 =>
                  (Report.Ident_Int(2) .. Report.Ident_Int(1) => <>));
         F433A00.Check_Init_Count (Expected => 0, Message => "Subtest K3");
      exception
         when others =>
            Report.Failed ("Case K3 : Exception raised");
      end Case_K3;

Case_K4 : declare
         K4 : array (2 .. 3) of T1(2 .. 1);
      begin
         F433A00.Reset_Init;
         K4 := (Report.Ident_Int(2) .. Report.Ident_Int(3) =>
                  (Report.Ident_Int(2) .. Report.Ident_Int(1) => <>));
         F433A00.Check_Init_Count (Expected => 0, Message => "Subtest K4");
      exception
         when others =>
            Report.Failed ("Case K4 : Exception raised");
      end Case_K4;

Case_L1 : declare
         L1 : array (4 .. 3, 3 .. 4) of T2(2 .. 3, 1 .. 2);
      begin
         F433A00.Reset_Init;
         L1 := (4 .. 3 => (3 .. 4 =>
             (F433A00.Init_Func .. F433A00.Init_Func =>
                (F433A00.Init_Func .. F433A00.Init_Func => <>))));
         F433A00.Check_Init_Count (Expected => 0, Message => "Subtest L1");
      exception
         when others =>
            Report.Failed ("Case L1 : Exception raised");
      end Case_L1;

Case_L2 : declare
         L2 : array (3 .. 4, 4 .. 3) of T2(2 .. 3, 1 .. 2);
      begin
         F433A00.Reset_Init;
         L2 := (Report.Ident_Int(3) .. Report.Ident_Int(4) =>
             (Report.Ident_Int(4) .. Report.Ident_Int(3) =>
                (F433A00.Init_Func .. F433A00.Init_Func =>
                   (F433A00.Init_Func .. F433A00.Init_Func => <>))));
         F433A00.Check_Init_Count (Expected => 0, Message => "Subtest L2");
     exception
        when others =>
           Report.Failed ("Case L2 : Exception raised");
     end Case_L2;

 Case_M1 : declare
         M1 : array (2 .. 3, 1 .. 2) of T2(1 .. 2, 9 .. 10);
      begin
         F433A00.Reset_Init;
         M1 := (2 .. 3 => (1 .. 2 =>
             (Report.Ident_Int(1) .. Report.Ident_Int(2) =>
                (Report.Ident_Int(9) .. Report.Ident_Int(10) => <>))));
         F433A00.Check_Init_Count (Expected => 16, Message => "Subtest M1");
      exception
         when others =>
            Report.Failed ("Case M1 : Exception raised");
      end Case_M1;

Case_M2 : declare
         M2 : array (2 .. 3, 1 .. 2) of T2(1 .. 2, 9 .. 10);
      begin
         F433A00.Reset_Init;
         M2 := (Report.Ident_Int(2) .. Report.Ident_Int(3) =>
                  (Report.Ident_Int(1) .. Report.Ident_Int(2) =>
                     (Report.Ident_Int(1) .. Report.Ident_Int(2) =>
                        (Report.Ident_Int(9) .. Report.Ident_Int(10) => <>))));
         F433A00.Check_Init_Count (Expected => 16, Message => "Subtest M2");
      exception
         when others =>
            Report.Failed ("Case M2 : Exception raised");
      end Case_M2;

Case_M3 : declare
         M3 : array (2 .. 3, 1 .. 2) of T2(1 .. 2, 2 .. 1);
      begin
         F433A00.Reset_Init;
         M3 := (2 .. 3 => (1 .. 2 =>
                 (Report.Ident_Int(1) .. Report.Ident_Int(2) =>
                    (Report.Ident_Int(2) .. Report.Ident_Int(1) => <>))));
         F433A00.Check_Init_Count (Expected => 0, Message => "Subtest M3");
      exception
         when others =>
            Report.Failed ("Case M3 : Exception raised");
      end Case_M3;

Case_M4 : declare
         M4 : array (2 .. 3, 1 .. 2) of T2(2 .. 1, 1 .. 2);
      begin
         F433A00.Reset_Init;
         M4 := (Report.Ident_Int(2) .. Report.Ident_Int(3) =>
                  (Report.Ident_Int(1) .. Report.Ident_Int(2) =>
                     (Report.Ident_Int(2) .. Report.Ident_Int(1) =>
                        (Report.Ident_Int(1) .. Report.Ident_Int(2) => <>))));
         F433A00.Check_Init_Count (Expected => 0, Message => "Subtest M4");
      exception
         when others =>
            Report.Failed ("Case M4 : Exception raised");
      end Case_M4;

   end;

   Report.Result;
end C433A03;
