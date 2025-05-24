-- C431A01.A
--
--                             Grant of Unlimited Rights
--
--     AdaCore holds unlimited rights in the software and documentation
--     contained herein. Unlimited rights are the same as those granted
--     by the U.S. Government for older parts of the Ada Conformity
--     Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--     By making this public release, AdaCore intends to confer upon all
--     recipients unlimited rights equal to those held by the Ada Conformity
--     Assessment Authority. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever,
--     and to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--     TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--     DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--     DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
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
--     This test is based on one submitted by AdaCore; AdaCore retains
--     the copyright on the test.
--
--*
--  OBJECTIVE:
--      Check that record aggregates and extension aggregates can
--      have a limited type.
--
--      Check that a component association of others => <> in a record
--      aggregate or extension aggregate may have any number of associated
--      components, including none.
--
--      Check that a component association in a record aggregate or
--      extension aggregate with a <> may have two or more associated
--      components of different types.
--
--  TEST DESCRIPTION:
--      This test declares objects of a record type provided by the
--      foundation with three components (one protected object, one task,
--      one limited record).
--      The foundation provides the task and protected types with support to
--      set and get one value, and thus check that these components have been
--      properly initialized.
--      We check that the task and protected components work properly, and
--      that the integer components are initialized with the proper number of
--      calls.
--
-- CHANGE HISTORY:
--      2 Feb 2004 JM  Initial Version.
--     25 Sep 2007 RLB Enhanced test to use foundation; test additional
--                     objectives; and made test self-checking.
--
--!
with Report;
with F431A00; use F431A00;
procedure C431A01 is

begin
   Report.Test ("C431A01", "In record and array aggregates: check that a " &
                           "component association with a <> may represent " &
                           "two or more components of different types; check " &
                           "that others => <> may represent any number of " &
                           "components, including none; and check that the " &
                           "aggregate can have a limited type");

   F431A00.Reset_Init;
   declare
      O1 : F431A00.My_Rec := (Info => <>, P => <>, T => <>);
      O2 : F431A00.My_Rec := (others => <>);
      O3 : F431A00.My_Rec := (Info | P | T => <>);
      O4 : F431A00.My_Rec := (Info => (80, others => <>),
                              P => <>, others => <>);
      O5 : F431A00.My_Rec := (Info => (80, 'D', others => <>),
                              P => <>, T => <>, others => <>);
   begin
      Check (O1, 4, "O1");
      Check (O2, 4, "O2");
      Check (O3, 4, "O3");
      Check (O4,80, "O4");
      Check (O5,80, "O5");
      F431A00.Check_Init_Count (Expected => 3, Message => "Subtest 1");
   end;

   F431A00.Reset_Init;
   declare
      type Nested_Rec is record
         C1 : F431A00.My_Rec := (Info => <>, P => <>, T => <>);
         C2 : Integer := F431A00.Init_Func(66);
      end record;
      O11 : Nested_Rec;
      O12 : Nested_Rec := (C1 | C2 => <>);
      O13 : Nested_Rec := (others => <>);
      O14 : Nested_Rec := (C1 => (Info => (80, others => <>),
                           T | P => <>), C2 => 10);
      O15 : Nested_Rec := (C1 => (Info => (80, C => 'D', others => <>),
                           T | P => <>, others => <>), others => <>);
   begin
      Check (O11.C1, 4, "O11");
      Check (O12.C1, 4, "O12");
      Check (O13.C1, 4, "O13");
      Check (O14.C1,80, "O14");
      Check (O15.C1,80, "O15");
      F431A00.Check_Init_Count (Expected => 7, Message => "Subtest 2");
   end;

   F431A00.Reset_Init;
   declare
      type Root is tagged limited record
         C10 : F431A00.My_Rec := (others => <>);
      end record;
      type T20 is new Root with record
         A20 : Acc_NLRec;
         C20 : Character := 'A';
      end record;
      type T30 is new T20 with record
         I30 : Integer := F431A00.Init_Func (51);
      end record;

      procedure Check (Item : in out T20'Class; Value : in Integer;
                    Message : in String) is
      begin
          Check (Item.C10, Value, Message);
          if Item.A20 /= null then
              Report.Failed (Message & " A20 component not null");
          end if;
          if Item.C20 /= 'A' then
              Report.Failed (Message & " C20 component not 'A'");
          end if;
      end Check;

      O20 : T20 := (Root with A20 => <>, C20 => <>);
      O21 : T20 := (Root'(C10 => (others => <>)) with A20 | C20 => <>);
      O22 : T20 := (Root'(C10 => (others => <>)) with others => <>);

      O23 : T30 := (T20 with I30 => <>);
      O24 : T30 := (Root with A20 | C20 | I30 => <>);
      O25 : T30 := (Root with others => <>);
      O26 : T30 := (T20 with others => <>);
      O27 : T30 := (T20 with I30 =>  31, others => <>);
   begin
      Check (O20, 4, "O20");
      Check (O21, 4, "O21");
      Check (O22, 4, "O21");
      Check (O23, 4, "O23");
      Check (O24, 4, "O24");
      Check (O25, 4, "O25");
      Check (O26, 4, "O26");
      Check (O27, 4, "O27");
      F431A00.Check_Init_Count (Expected => 12, Message => "Subtest 3");
   end;

   Report.Result;
end C431A01;
