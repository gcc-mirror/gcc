-- C433A04.A
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
--     component is initialized by default. (Non-limited cases.)
--
--  TEST DESCRIPTION:
--     This test tries various aggregates containing <> for a number
--     of interesting types. The test was inspired by tests submitted by
--     AdaCore, but was written from scratch to test many more kinds of
--     types.
--
-- CHANGE HISTORY:
--     20 Sep 2007 RLB Created test.
--
--!
with Report;
with TcTouch;
with F433A00;
with Ada.Finalization;
procedure C433A04 is
   Default : constant F433A00.Ctrl_Rec :=
      (Ada.Finalization.Controlled with Info => 33);
   type TA is array (1 .. 3) of F433A00.Ctrl_Rec;
   type TA2 is array (1 .. 2, 1 .. 4) of F433A00.Ctrl_Rec;
   use type F433A00.Acc_NLRec;
   type TB is array (1 .. 3) of F433A00.Acc_NLRec;
   type TB2 is array (1 .. 2, 1 .. 4) of F433A00.Acc_NLRec;
   type TBS is array (1 .. 2) of TB;
   type TC is array (1 .. 2) of F433A00.Arr_NLRec;
   type TD is array (1 .. 5) of Character;

begin
   Report.Test ("C433A04", "Check that for each association with a <> in an " &
                           "array aggregate, the component is initialized by " &
                           "default. (Non-limited cases.)");

   TcTouch.Flush;
   declare
      O1 : TA := (others => <>);
      Result : TA := (others => Default);
   begin
      TcTouch.Validate_One_Of ("iii", "iiiiii", Message => "O1: Others => <>");
         -- First if build-in-place, second if not.
      if O1 /= Result then
         Report.Failed ("Wrong value: O1");
      end if;
   end;

   declare
      O2 : TA := TA'(1 => Default, 2 | 3 => <>);
      Result : TA := (others => Default);
   begin
      TcTouch.Validate_One_Of ("ii", "iiiii", Message => "O2: 2 | 3 => <>");
         -- First if build-in-place, second if not.
      if O2 /= Result then
         Report.Failed ("Wrong value: O2");
      end if;
   end;

   declare
      O3 : TA := (1 .. 2 => <>, 3 => Default);
      Result : TA := (others => Default);
   begin
      TcTouch.Validate_One_Of ("ii", "iiiii", Message => "O3: 1 .. 2 => <>");
         -- First if build-in-place, second if not.
      if O3 /= Result then
         Report.Failed ("Wrong value: O3");
      end if;
   end;

   declare
      O4 : TA := TA'(2 => <>, 1 | 3 => Default);
      Result : TA := (others => Default);
   begin
      TcTouch.Validate_One_Of ("i", "iiii", Message => "O4: 2 => <>");
         -- First if build-in-place, second if not.
      if O4 /= Result then
         Report.Failed ("Wrong value: O4");
      end if;
   end;

   declare
      O5 : TA2 := (others => (others => <>));
   begin
      TcTouch.Validate_One_Of ("iiiiiiii", "iiiiiiiiiiiiiiii", Message => "O5");
         -- First if build-in-place, second if not.
      if O5 /= (TA2'Range(1) => (TA2'Range(2) => Default)) then
         Report.Failed ("Wrong value: O5");
      end if;
   end;

   declare
      O6 : TA2 := (1 => (1 .. 4 => <>), 2 => (others => Default));
   begin
      TcTouch.Validate_One_Of ("iiii", "iiiiiiiiiiii", Message => "O6");
         -- First if build-in-place, second if not.
      if O6 /= (TA2'Range(1) => (TA2'Range(2) => Default)) then
         Report.Failed ("Wrong value: O6");
      end if;
   end;

   declare
      O7 : TA2 := TA2'(1 .. 2 => (1 | 3 => <>, 2 | 4 => Default));
   begin
      TcTouch.Validate_One_Of ("iiii", "iiiiiiiiiiii", Message => "O7");
         -- First if build-in-place, second if not.
      if O7 /= (TA2'Range(1) => (TA2'Range(2) => Default)) then
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
      O12 := TB'(1 => new F433A00.Ctrl_Rec, 2 | 3 => <>);
      O13 := (1 .. 2 => <>, 3 => new F433A00.Ctrl_Rec);
      O14 := TB'(2 => <>, 1 | 3 => new F433A00.Ctrl_Rec);
      if O11 /= TB'(others => null) then
         Report.Failed ("Wrong value: O11");
      end if;
      if O12(2) /= null or O12(3) /= null then
         Report.Failed ("Wrong value: O12");
      end if;
      if O13(1) /= null or O13(2) /= null then
         Report.Failed ("Wrong value: O13");
      end if;
      if O14(2) /= null then
         Report.Failed ("Wrong value: O14");
      end if;
      O15:= (others => (others => <>));
      O16:= TB2'(1 => (1 .. 4 => <>), 2 => (others => null));
      O17:= (1 .. 2 => (1 | 3 => <>, 2 | 4 => null));
      if O15 /= (TB2'Range(1) => (TB2'Range(2) => null)) then
         Report.Failed ("Wrong value: O15");
      end if;
      if O16 /= (TB2'Range(1) => (TB2'Range(2) => null)) then
         Report.Failed ("Wrong value: O16");
      end if;
      if O17 /= (TB2'Range(1) => (TB2'Range(2) => null)) then
         Report.Failed ("Wrong value: O17");
      end if;
      O18:= (others => <>);
      O19:= TBS'(1 => <>, 2 => (others => null));
      if O18 /= (TBS'Range => (TB'Range => null)) then
         Report.Failed ("Wrong value: O18");
      end if;
      if O19 /= (TBS'Range => (TB'Range => null)) then
         Report.Failed ("Wrong value: O19");
      end if;
   end;

   TcTouch.Flush;
   declare
      O21 : TC := (others => <>);
      Result : TC := (others => (others => Default));
   begin
      TcTouch.Validate_One_Of ("iiiiiiii", "iiiiiiiiiiiiiiii", Message => "O21");
         -- First if build-in-place, second if not.
      if O21 /= Result then
         Report.Failed ("Wrong value: O21");
      end if;
   end;

   declare
      O22 : TC := TC'(1 => (others => Default), 2 => <>);
      Result : TC := (others => (others => Default));
   begin
      TcTouch.Validate_One_Of ("iiii", "iiiiiiiiiiii", Message => "O22");
         -- First if build-in-place, second if not.
      if O22 /= Result then
         Report.Failed ("Wrong value: O22");
      end if;
   end;

   declare
      O23 : TC := (1 .. 2 => <>);
      Result : TC := (others => (others => Default));
   begin
      TcTouch.Validate_One_Of ("iiiiiiii", "iiiiiiiiiiiiiiii", Message => "O23");
         -- First if build-in-place, second if not.
      if O23 /= Result then
         Report.Failed ("Wrong value: O23");
      end if;
   end;

   declare
      O31 : TD;
      O32 : TD;
      O33 : TD;
      O34 : TD;
      O35 : TD;
   begin
      O31 := (others => <>);
      O32 := TD'(1 => 'A', 2 .. 5 => <>);
      O33 := (1 | 2 | 5 => <>, 3 | 4 => 'C');
      O34 := TD'(1 => <>, 2 .. 5 => 'D');
      O35 := (1 .. 5 => <>);
      -- Note: <> cases are uninitialize, so we can only
      -- check the explicitly initialized components.
      -- O31: nothing to check.
      if O32(1) /= 'A' then
         Report.Failed ("Wrong value: O32");
      end if;
      if O33(3) /= 'C' or O33(4) /= 'C' then
         Report.Failed ("Wrong value: O33");
      end if;
      if O34(2..5) /= "DDDD" then
         Report.Failed ("Wrong value: O34");
      end if;
      -- O35: nothing to check.
   end;

   Report.Result;
end C433A04;
