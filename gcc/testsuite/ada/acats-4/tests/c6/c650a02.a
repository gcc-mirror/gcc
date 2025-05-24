-- C650A02.A
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
--
--*
-- OBJECTIVE:
--     Check that the tag of the result of a function that returns a
--     class-wide tagged type with a simple return statement is that of
--     the return expression. Case A: limited types.
--
--     Check that the tag of the result of a function that returns a
--     class-wide tagged type with an extended return statement whose subtype
--     indication has a class-wide type is the tag of the initializing
--     expression. Case A: limited types.
--
-- TEST DESCRIPTION:
--     We try a function that returns objects of various types using
--     different methods. Because of the limited type rules, we can only
--     use function calls or extension aggregates here.
--
--     As a side-effect of this test, we test that a class-wide function
--     can return build-in-place objects. (We don't have a place for that
--     as a specific objective.)
--
--     While the form of the test function isn't very realistic, each
--     of the return expressions is quite plausible, constructing an alert
--     in one of the allowed ways. A class-wide function of a limited type
--     would have to use one of these forms to return a value.
--
--     This is an Ada 2012 test as we use the extension to use a specific
--     type in the extended return for a function returning a class-wide type.
--
-- CHANGE HISTORY:
--     20 Aug 2015  RLB  Created test.

with F650A00.P, F650A00.S;
with Report, Ada.Tags;
procedure C650A02 is

   use type Ada.Tags.Tag;

   function Get_Alert (Subtest : in Natural) return F650A00.Alert'Class is
   begin
      case Subtest is
         when 1 =>
             return F650A00.P.Practice_Alert'(F650A00.Alert with
                Status => F650A00.P.Practice, Urgency => F650A00.P.Medium);
         when 2 =>
             return Obj : F650A00.P.Practice_Alert;
         when 3 =>
             return F650A00.S.Special_Alert'(F650A00.P.Practice_Alert with
                Age => 57, Display => F650A00.S.Big_Screen);
         when 4 =>
             return Result : F650A00.S.Special_Alert(Age => 78) do
                Result.Display := F650A00.S.Console;
             end return;
         when others => raise Program_Error;
      end case;
   end Get_Alert;

   function Test_Function (Subtest : in Natural) return F650A00.Alert'Class is
   begin
      case Subtest is
         when 1 =>
             return F650A00.P.Practice_Alert'(F650A00.Alert with
                Status => F650A00.P.Real, Urgency => F650A00.P.High);
         when 2 =>
             return F650A00.S.Special_Alert'(F650A00.P.Practice_Alert with
                Age => 19, Display => F650A00.S.Big_Screen);
         when 3 =>
             return Obj : F650A00.Alert'Class :=
                F650A00.P.Practice_Alert'(F650A00.Alert with
                    Status => F650A00.P.Dont_Care, Urgency => F650A00.P.Low);
         when 4 =>
             return Obj : F650A00.Alert'Class :=
                F650A00.S.Special_Alert'(F650A00.P.Practice_Alert with
                Age => 97, Display => F650A00.S.Teletype) do
                Obj.Set_Alert_Time (10.0);
             end return;
         when 5 =>
             return Get_Alert (1);
         when 6 =>
             return Get_Alert (2);
         when 7 =>
             return Result : F650A00.Alert'Class := Get_Alert (3);
         when 8 =>
             return Result : F650A00.Alert'Class := Get_Alert (4);
         when others => raise Program_Error;
      end case;
   end Test_Function;

   procedure Check_Result (P : in F650A00.Alert'Class;
                           Practice_Expected : in Boolean;
                           Urgency : in F650A00.P.Urgency_Kind;
                           Subtest : in Natural) is
      use type F650A00.P.Urgency_Kind;
   begin
      if Practice_Expected then
         if P'Tag /= F650A00.P.Practice_Alert'Tag then
             Report.Failed ("Wrong tag - expected practice alert (" &
                                               Natural'Image(Subtest) & ')');
         end if;
      else
         if P'Tag /= F650A00.S.Special_Alert'Tag then
             Report.Failed ("Wrong tag - expected special alert - (" &
                                               Natural'Image(Subtest) & ')');
         end if;
      end if;
      if F650A00.P.Practice_Alert'Class(P).Urgency /= Urgency then
         Report.Failed ("Wrong urgency - expected " &
            F650A00.P.Urgency_Kind'Image(Urgency) &
            " (" & Natural'Image(Subtest) & ')');
      end if;
   end Check_Result;

begin
   Report.Test ("C650A02", "Check that the tag of the result of a function " &
                           "that returns a class-wide tagged type is that " &
                           "of the return expression. Case A: limited types");

   declare
      Res_1 : F650A00.Alert'Class := Test_Function (1);
   begin
      if Res_1'Tag /= F650A00.P.Practice_Alert'Tag then
         Report.Failed ("Wrong tag (A)");
      end if;
      Check_Result (Res_1, Practice_Expected => True,
                    Urgency => F650A00.P.High, Subtest => 1);
   end;

   Check_Result (Test_Function (2), Practice_Expected => False,
                 Urgency => F650A00.P.Low, Subtest => 2);

   declare
      Res_3 : F650A00.Alert'Class := Test_Function (3);
   begin
      Check_Result (Res_3, Practice_Expected => True,
                    Urgency => F650A00.P.Low, Subtest => 3);
   end;

   declare
      Res_4 : F650A00.Alert'Class := Test_Function (4);
   begin
      Check_Result (Res_4, Practice_Expected => False,
                    Urgency => F650A00.P.Low, Subtest => 4);
   end;

   Check_Result (Test_Function (5), Practice_Expected => True,
                 Urgency => F650A00.P.Medium, Subtest => 5);

   declare
      Res_6 : F650A00.Alert'Class := Test_Function (6);
   begin
      Check_Result (Res_6, Practice_Expected => True,
                    Urgency => F650A00.P.Low, Subtest => 6);
   end;

   declare
      Res_7 : F650A00.Alert'Class := Test_Function (7);
   begin
      Check_Result (Res_7, Practice_Expected => False,
                    Urgency => F650A00.P.Low, Subtest => 7);
   end;

   Check_Result (Test_Function (8), Practice_Expected => False,
                 Urgency => F650A00.P.Low, Subtest => 8);

   Report.Result;
end C650A02;

