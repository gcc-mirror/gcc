-- C431002.A

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
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE
--     Check that a component association of others => <> in a record or
--     extension aggregate may have any number of associated components,
--     including none. (Case 2: Null records and extensions.)
--
--     Check that for each association with a <> in a record aggregate, if
--     the associated component does not have a default expression but the
--     type does have a Default_Value, the component is initialized by default
--     to the Default_Value.
--
-- TEST DESCRIPTION
--     This test checks AI05-0016-1. These cases are most likely to appear
--     during development, when it's known that a type is needed but its
--     properties are not yet known.

-- HISTORY:
--      21 NOV 2014   RLB   Created test.
--      13 MAR 2015   RLB   Eliminated overlong line.

with Report;
procedure C431002 is

   type Color_Type is (Unknown, Red, Orange, Yellow, Green, Blue,
      Indigo, Violet) with Default_Value => Unknown;

   type Default_to_Zero_Type is range -10000 .. 10000
      with Default_Value => 0;

   type Half_Type is digits 4
      with Default_Value => 0.5;

   type Null_Rec is null record;

   type Root is tagged record
      Color : Color_Type;
      Halves: Half_Type;
   end record;

   type Null_Ext is new Root with null record;

   type Val_Ext is new Root with record
      Zero  : Default_to_Zero_Type;
   end record;

   procedure Check_1 (Obj : Root'Class; Color : Color_Type;
                      Halves : Half_Type;
                      Test_Case : Character) is
   begin
      if Obj.Color /= Color then
         Report.Failed ("Color incorrect (" & Test_Case & ")");
      end if;
      if Obj.Halves /= Halves then
         Report.Failed ("Halves incorrect (" & Test_Case & ")");
      end if;
   end Check_1;

   procedure Check_2 (Obj : Val_Ext'Class; Color : Color_Type;
                      Halves : Half_Type; Int : Default_to_Zero_Type;
                      Test_Case : Character) is
   begin
      if Obj.Color /= Color then
         Report.Failed ("Color incorrect (" & Test_Case & ")");
      end if;
      if Obj.Halves /= Halves then
         Report.Failed ("Halves incorrect (" & Test_Case & ")");
      end if;
      if Obj.Zero /= Int then
         Report.Failed ("Int incorrect (" & Test_Case & ")");
      end if;
   end Check_2;

begin

   Report.Test ("C431002",
                "Check that a component association of others => <> in a " &
                "record or extension aggregate may have any number of "    &
                "associated components, including none. (Case 2: Null "    &
                "records and extensions)");

   -- Check a null record:
   declare
      O1 : Null_Rec := (others => <>);
   begin
      if O1 /= Null_Rec'(null record) then
         Report.Failed ("Not null record (A)");
      end if;
   end;

   -- Check tagged records (record aggregates):
   Check_1 (Root'(others => <>),
            Color => Unknown, Halves => 0.5,
            Test_Case => 'B');

   Check_1 (Null_Ext'(Color => Red, others => <>),
            Color => Red, Halves => 0.5,
            Test_Case => 'C');

   Check_1 (Null_Ext'(Color => Red, Halves => 2.5, others => <>),
            Color => Red, Halves => 2.5,
            Test_Case => 'D');

   Check_2 (Val_Ext'(Color => Yellow, Halves => 2.5, others => <>),
            Color => Yellow, Halves => 2.5, Int => 0,
            Test_Case => 'E');

   Check_2 (Val_Ext'(Color => Blue, Halves => 2.5, Zero => 2, others => <>),
            Color => Blue, Halves => 2.5, Int => 2,
            Test_Case => 'F');

   Check_2 (Val_Ext'(Halves => 2.5, Zero => 2, others => <>),
            Color => Unknown, Halves => 2.5, Int => 2,
            Test_Case => 'G');

   Check_2 (Val_Ext'(Color => Blue, Halves => <>, Zero => 2),
            Color => Blue, Halves => 0.5, Int => 2,
            Test_Case => 'H');

   -- Check tagged records (extension aggregates):
   declare
      O2 : Root := (Color => Orange, Halves => 4.25, others => <>);
   begin
      Check_1 (O2,
               Color => Orange, Halves => 4.25,
               Test_Case => 'J');

      Check_1 (Null_Ext'(Root with others => <>),
               Color => Unknown, Halves => 0.5,
               Test_Case => 'K');

      Check_1 (Null_Ext'(O2 with others => <>),
               Color => Orange, Halves => 4.25,
               Test_Case => 'L');

      Check_2 (Val_Ext'(Root with others => <>),
               Color => Unknown, Halves => 0.5, Int => 0,
               Test_Case => 'M');

      Check_2 (Val_Ext'(O2 with others => <>),
               Color => Orange, Halves => 4.25, Int => 0,
               Test_Case => 'N');

      Check_2 (Val_Ext'(Root with Zero => 6, others => <>),
               Color => Unknown, Halves => 0.5, Int => 6,
               Test_Case => 'P');

      Check_2 (Val_Ext'(O2 with Zero => <>, others => <>),
               Color => Orange, Halves => 4.25, Int => 0,
               Test_Case => 'Q');

   end;

   Report.Result;

end C431002;
