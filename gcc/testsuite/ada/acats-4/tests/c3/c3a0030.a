-- C3A0030.A
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
--
-- OBJECTIVE:
--      Check that an access discriminant only is null excluding when a
--      null exclusion is given.
--
-- TEST DESCRIPTION:
--      This is inconsistent with Ada 95 (all anonymous access types were
--      null excluding in Ada 95).
--
--      As this test is about ensuring that checks are made (or not made),
--      we do not try to describe a usage scenario. This test is about
--      detecting bugs, and no one (other than ACATS test writers)
--      intentionally depend on the behavior of bugs.
--
-- CHANGE HISTORY:
--      26 Jan 16   RLB     Created test.
--
--!

with Report;
with Ada.Exceptions;
procedure C3A0030 is

   type Bar is record
      A : Character;
   end record;

   type Acc_Bar is access all Bar;

   Foo : aliased Bar := (A => 'A');

   type Exclude_Record (Ptr : not null access Bar) is record
      C : Character;
   end record;

   type Include_Record (Ptr : access Bar) is record
      C : Character;
   end record;

   procedure Check_Bar (Foo : access Bar; Expected : in Character;
                        Subtest : in Natural) is
   begin
      if Expected = ' ' then
         if Foo /= null then
            Report.Failed ("Non-null Bar value when null expected, case"
               & Natural'Image(Subtest));
         end if;
      else
         if Foo = null then
            Report.Failed ("Null Bar value when non-null expected, case"
               & Natural'Image(Subtest));
         elsif Foo.A /= Expected then
            Report.Failed ("Unexpected Bar value, expected=" & Expected &
              "; saw=" & Foo.A & "; case" & Natural'Image(Subtest));
         end if;
      end if;
   end Check_Bar;

   function Make_Exclude (Foo : access Bar) return Exclude_Record is
   begin
      return (Ptr => Foo, C => 'Q');
   end Make_Exclude;

   function Make_Include (Foo : access Bar) return Include_Record is
   begin
      return (Ptr => Foo, C => 'Q');
   end Make_Include;


begin

   Report.Test ("C3A0030", "Check that an access discriminant only is null " &
                           "excluding when a null exclusion is given");

   Test_Block:
   declare
      Foo_Ptr1 : Acc_Bar := Foo'Access;
      Foo_Ptr2 : Acc_Bar := null;
   begin

      -- Explicit constraint:
      begin
         declare
            Rec : Exclude_Record (Foo'Access);
         begin
            Check_Bar (Rec.Ptr, 'A', Subtest => 1);
         end;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 1: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      begin
         declare
            Rec : Exclude_Record (null);
         begin
            Check_Bar (Rec.Ptr, ' ', Subtest => 2);
            Report.Failed
               ("Null allowed for null-excluding discriminant, case 2");
         end;
      exception
         when Constraint_Error =>
            null; -- Expected exception.
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 2: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      begin
         declare
            Rec : Include_Record (Foo'Access);
         begin
            Foo.A := 'B';
            Check_Bar (Rec.Ptr, 'B', Subtest => 3);
         end;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 3: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;
      begin
         declare
            Rec : Include_Record (null);
         begin
            Check_Bar (Rec.Ptr, ' ', Subtest => 4);
         end;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 4: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      -- Discriminant initialized by an aggregate:
      begin
         declare
            Rec : Exclude_Record :=
               (Ptr => Foo_Ptr1, C => 'D');
         begin
            Foo.A := 'C';
            Check_Bar (Rec.Ptr, 'C', Subtest => 5);
         end;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 5: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      begin
         declare
            Rec : Exclude_Record :=
               (Ptr => Foo_Ptr2, C => 'G');
         begin
            Check_Bar (Rec.Ptr, ' ', Subtest => 6);
            Report.Failed
               ("Null allowed for null-excluding discriminant, case 6");
         end;
      exception
         when Constraint_Error =>
            null; -- Expected exception.
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 6: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      begin
         declare
            Rec : Include_Record :=
               (Ptr => Foo_Ptr1, C => 'Z');
         begin
            Foo.A := 'D';
            Check_Bar (Rec.Ptr, 'D', Subtest => 7);
         end;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 7: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      begin
         declare
            Rec : Include_Record :=
               (Ptr => Foo_Ptr2, C => 'X');
         begin
            Check_Bar (Rec.Ptr, ' ', Subtest => 8);
         end;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 8: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      -- Discriminant created by a function call:
      begin
         declare
            Rec : Exclude_Record :=
               Make_Exclude (Foo_Ptr1);
         begin
            Foo.A := 'E';
            Check_Bar (Rec.Ptr, 'E', Subtest => 9);
         end;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 9: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      begin
         declare
            Rec : Exclude_Record :=
               Make_Exclude (null);
         begin
            Check_Bar (Rec.Ptr, ' ', Subtest => 10);
            Report.Failed
               ("Null allowed for null-excluding discriminant, case 10");
         end;
      exception
         when Constraint_Error =>
            null; -- Expected exception.
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 10: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      begin
         declare
            Rec : Include_Record :=
               Make_Include (Foo'Access);
         begin
            Foo.A := 'F';
            Check_Bar (Rec.Ptr, 'F', Subtest => 11);
         end;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 11: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      begin
         declare
            Rec : Include_Record :=
               Make_Include (Foo_Ptr2);
         begin
            Check_Bar (Rec.Ptr, ' ', Subtest => 12);
         end;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 12: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end C3A0030;
