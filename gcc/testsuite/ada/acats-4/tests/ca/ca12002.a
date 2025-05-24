-- CA12002
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
--
-- OBJECTIVE:
--    Check that the declaration of a public descendant of a unit L can
--    mention a private child of L in a private_with_clause.
--
--    Check that a public descendant of a public root library unit can be
--    mentioned in a private_with_clause on any unit.
--
--    Check that a name mentioned in a private with clause can be used in
--    a private part, including those of nested and descendant packages.
--
--    Check that a name mentioned in a private with clause can be used in
--    the visible part of a private descendant, in a body, and in a subunit.
--
--
-- CHANGE HISTORY:
--    06 May 2003   PHL   Initial version.
--    25 Apr 2007   RLB   Updated test for ACATS.
--!
package CA12002_Parent is
end CA12002_Parent;


with Report;
pragma Elaborate (Report);
private package CA12002_Parent.Priv_Pack_Child is
   X : constant Integer := Report.Ident_Int (5);
end CA12002_Parent.Priv_Pack_Child;


private procedure CA12002_Parent.Priv_Proc_Child (X : out Integer);


with Report;
procedure CA12002_Parent.Priv_Proc_Child (X : out Integer) is
begin
   X := Report.Ident_Int (7);
end CA12002_Parent.Priv_Proc_Child;


private function CA12002_Parent.Priv_Func_Child return Boolean;


with Report;
function CA12002_Parent.Priv_Func_Child return Boolean is
begin
   return Report.Ident_Bool (False);
end CA12002_Parent.Priv_Func_Child;


private with CA12002_Parent.Priv_Pack_Child;
private with CA12002_Parent.Priv_Proc_Child;
private with CA12002_Parent.Priv_Func_Child;
pragma Elaborate (CA12002_Parent.Priv_Pack_Child);
pragma Elaborate_All (CA12002_Parent.Priv_Func_Child);
package CA12002_Parent.Pub_Pack_Child_1 is
   X : constant Integer;
   Y : constant Boolean;
   procedure P (X : out Integer);
   function F return Integer;
   package Nested is
      Z : constant Integer;
   private
      Z : constant Integer := CA12002_Parent.Priv_Pack_Child.X * 3;
   end Nested;
private
   X : constant Integer := CA12002_Parent.Priv_Pack_Child.X;
   Y : constant Boolean := CA12002_Parent.Priv_Func_Child;
end CA12002_Parent.Pub_Pack_Child_1;


package body CA12002_Parent.Pub_Pack_Child_1 is
   procedure P (X : out Integer) renames CA12002_Parent.Priv_Proc_Child;
   function F return Integer is separate;
end CA12002_Parent.Pub_Pack_Child_1;


separate (CA12002_Parent.Pub_Pack_Child_1)
function F return Integer is
begin
   return CA12002_Parent.Priv_Pack_Child.X - 1;
end F;


package CA12002_Parent.Pub_Pack_Child_1.Pub_Grandchild is
   W : constant Boolean;
private
   W : constant Boolean := not CA12002_Parent.Priv_Func_Child;
end CA12002_Parent.Pub_Pack_Child_1.Pub_Grandchild;



private with CA12002_Parent.Priv_Pack_Child;
package CA12002_Parent.Pub_Pack_Child_2 is

   function F return Float;

end CA12002_Parent.Pub_Pack_Child_2;


private package CA12002_Parent.Pub_Pack_Child_2.Priv_Grandchild is
   X : constant Integer := CA12002_Parent.Priv_Pack_Child.X + 2;
end CA12002_Parent.Pub_Pack_Child_2.Priv_Grandchild;


package CA12002_Parent.Pub_Pack_Child_2.Priv_Grandchild.Pub_Greatgrandchild is
   X : constant Float :=
      Float (3 * CA12002_Parent.Pub_Pack_Child_2.Priv_Grandchild.X);
end CA12002_Parent.Pub_Pack_Child_2.Priv_Grandchild.Pub_Greatgrandchild;


with CA12002_Parent.Pub_Pack_Child_2.Priv_Grandchild.Pub_Greatgrandchild;
package body CA12002_Parent.Pub_Pack_Child_2 is

   function F return Float is
   begin
      return
         CA12002_Parent.Pub_Pack_Child_2.Priv_Grandchild.Pub_Greatgrandchild.X;
   end F;

end CA12002_Parent.Pub_Pack_Child_2;


private with CA12002_Parent.Pub_Pack_Child_1.Pub_Grandchild;
with CA12002_Parent.Pub_Pack_Child_2;
with Report;
use Report;
procedure CA12002 is
begin
   Test ("CA12002",
         "Check that the declaration of a public descendant of a unit L can " &
         "mention a private child of L in a private with clause. " &
         "Check that a public descendant of a public root library unit can " &
         "be mentioned in a private with clause on any unit. Check that a " &
         "name mentioned in a private with clause can be used in a private " &
         "part, including those of nested and descendant packages. " &
         "Check that a name mentioned in a private with clause can be used " &
         "in the visible part of a private descendant, in a body, and " &
         "in a subunit");

   if CA12002_Parent.Pub_Pack_Child_1.X /= Report.Ident_Int (5) then
      Failed ("Error referencing object from private package");
   end if;

   if CA12002_Parent.Pub_Pack_Child_1.Y then
      Failed ("Error calling private function");
   end if;

   if CA12002_Parent.Pub_Pack_Child_1.Nested.Z /= Report.Ident_Int (15) then
      Failed ("Error referencing object from private package in nested package");
   end if;

   declare
      X : Integer := Report.Ident_Int (1);
   begin
      CA12002_Parent.Pub_Pack_Child_1.P (X);
      if X /= Report.Ident_Int (7) then
         Failed ("Error calling private procedure");
      end if;
   end;

   if CA12002_Parent.Pub_Pack_Child_1.F /= Report.Ident_Int (4) then
      Failed ("Error calling separate function");
   end if;

   if not CA12002_Parent.Pub_Pack_Child_1.Pub_Grandchild.W then
      Failed ("Error calling private function in grandchild");
   end if;

   if CA12002_Parent.Pub_Pack_Child_2.F /= 21.0 then
      Failed ("Error referencing object through private descendant");
   end if;

   Result;
end CA12002;
