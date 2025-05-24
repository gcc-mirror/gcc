-- C650B01.A
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
--     Check that the result of a function that returns a specific tagged type
--     has the tag of the tagged result type, even if the return expression has
--     a different tag.
--
-- TEST DESCRIPTION:
--     We create a function that returns a specific tagged type, with the
--     return expression being a view conversion of various kinds of
--     class-wide expression (object, function return, dereference of
--     access-to-class-wide).
--
--     While the form of the test function isn't very realistic, each
--     of the return expressions is quite plausible. An element of a container
--     of class-wide objects could be accessed by function Element (a
--     class-wide function return) or via function Reference (a dereference
--     of an access-to-class-wide).
--
--     Returning the correct tag is important so that the dispatching rules
--     (particularlly the tag-indeterminant rules) make sense. The rules
--     assume that a function returning a specific tagged type returns an
--     object of that exact type; if an object of some other type is returned,
--     there might not be any tag check to detect the confusion, and perhaps
--     the wrong routine would process the object.
--
--     We do not need to test limited types here, as the build-in-place rules
--     do not allow the view conversion that triggers the problem. For this
--     reason, build-in-place is never required for these return statements;
--     it is always OK for the compiler to copy the object if that is required
--     to change the tag.
--
--     Note that 6.5(5.2/3) requires that the return_subtype_indication be
--     the same specific type; we don't need to test any other types in
--     extended return statements.
--
--     This is an Ada 2012 test as we use the extension to use a specific
--     type in the extended return for a function returning a class-wide type.
--
-- CHANGE HISTORY:
--     20 Aug 2015  RLB  Created test.

with F650B00_2, F650B00_3, F650B00_4;
with Report, TCTouch, Ada.Tags;
procedure C650B01 is

   use type Ada.Tags.Tag;

   type List_Node is access all F650B00_2.Windmill'Class;

   W_List : List_Node := new F650B00_2.Windmill'(F650B00_2.Create); --- 'd'

   P_List : List_Node := new F650B00_3.Pump'(F650B00_3.Create); --- 'h'

   M_List : List_Node := new F650B00_4.Mill'(F650B00_4.Create); --- 'hl'

   Class : F650B00_2.Windmill'Class := M_List.all;

   function Get_Windmill (A_Mill : in Boolean)
      return F650B00_2.Windmill'Class is
   begin
      if A_Mill then
         return M_List.all;
      else
         return M : F650B00_3.Pump := F650B00_3.Create; --- 'h'
      end if;
   end Get_Windmill;

   function Test_Function (Subtest : in Natural) return F650B00_2.Windmill is
   begin
      case Subtest is
         when 1 =>
            return F650B00_2.Windmill(P_List.all);
         when 2 =>
            return F650B00_2.Windmill(Class);
         when 3 =>
            return F650B00_2.Windmill(Get_Windmill (True));
         when 4 =>
            return F650B00_2.Windmill(M_List.all);
         when 5 =>
            return Obj : F650B00_2.Windmill :=
               F650B00_2.Windmill(W_List.all);
         when 6 =>
            return Obj : F650B00_2.Windmill :=
               F650B00_2.Windmill(Class);
         when 7 =>
            return Obj : F650B00_2.Windmill :=
               F650B00_2.Windmill(Get_Windmill (True));
         when 8 =>
            return Obj : F650B00_2.Windmill :=
               F650B00_2.Windmill(Get_Windmill (False)) do --- 'h'
               Obj.Add_Spin (4); --- 'e'
            end return;
         when others => raise Program_Error;
      end case;
   end Test_Function;

   procedure Check_Result (P : in out F650B00_2.Windmill'Class;
                           Subtest : in Natural) is
   begin
      if P'Tag /= F650B00_2.Windmill'Tag then
         Report.Failed ("Wrong tag (" & Natural'Image(Subtest) & ')');
      end if;
      P.Stop;  --- 'f'
      TCTouch.Validate (Expected => "f",
                        Message =>  "Dispatching wrong (" &
                                        Natural'Image(Subtest) & ')');
   end Check_Result;

begin
   Report.Test ("C650B01", "Check that the result of a function that " &
                           "returns a specific tagged type has the tag of " &
                           "the tagged result type, even if the return " &
                           "expression has a different tag");

   W_List.Add_Spin (84); --- 'e'
   P_List.Add_Spin (12); --- 'e'
   M_List.Add_Spin (52); --- 'e'
   Class.Add_Spin (87); --- 'e'

   TCTouch.Validate (Expected => "dhhleeee",
                     Message =>  "Incorrect construction (A)");

   declare
      Res_1 : F650B00_2.Windmill := Test_Function (1);
   begin
      if F650B00_2.Windmill'Class(Res_1)'Tag /= F650B00_2.Windmill'Tag then
         Report.Failed ("Wrong tag (B)");
      end if;
      Check_Result (Res_1, 1);
   end;

   declare
      Res_2 : F650B00_2.Windmill := Test_Function (2);
   begin
      Check_Result (Res_2, 2);
   end;

   declare
      Res_3 : F650B00_2.Windmill := Test_Function (3);
   begin
      Check_Result (Res_3, 3);
   end;

   declare
      Res_4 : F650B00_2.Windmill := Test_Function (4);
   begin
      Check_Result (Res_4, 4);
   end;

   declare
      Res_5 : F650B00_2.Windmill := Test_Function (5);
   begin
      Check_Result (Res_5, 5);
   end;

   declare
      Res_6 : F650B00_2.Windmill := Test_Function (6);
   begin
      Check_Result (Res_6, 6);
   end;

   declare
      Res_7 : F650B00_2.Windmill := Test_Function (7);
   begin
      Check_Result (Res_7, 7);
   end;

   declare
      Res_8 : F650B00_2.Windmill := Test_Function (8);
   begin
      TCTouch.Validate (Expected => "he",
                        Message =>  "Incorrect construction (H)");

      Check_Result (Res_8, 8);
   end;

   Report.Result;
end C650B01;

