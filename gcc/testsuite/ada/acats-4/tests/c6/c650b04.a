-- C650B04.A
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
--     Check that Program_Error is raised if the tag identified by the result
--     object for a function returning a class-wide type has a master that
--     does not include the elaboration of the master that elaborated the
--     function body. Case 1: Nested subprograms.
--
-- TEST DESCRIPTION:
--     We try returning objects of types declared at various nesting levels.
--     Note that a type declared at an inner or parallel nesting level is
--     not visible, and thus can only occur at run-time when passed in as
--     a parameter.
--
--     The test cases here are derived from the legal cases in test
--     B650005, which tests the matching Legality Rule. (Obviously, we only
--     need to test legal cases.)
--
--     While the form of the test functions are not very realistic by itself,
--     the danger caused by this error being undetected can be severe, as
--     one can dispatch to a subprogram that does not exist in Ada terms.
--     The best case is that *just* a nonexistent scalar object is used (as
--     in this example); the effects can get worse all the way to writing
--     some other tasks's memory.
--
--     Note: We do not use the TcTouch results in this test.
--
-- CHANGE HISTORY:
--     21 Jan 2016  RLB  Created test.
--     22 Jan 2016  RLB  Added parameter subtests.

with F650B00_2, F650B00_3;
with Report, TCTouch;
procedure C650B04 is

   type List_Node is access all F650B00_2.Windmill'Class;

   P_List : List_Node := new F650B00_3.Pump'(F650B00_3.Create);

   W_Obj : F650B00_2.Windmill := F650B00_2.Create;

   Class_Obj : F650B00_2.Windmill'Class := W_Obj;

   function Get_Windmill return F650B00_2.Windmill is
   begin
      return F650B00_2.Create; --- 'd'
   end Get_Windmill;

   function Return_Param (Obj     : in F650B00_2.Windmill'Class;
                          Use_Ext : in Boolean)
      return F650B00_2.Windmill'Class is
   begin
      if Use_Ext then
         return Result : F650B00_2.Windmill'Class := Obj do
             null; -- Might raise Program_Error,
                   -- depending upon the tag of the object.
         end return;
      else
         return Obj; -- Might raise Program_Error,
                     -- depending upon the tag of the object.
      end if;
   end Return_Param;

   function Test_Function (Subtest : in Natural)
      return F650B00_2.Windmill'Class is

      type Nest_Windmill is new F650B00_2.Windmill with record
          Id : Natural;
      end record;

      overriding
      function Create return Nest_Windmill;

      overriding
      function Spin (Mill : in Nest_Windmill)
         return F650B00_2.Rotational_Measurement;

      overriding
      procedure Add_Spin (To_Mill : in out Nest_Windmill;
                          RPMs    : in     F650B00_2.Rotational_Measurement) is
         use type F650B00_2.Rotational_Measurement;
      begin
         F650B00_2.Add_Spin (F650B00_2.Windmill(To_Mill),
            RPMs + F650B00_2.Rotational_Measurement(Subtest));
      end Add_Spin;

      overriding
      function Create return Nest_Windmill is
          A : Nest_Windmill;
      begin
          A.Id := Subtest;
          return A;
      end Create;

      Saved_Id : Natural := 0;
      Saved_Spin : F650B00_2.Rotational_Measurement := 0;

      overriding
      function Spin (Mill : in Nest_Windmill)
         return F650B00_2.Rotational_Measurement is
         -- A memo function version of Spin.
      begin
         if Mill.Id = Saved_Id then
            return Saved_Spin;
         else
            Saved_Spin := F650B00_2.Spin (F650B00_2.Windmill(Mill));
            Saved_Id := Mill.Id;
            return Saved_Spin;
         end if;
      end Spin;

      N_Obj : aliased Nest_Windmill := Create;

      N_Class_Obj : F650B00_2.Windmill'Class := N_Obj;

      function Nest_Test_Function (N : in Natural;
                                   ACW : access Nest_Windmill'Class)
         return Nest_Windmill'Class is
         type Inner_Windmill is new Nest_Windmill with null record;
         overriding

         procedure Add_Spin
            (To_Mill : in out Inner_Windmill;
             RPMs    : in     F650B00_2.Rotational_Measurement) is
            use type F650B00_2.Rotational_Measurement;
         begin
            F650B00_2.Add_Spin (F650B00_2.Windmill(To_Mill),
               RPMs + F650B00_2.Rotational_Measurement(N));
         end Add_Spin;

         IW_Obj : Inner_Windmill := Create;

      begin
         case N is
            when 1 =>
               return Obj : Nest_Windmill do       -- OK here, but P_E
                   null;                           -- for Test_Function
               end return;
            when 2 =>
               return Obj : Nest_Windmill'Class := -- OK here, but P_E for
                                Nest_Test_Function (N-1, ACW) do  -- Test_Func
                   null;
               end return;
            when 3 =>
               return Nest_Test_Function (N-2, ACW); -- OK here, but P_E
                                                     -- for Test_Function
            when 4 =>
               return Obj : Nest_Windmill'Class := -- P_E here.
                                Nest_Windmill'Class(IW_Obj) do
                   null;
               end return;
            when 5 =>
               return Nest_Windmill'Class(IW_Obj); -- P_E here.
            when 6 =>
               return Obj : Nest_Windmill'Class := -- OK here, but P_E
                               ACW.all do          -- for Test_Function.
                   null;
               end return;
            when 7 =>
               return ACW.all;                     -- OK here, but P_E
                                                   -- for Test_Function.
            when 8 =>
               return Nest_Windmill'Class(
                   Return_Param (F650B00_2.Windmill'Class(IW_Obj),
                                 Use_Ext => True));   -- P_E here.
            when 9 =>
               return Nest_Windmill'Class(
                   Return_Param (F650B00_2.Windmill'Class(IW_Obj),
                                 Use_Ext => False));  -- P_E here.
            when 10 =>
               return Nest_Windmill'Class(
                   Return_Param (F650B00_2.Windmill'Class(N_Obj),
                                 Use_Ext => True));   -- P_E here.
            when 11 =>
               return Nest_Windmill'Class(
                   Return_Param (F650B00_2.Windmill'Class(N_Obj),
                                 Use_Ext => False));  -- P_E here.

            when others =>
               raise Constraint_Error with "Incorrect parameter";
         end case;
      end Nest_Test_Function;

   begin
      declare
         Local_Val : F650B00_2.Rotational_Measurement :=
            F650B00_2.Rotational_Measurement(Subtest/2);

         type Blk_Windmill is new F650B00_2.Windmill with null record;

         overriding
         procedure Add_Spin
            (To_Mill : in out Blk_Windmill;
             RPMs    : in     F650B00_2.Rotational_Measurement) is
            use type F650B00_2.Rotational_Measurement;
         begin
            F650B00_2.Add_Spin (F650B00_2.Windmill(To_Mill),
               RPMs + Local_Val);
         end Add_Spin;

         L_Obj : Blk_Windmill := Create;

      begin
         case Subtest is
            when 1 =>
               return Obj : F650B00_2.Windmill'Class := W_Obj do      -- OK.
                  null;
               end return;
            when 2 =>
               return Class_Obj;                                      -- OK.
            when 3 =>
               return Obj : F650B00_2.Windmill'Class := Class_Obj do  -- OK.
                  null;
               end return;
            when 4 =>
               return Obj : F650B00_2.Windmill'Class :=               -- P_E.
                  F650B00_2.Windmill'Class(L_Obj) do
                  null;
               end return;
            when 5 =>
               return F650B00_2.Windmill'Class(L_Obj);                -- P_E.
            when 6 =>
               return Obj : F650B00_2.Windmill'Class :=               -- P_E.
                  F650B00_2.Windmill'Class(N_Obj) do
                  null;
               end return;
            when 7 =>
               return F650B00_2.Windmill'Class(N_Obj);                -- P_E.
            when 8 =>
               return Obj : F650B00_2.Windmill'Class :=
                                                  P_List.all do       -- OK.
                  null;
               end return;
            when 9 =>
               return P_List.all;                                     -- OK.
            when 10..16 =>
               return Obj : F650B00_2.Windmill'Class :=
                  F650B00_2.Windmill'Class(
                      Nest_Test_Function(Subtest-9, N_Obj'Access)) do
                  null;
               end return;
            when 17..23 =>
               return F650B00_2.Windmill'Class(
                            Nest_Test_Function(Subtest-16, N_Obj'Access));
            when 24..27 =>
               return F650B00_2.Windmill'Class(
                            Nest_Test_Function(Subtest-16, N_Obj'Access));
            when 28 =>
               return Return_Param (F650B00_2.Windmill'Class(N_Obj),
                                    Use_Ext => False);                -- P_E.
            when 29 =>
               return Return_Param (F650B00_2.Windmill'Class(N_Obj),
                                    Use_Ext => True);                 -- P_E.
            when 30 =>
               return Return_Param (F650B00_2.Windmill'Class(L_Obj),
                                    Use_Ext => False);                -- P_E.
            when 31 =>
               return Return_Param (F650B00_2.Windmill'Class(L_Obj),
                                    Use_Ext => True);                 -- P_E.
            when 32 =>
               return Return_Param (Class_Obj,
                                    Use_Ext => False);                -- OK.
            when 33 =>
               return Return_Param (Class_Obj,
                                    Use_Ext => True);                 -- OK.

            when others =>
               raise Constraint_Error with "Incorrect parameter";
         end case;
      end;
   end Test_Function;


   Base_RPM : constant F650B00_2.Rotational_Measurement := 20;

   procedure Check_Result (Expected_Spin : in F650B00_2.Rotational_Measurement;
                           Fails   : in Boolean;
                           Subtest : in Natural) is
   begin
      declare
         Obj : F650B00_2.Windmill'Class := Test_Function(Subtest);
      begin
         if Fails then
            Report.Failed ("Test function did not raise Program_Error " &
                "as expected (" & Natural'Image(Subtest) & ')');
         end if;
         F650B00_2.Add_Spin (Obj, Base_RPM);
         if F650B00_2."/=" (F650B00_2.Spin (Obj), Expected_Spin) then
            Report.Failed ("Unexpected spin value (" &
                Natural'Image(Subtest) & ')');
         end if;
      end;
   exception
      when Program_Error =>
         if Fails then
            null;
            --Report.Comment ("Test function raised Program_Error " &
            --    "as expected (" & Natural'Image(Subtest) & ')');
         else
            Report.Failed ("Test function unexpectedly raised " &
                "Program_Error (" & Natural'Image(Subtest) & ')');
         end if;
   end Check_Result;

   use type F650B00_2.Rotational_Measurement;

begin
   Report.Test ("C650B04", "Check that Program_Error is raised if the tag " &
                           "identified by the result object for a function " &
                           "returning a class-wide type has a master that " &
                           "does not include the elaboration of the master " &
                           "that elaborated the function body. Case 1: " &
                           "Nested subprograms");

   Check_Result (Base_RPM, Fails => False, Subtest => 1);

   Check_Result (Base_RPM, Fails => False, Subtest => 2);

   Check_Result (Base_RPM, Fails => False, Subtest => 3);

   Check_Result (Base_RPM + 2, Fails => True, Subtest => 4);

   Check_Result (Base_RPM + 2, Fails => True, Subtest => 5);

   Check_Result (Base_RPM + 6, Fails => True, Subtest => 6);

   Check_Result (Base_RPM + 7, Fails => True, Subtest => 7);

   Check_Result (Base_RPM, Fails => False, Subtest => 8);

   Check_Result (Base_RPM, Fails => False, Subtest => 9);

   Check_Result (Base_RPM + 10, Fails => True, Subtest => 10);

   Check_Result (Base_RPM + 11, Fails => True, Subtest => 11);

   Check_Result (Base_RPM + 12, Fails => True, Subtest => 12);

   Check_Result (Base_RPM + 4, Fails => True, Subtest => 13);

   Check_Result (Base_RPM + 5, Fails => True, Subtest => 14);

   Check_Result (Base_RPM + 15, Fails => True, Subtest => 15);

   Check_Result (Base_RPM + 16, Fails => True, Subtest => 16);

   Check_Result (Base_RPM + 17, Fails => True, Subtest => 17);

   Check_Result (Base_RPM + 18, Fails => True, Subtest => 18);

   Check_Result (Base_RPM + 19, Fails => True, Subtest => 19);

   Check_Result (Base_RPM + 4, Fails => True, Subtest => 20);

   Check_Result (Base_RPM + 5, Fails => True, Subtest => 21);

   Check_Result (Base_RPM + 22, Fails => True, Subtest => 22);

   Check_Result (Base_RPM + 23, Fails => True, Subtest => 23);

   Check_Result (Base_RPM + 8, Fails => True, Subtest => 24);

   Check_Result (Base_RPM + 9, Fails => True, Subtest => 25);

   Check_Result (Base_RPM + 26, Fails => True, Subtest => 26);

   Check_Result (Base_RPM + 27, Fails => True, Subtest => 27);

   Check_Result (Base_RPM + 26, Fails => True, Subtest => 28);

   Check_Result (Base_RPM + 27, Fails => True, Subtest => 29);

   Check_Result (Base_RPM + 15, Fails => True, Subtest => 30);

   Check_Result (Base_RPM + 15, Fails => True, Subtest => 31);

   Check_Result (Base_RPM, Fails => False, Subtest => 32);

   Check_Result (Base_RPM, Fails => False, Subtest => 33);

   Report.Result;
end C650B04;

