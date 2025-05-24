-- C650003.A
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
-- OBJECTIVE:
--     Check that reaching the "end return" of an extended_return_statement
--     that applies to a function causes the function to return.
--
--     Check that a simple return statement in the
--     handled_sequence_of_statements of an extended_return_statement
--     completes the extended_return_statement and causes the function to
--     return.
--
--     Check that completing an extended_return_statement that applies to
--     a function by an exit, goto, or exception propagation does not
--     cause the function to return.
--
-- TEST DESCRIPTION:
--     A series of constructor functions are defined that create limited
--     records. The functions use extended returns that return early
--     for some arguments.
--
--     Note that AI05-0058-1 clarifies the meaning of 6.5(22/2) to make it
--     clear that the third objective is intended.
--
-- CHANGE HISTORY:
--     26 Mar 2008 RLB Created test.
--     07 Nov 2008 RLB Corrected third test objective.
--
with Report;
procedure C650003 is

   type Lim1 is limited record
      Val  : Natural;
      Bool : Boolean;
   end record;

   function Construct (Val : in Natural) return Lim1 is
      -- Check simple return in an extended return and "normal"
      -- extended returns.
   begin
      return Obj : Lim1 do
         Obj.Val := Val;
         while Obj.Val >= 3 loop
            Obj.Val := Obj.Val - 3;
            if Obj.Val = 0 then
               Obj.Val := Val;
               Obj.Bool := True;
               return;
            end if;
         end loop;
         Obj.Val := Val;
         if Val mod Report.Ident_Int(3) = 0 then
            Report.Failed ("Simple return did not return for" & Natural'Image(Val));
         end if;
         Obj.Bool := Report.Ident_Bool(False);
      end return;
      Report.Failed ("Extended return did not return for" & Natural'Image(Val));
   end Construct;


   function Exit_Construct (Val : in Natural) return Lim1 is
      -- Check exit in an extended return and a "normal" simple return.
      Working : Natural := Val;
   begin
      Our_Loop: while Working >= 3 loop
         Working := Working - 3;
         return Obj : Lim1 do
            Obj.Val := Val;
            if Working = 0 then
               Obj.Bool := True;
               exit Our_Loop; -- Does not return!
            end if;
            Obj.Bool := Report.Ident_Bool(False);
            if Working = Report.Ident_Int(0) then
               Report.Failed ("Exit not executed for" & Natural'Image(Val));
            end if;
         end return;
      end loop Our_Loop;
      return (Val => Val, Bool => False);
      Report.Failed ("Normal simple return did not return for" & Natural'Image(Val));
   end Exit_Construct;


   function Goto_Construct (Val : in Natural) return Lim1 is
      -- Check goto in an extended return and a "normal" simple return.
   begin
      return Obj : Lim1 do
         Obj.Val := Val;
         if Report.Ident_Int(Val) mod 7 = 0 then
            Obj.Bool := True;
            goto Skip; -- Does not return!
         end if;
         Obj.Bool := Report.Ident_Bool(False);
         if Val mod Report.Ident_Int(7) = 0 then
            Report.Failed ("Goto not executed for" & Natural'Image(Val));
         end if;
      end return;
   <<Skip>> null;
      return (Val => Val, Bool => False);
      Report.Failed ("Normal simple return did not return for" & Natural'Image(Val));
   end Goto_Construct;


   function Except_Construct (Val : in Natural) return Lim1 is
      -- Check exceptions in an extended return and a "normal" simple return.
   begin
      return Obj : Lim1 do
         Obj.Val := Val;
         if Report.Ident_Int(Val) mod 4 = 0 then
            Obj.Bool := True;
            raise Program_Error; -- Does not return!
         end if;
         Obj.Bool := Report.Ident_Bool(False);
         if Val mod Report.Ident_Int(4) = 0 then
            Report.Failed ("Exception not raised for" & Natural'Image(Val));
         end if;
      end return;
      Report.Failed ("Extended return did not return for" & Natural'Image(Val));
   exception
      when Program_Error =>
         if Val mod Report.Ident_Int(4) /= 0 then
            Report.Failed ("Exception raised for wrong case" & Natural'Image(Val));
         end if;
         return (Val => Val/2, Bool => False);
         Report.Failed ("Normal simple return did not return for" & Natural'Image(Val));
   end Except_Construct;


   generic
      type Disc is (<>);
   package Gen is
      type Lim2 is limited record
         Val  : Disc;
         Bool : Boolean;
      end record;
      function Construct (Val : in Disc) return Lim2;
   end Gen;

   package body Gen is
      function Construct (Val : in Disc) return Lim2 is
         -- Check simple return in an extended return and "normal"
         -- extended returns.
      begin
         return Obj : Lim2 do
            Obj.Val := Val;
            if Report.Ident_Int(Disc'Pos(Val)) mod 2 = 0 then
               Obj.Bool := True;
               return;
            end if;
            if Disc'Pos(Val) mod Report.Ident_Int(2) = 0 then
               Report.Failed ("Simple return did not return for" & Disc'Image(Val));
            end if;
            Obj.Bool := Report.Ident_Bool(False);
         end return;
         Report.Failed ("Extended return did not return for" & Disc'Image(Val));
      end Construct;
   end Gen;

   type Enum is (Zero, One, Two, Three);
   package Inst is new Gen (Enum);

   generic
      Mod_Value : in Natural;
   function Gen_Con (Val : in Natural) return Lim1;

   function Gen_Con (Val : in Natural) return Lim1 is
      -- Check simple return in an extended return and "normal"
      -- extended returns in a generic function.
   begin
      return Obj : Lim1 do
         Obj.Val := Val;
         if Report.Ident_Int(Val) mod Mod_Value = 0 then
            Obj.Bool := True;
            return;
         end if;
         if Val mod Mod_Value = 0 then
            Report.Failed ("Simple return did not return for" & Natural'Image(Val));
         end if;
         Obj.Bool := Report.Ident_Bool(False);
      end return;
      Report.Failed ("Extended return did not return for" & Natural'Image(Val));
   end Gen_Con;

   function Inst_Construct is new Gen_Con (Mod_Value => 5);

begin
   Report.Test ("C650003", "Check that an extended return statement can be " &
                           "completed by a simple return statement; and that " &
                           "completing the extended return returns from the " &
                           "enclosing function");

   declare
      O1 : Lim1 := Construct (Val => 3);
      O2 : Lim1 := Construct (Val => 8);
      O3 : Inst.Lim2 := Inst.Construct (Val => One);
      O4 : Inst.Lim2 := Inst.Construct (Val => Two);
      O5 : Lim1 := Inst_Construct (Val => 5);
      O6 : Lim1 := Inst_Construct (Val => 11);
      O7 : Lim1 := Exit_Construct (Val => 6);
      O8 : Lim1 := Exit_Construct (Val => 4);
      O9 : Lim1 := Goto_Construct (Val => 7);
      OA : Lim1 := Goto_Construct (Val => 9);
      OB : Lim1 := Except_Construct (Val => 12);
      OC : Lim1 := Except_Construct (Val => 13);
   begin
      if O1.Val /= 3 or (not O1.Bool) then
         Report.Failed ("Wrong value for O1");
      end if;
      if O2.Val /= 8 or (O2.Bool) then
         Report.Failed ("Wrong value for O2");
      end if;
      if O3.Val /= One or (O3.Bool) then
         Report.Failed ("Wrong value for O3");
      end if;
      if O4.Val /= Two or (not O4.Bool) then
         Report.Failed ("Wrong value for O4");
      end if;
      if O5.Val /= 5 or (not O5.Bool) then
         Report.Failed ("Wrong value for O5");
      end if;
      if O6.Val /= 11 or (O6.Bool) then
         Report.Failed ("Wrong value for O6");
      end if;
      if O7.Val /= 6 or (O7.Bool) then
         Report.Failed ("Wrong value for O7 - exited return statement returned");
      end if;
      if O8.Val /= 4 or (O8.Bool) then
         Report.Failed ("Wrong value for O8");
      end if;
      if O9.Val /= 7 or (O9.Bool) then
         Report.Failed ("Wrong value for O9 - goto out of return statement " &
                        "returned");
      end if;
      if OA.Val /= 9 or (OA.Bool) then
         Report.Failed ("Wrong value for OA");
      end if;
      if OB.Val /= 6 or (OB.Bool) then
         Report.Failed ("Wrong value for OB - exception propagation out of " &
                        "return statement returned");
      end if;
      if OC.Val /= 13 or (OC.Bool) then
         Report.Failed ("Wrong value for OC");
      end if;
   end;

   Report.Result;

end C650003;
