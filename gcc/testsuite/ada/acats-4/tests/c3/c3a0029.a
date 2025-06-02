-- C3A0029.A
--
--                            Grant of Unlimited Rights
--
--    AdaCore holds unlimited rights in the software and documentation
--    contained herein. Unlimited rights are the same as those granted
--    by the U.S. Government for older parts of the Ada Conformity
--    Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--    By making this public release, AdaCore intends to confer upon all
--    recipients unlimited rights equal to those held by the Ada Conformity
--    Assessment Authority. These rights include rights to use, duplicate,
--    release or disclose the released technical data and computer software
--    in whole or in part, in any manner and for any purpose whatsoever,
--    and to have or permit others to do so.
--
--                                   DISCLAIMER
--
--    ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--    DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--    TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--    DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--    DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--    PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                    Notice
--
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--
--    This test is based on one submitted by AdaCore; AdaCore retains the
--    copyright on the test.
--*
--  OBJECTIVE:
--    Check that Constraint_Error is raised when a null access value is
--    converted to a null excluding subtype of a named access-to-subprogram
--    type. Case B: null_exclusion given at point of use of the access-
--    to-normal-subprogram type.
--
--    Check that a null_exclusion can be given in a subtype_indication,
--    discriminant_specification, parameter_specification, function result
--    subtype, and formal_object_declaration if the subtype_mark is a
--    access-to-subprogram subtype that does not exclude null.
--
--    Check that Constraint_Error is raised when a null access value is
--    converted to a null excluding anonynous access-to-protected-subprogram
--    type.
--
--  CHANGE HISTORY:
--    02 May 2008 RLB Created ACATS test based on C3A0027.
--    18 Jul 2008 RLB Fixed return statement error.
--!
with Report;
procedure C3A0029 is

   protected PO is
      function Double (X : Float) return Float;
      function Triple (X : Float) return Float;
   end PO;

   protected body PO is
      function Double (X : Float) return Float is
      begin
         return 2.0 * X;
      end Double;

      function Triple (X : Float) return Float is
      begin
         return 3.0 * X;
      end ;
   end PO;

   type Acc_Func_Ptr is access protected function (X : Float) return Float;

   function Null_Init return Acc_Func_Ptr is
   begin
      return null;
   end Null_Init;

   function Double_Init return Acc_Func_Ptr is
   begin
      return PO.Double'Access;
   end Double_Init;

   procedure Use_It (Obj : Acc_Func_Ptr) is
      -- Use Obj so that the compiler cannot remove the actual from
      -- the program as a dead object.
   begin
      if Obj = null then
         null;
      elsif Report.Equal (Integer(Obj(2.0)), 0) then
         Report.Comment ("Don't optimize Obj");
      end if;
   end Use_It;

begin
   Report.Test ("C3A0029", "Check that Constraint_Error is raised when a null " &
                           "access value is converted to a null excluding " &
                           "subtype of a named access-to-protected-subprogram type");

   --  Object declaration tests --------------------------------------------

   begin
      declare
         O_1 : not null Acc_Func_Ptr;    -- Test (default initialization).
      begin
         Report.Failed ("O_1: Wrong value");
         Use_It (O_1);
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   begin
      declare
         O_2 : not null Acc_Func_Ptr := Null_Init;      -- Test (initialization).
      begin
         Report.Failed ("O_2: Wrong value");
         Use_It (O_2);
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      O_3 : not null Acc_Func_Ptr := Double_Init;
   begin
      O_3 := Null_Init;                            -- Test (assignment).
      Report.Failed ("O_3: Wrong value");
      Use_It (O_3);
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      O_4 : not null Acc_Func_Ptr := Double_Init;
   begin
      if O_4 (5.0) /= 10.0 then
         Report.Failed ("Wrong result from O_4");
      end if;
   end;


   --  Array component and aggregate tests ------------------------------

   declare
      type Table is array (Integer range <>) of not null Acc_Func_Ptr;
      T      : Table (1 .. 2) := (PO.Double'Access, PO.Triple'Access);
      Result : Float;
   begin
      Result := T(1) (3.0);
      if Result /= 6.0 then
         Report.Failed ("Wrong value at T(1)");
      end if;

      Result := T(2) (3.0);
      if Result /= 9.0 then
         Report.Failed ("Wrong value at T(2)");
      end if;

      begin
         T(1) := Null_Init;  -- Test (component assignment)
         Report.Failed ("Missing null-exclusion check (AO_1)");
         Use_It (T(1));
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         declare
            AO_2 : Table (1 .. 2) := (Double_Init, Null_Init); -- Test (aggregate)
         begin
            Report.Failed ("Missing null-exclusion check (AO_2)");
            Use_It (AO_2(2));
         end;
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         declare
            AO_3 : Table (1 .. 2); -- Test (default initialization)
         begin
            Report.Failed ("Missing null-exclusion check (AO_3)");
            Use_It (AO_3(2));
         end;
      exception
         when Constraint_Error =>
            null;
      end;
   end;


   --  Record component and aggregate tests -----------------------------

   begin
      declare
         type Rec is record
            C : not null Acc_Func_Ptr;
         end record;
         RO_1 : Rec;       -- Test (implicit record component default initialization).
      begin
         Report.Failed ("RO_1: Wrong value");
         Use_It (RO_1.C);
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   begin
      declare
         type Rec is record
            C : not null Acc_Func_Ptr;
         end record;
         RO_2 : Rec := (C => Null_Init);   -- Test (record aggregate).
      begin
         Report.Failed ("RO_2: Wrong value");
         Use_It (RO_2.C);
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   begin
      declare
         type Rec is record
            C : not null Acc_Func_Ptr := Null_Init;
         end record;
         RO_3 : Rec;       -- Test (explicit record component default initialization).
      begin
         Report.Failed ("RO_3: Wrong value");
         Use_It (RO_3.C);
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   begin
      declare
         type Rec is record
            C : not null Acc_Func_Ptr := Null_Init;
         end record;
         RO_4 : Rec := (C => Double_Init); -- Default expression not evaluated.
      begin
         if RO_4.C (4.5) /= 9.0 then
            Report.Failed ("Wrong value at RO_4.C");
         end if;
         RO_4.C := Null_Init;           -- Test (record component assignment).
         Report.Failed ("RO_4: Wrong value");
         Use_It (RO_4.C);
      exception
         when Constraint_Error =>
            null;
      end;
   exception
      when Constraint_Error =>
         Report.Failed ("RO_4: Constraint_Error raised");
   end;

   declare
      type Rec is record
         C : not null Acc_Func_Ptr := Double_Init;
      end record;
      RO_5 : Rec;
   begin
      RO_5.C := Null_Init;           -- Test (record component assignment).
      Report.Failed ("RO_5: Wrong value");
      Use_It (RO_5.C);
   exception
      when Constraint_Error =>
         null;
   end;


   --  Discriminant tests -----------------------------------------------

   begin
      declare
         type Rec (D : not null Acc_Func_Ptr) is
            record
               Dummy : Integer := 0;
            end record;
         RO_6 : Rec (Null_Init);        -- Test (discriminant constraint).
      begin
         RO_6.Dummy := 0;
         Report.Failed ("RO_6: Wrong value");
         Use_It (RO_6.D);
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   begin
      declare
         type Rec (D : not null Acc_Func_Ptr := Null_Init) is
            limited record
               Dummy : Integer := 0;
            end record;
         RO_7 : Rec;           -- Test (defaulted discriminant).
      begin
         RO_7.Dummy := 0;
         Report.Failed ("RO_7: Wrong value");
         Use_It (RO_7.D);
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      type Rec (D : not null Acc_Func_Ptr := Null_Init) is
         limited record
            Dummy : Integer := 0;
         end record;
      RO_8 : Rec (Double_Init);
   begin
      if RO_8.D (1.5) /= 3.0 then
         Report.Failed ("Wrong value at RO_8.D");
      end if;
   end;


   --  Parameter tests --------------------------------------------------

   declare
      procedure Proc_1 (F : not null Acc_Func_Ptr) is
      begin
         Use_It (F);
      end Proc_1;
   begin
      begin
         Proc_1 (Null_Init);
         Report.Failed ("Proc_1: Wrong value");
      exception
         when Constraint_Error =>
            null;
      end;
   end;

   begin
      declare
         procedure Proc_2 (F :
            not null Acc_Func_Ptr := Null_Init) is
         begin
            Use_It (F);
         end Proc_2;
      begin
         Proc_2;
         Report.Failed ("Proc_2: Wrong value");
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      function Evaluate
        (Fn : not null Acc_Func_Ptr; X  : Float) return Float is
      begin
         return Fn (X);
      end Evaluate;

      Result : Float;
   begin
      Result := Evaluate (Double_Init, 3.0);
      if Result /= 6.0 then
         Report.Failed ("Wrong value in called function");
      end if;

      begin
         Result := Evaluate (Null_Init, 3.0);
         Report.Failed ("Missing null-exclusion check - function");
         if Result /= 6.0 then
            Report.Comment ("Don't optimize Result");
         end if;
      exception
         when Constraint_Error =>
            null;
      end;
   end;


   --  Result subtype tests ---------------------------------------------

   declare
      function Func_1 (F : Acc_Func_Ptr) return not null Acc_Func_Ptr is
      begin
         return F;
      end Func_1;
      Obj : Acc_Func_Ptr;
   begin
      begin
         Obj := Func_1 (Null_Init);
         Report.Failed ("Func_1: Wrong value");
         Use_It (Obj);
      exception
         when Constraint_Error =>
            null;
      end;
      Obj := Func_1 (Double_Init);
      if Obj(4.0) /= 8.0 then
         Report.Failed ("Func_1_OK: Wrong value");
      end if;
   end;

   declare
      function Func_2 (F : Acc_Func_Ptr) return not null Acc_Func_Ptr is
      begin
         return Result : not null Acc_Func_Ptr := PO.Double'Access do
            Result := F;
         end return;
      end Func_2;
      Obj : Acc_Func_Ptr;
   begin
      begin
         Obj := Func_2 (Null_Init);
         Report.Failed ("Func_2: Wrong value");
         Use_It (Obj);
      exception
         when Constraint_Error =>
            null;
      end;
      Obj := Func_2 (Double_Init);
      if Obj(7.0) /= 14.0 then
         Report.Failed ("Func_2_OK: Wrong value");
      end if;
   end;


   --  Formal object tests ----------------------------------------------

   declare
      generic
         F : not null Acc_Func_Ptr := Null_Init;
      package GPack is
         procedure User;
      end GPack;

      package body GPack is
         procedure User is
         begin
            Use_It (F);
         end User;
      end GPack;
   begin
      begin
         declare
            package Inst_1 is new GPack; -- Object default.
         begin
            Report.Failed ("Inst_1: Wrong value");
            Inst_1.User;
         end;
      exception
         when Constraint_Error =>
            null;
      end;
      begin
         declare
            package Inst_2 is new GPack (Null_Init);
         begin
            Report.Failed ("Inst_2: Wrong value");
            Inst_2.User;
         end;
      exception
         when Constraint_Error =>
            null;
      end;
      begin
         declare
            package Inst_3 is new GPack (Double_Init);
         begin
            Inst_3.User;
         end;
      exception
         when Constraint_Error =>
            Report.Failed ("Inst_3: Constraint_Error raised");
      end;
   end;

   Report.Result;
end C3A0029;
