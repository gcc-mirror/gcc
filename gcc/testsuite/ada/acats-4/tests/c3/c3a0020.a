-- C3A0020.A
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
--    converted to a null excluding subtype of a named access type.
--    Case B: null_exclusion given in a subtype of a general access type.
--
--  CHANGE HISTORY:
--    17 MAR 2004 JM  Initial Version
--    30 Apr 2008 RLB Converted to ACATS test, adding test cases (many
--                    borrowed from submitted test CY30040), and splitting
--                    into several parts to keep size manageable.
--    02 May 2008 RLB Added missing optimization breakers.
--    18 Jul 2008 RLB Fixed return statement error.
--!

with Report;
procedure C3A0020 is
   type    Int_Ptr              is access all Integer;

   subtype Not_Null_Subtype is not null Int_Ptr;

   type NN_Int_Ptr_Array is array (1 .. 3) of Not_Null_Subtype;

   type Int_Ptr_Ptr is access Int_Ptr;

   function Null_Init return Int_Ptr is
   begin
      return null;
   end Null_Init;

   Aux : aliased Integer := 99;
   function Non_Null_Init return Int_Ptr is
   begin
      return Aux'Access;
   end Non_Null_Init;

   procedure Use_It (Obj : in Int_Ptr) is
      -- Use Obj so that the compiler cannot remove the actual from
      -- the program as a dead object.
   begin
      if Obj = null then
         null;
      elsif not Report.Equal (Obj.all, Obj.all) then
         Report.Comment ("Don't optimize Obj");
      end if;
   end Use_It;

begin
   Report.Test ("C3A0020", "Check that Constraint_Error is raised when a " &
                           "null access value is converted to a null " &
                           "excluding subtype of a named access type. " &
                           "Case B: null_exclusion given in a subtype of " &
                           "a general access type");

   --  Object declaration tests --------------------------------------------

   begin
      declare
         O_1 : Not_Null_Subtype;       -- Test (default initialization).
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
         O_2 : Not_Null_Subtype := Null_Init;      -- Test (initialization).
      begin
         Report.Failed ("O_2: Wrong value");
         Use_It (O_2);
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      O_3 : Not_Null_Subtype := Non_Null_Init;
   begin
      O_3 := Null_Init;                            -- Test (assignment).
      Report.Failed ("O_3: Wrong value");
      Use_It (O_3);
   exception
      when Constraint_Error =>
         null;
   end;


   --  Allocators -------------------------------------------------------
   begin
      declare
         A_1 : Int_Ptr_Ptr := new Not_Null_Subtype;   -- Test (default initialization).
      begin
         Report.Failed ("A_1: Wrong value");
         Use_It (A_1.all);
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   begin
      declare
         A_2 : Int_Ptr_Ptr := new Not_Null_Subtype'(Null_Init);   -- Test.
      begin
         Report.Failed ("A_2: Wrong value");
         Use_It (A_2.all);
      end;
   exception
      when Constraint_Error =>
         null;
   end;


   --  Array component and aggregate tests ------------------------------

   begin
      declare
         AO_1 : NN_Int_Ptr_Array;       -- Test (default initialization).
      begin
         Report.Failed ("AO_1: Wrong value");
         Use_It (AO_1(1));
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   begin
      declare
         AO_2 : NN_Int_Ptr_Array :=
            (Non_Null_Init, Null_Init, Non_Null_Init); -- Test (aggregate).
      begin
         Report.Failed ("AO_2: Wrong value");
         Use_It (AO_2(2));
      end;
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      AO_3 : NN_Int_Ptr_Array :=
         (Non_Null_Init, Non_Null_Init, Non_Null_Init);
   begin
      AO_3(1) := Null_Init;                -- Test (component assignment).
      Report.Failed ("AO_3: Wrong value");
      Use_It (AO_3(1));
   exception
      when Constraint_Error =>
         null;
   end;


   --  Record component and aggregate tests -----------------------------

   begin
      declare
         type Rec is record
            C : Not_Null_Subtype;
         end record;
         RO_1 : Rec;         -- Test (implicit record component default initialization).
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
            C : Not_Null_Subtype;
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
            C : Not_Null_Subtype := Null_Init;
         end record;
         RO_3 : Rec;         -- Test (explicit record component default initialization).
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
            C : Not_Null_Subtype := Null_Init;
         end record;
         RO_4 : Rec := (C => Non_Null_Init); -- Default expression not evaluated.
      begin
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
         C : Not_Null_Subtype := Non_Null_Init;
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
         type Rec (D : Not_Null_Subtype) is
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
         type Rec (D : Not_Null_Subtype := Null_Init) is
            record
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

   begin
      declare
         type Rec (D : Not_Null_Subtype := Non_Null_Init) is
            record
               Dummy : Integer := 0;
            end record;
         RO_8 : Rec;
      begin
         RO_8 := (D => Null_Init, Dummy => 0); -- Test (discriminant in aggregate)
         Report.Failed ("RO_8: Wrong value");
         Use_It (RO_8.D);
      end;
   exception
      when Constraint_Error =>
         null;
   end;


   --  Parameter tests --------------------------------------------------

   declare
      procedure Proc_1 (F : in Not_Null_Subtype) is
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
         procedure Proc_2 (F : in Not_Null_Subtype := Null_Init) is
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
      procedure Proc_3 (F : in out Not_Null_Subtype) is
      begin
         F := Null_Init;
      end Proc_3;
      Obj : Int_Ptr := Non_Null_Init;
   begin
      begin
         Proc_3 (Obj);
         Report.Failed ("Proc_3: Wrong value");
         Use_It (Obj);
      exception
         when Constraint_Error =>
            null;
      end;
   end;


   --  Result subtype tests ---------------------------------------------

   declare
      function Func_1 (F : Int_Ptr) return Not_Null_Subtype is
      begin
         return F;
      end Func_1;
      Obj : Int_Ptr;
   begin
      begin
         Obj := Func_1 (Null_Init);
         Report.Failed ("Func_1: Wrong value");
         Use_It (Obj);
      exception
         when Constraint_Error =>
            null;
      end;
      Obj := Func_1 (Non_Null_Init);
      if Obj /= Non_Null_Init then
         Report.Failed ("Func_1_OK: Wrong value");
      end if;
   end;

   declare
      function Func_2 (F : Int_Ptr) return Not_Null_Subtype is
      begin
         return Result : Not_Null_Subtype := Aux'Access do
            Result := F;
         end return;
      end Func_2;
      Obj : Int_Ptr;
   begin
      begin
         Obj := Func_2 (Null_Init);
         Report.Failed ("Func_2: Wrong value");
         Use_It (Obj);
      exception
         when Constraint_Error =>
            null;
      end;
      Obj := Func_2 (Non_Null_Init);
      if Obj /= Non_Null_Init then
         Report.Failed ("Func_2_OK: Wrong value");
      end if;
   end;


   --  Formal object tests ----------------------------------------------

   declare
      generic
         F : Not_Null_Subtype := Null_Init;
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
            package Inst_3 is new GPack (Non_Null_Init);
         begin
            Inst_3.User;
         end;
      exception
         when Constraint_Error =>
            Report.Failed ("Inst_3: Constraint_Error raised");
      end;
   end;

   Report.Result;
end C3A0020;

