-- C413006.A
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
--     Check that if X denotes a task object, an entry call of the form
--     X.E is permitted.

-- TEST DESCRIPTION:
--     We test an X that is a stand-alone object, an array component, a
--     record component, a parameter, and a function call.
--     Access-to-task object cases are tested in C41306B and C41306C.

-- CHANGE HISTORY:
--     25 Jan 2008 RLB Created test based on C41306A.

with Report;
procedure C413006 is

   Global : Natural := 0;

   task type Tsk (Val : Natural) is
       entry  E;
   end Tsk;

   task body Tsk is
   begin
      accept E do
         Global := Tsk.Val;
      end E ;
   end Tsk;

begin

    Report.Test ("C413006" , "Check that if X denotes a task object, an " &
                             "entry call of the form X.E is permitted");

    Case_A : declare -- Stand-alone object
       X : Tsk (10);
    begin
       X.E;    -- Global set to 10.
       if Global /= Report.Ident_Int(10) then
          Report.Failed ("Wrong value for global variable - A1");
       end if;
    end Case_A;

    Case_B : declare -- Array component
       X : array (1..2) of Tsk(4);
    begin
       X(1).E;    -- Global set to 4.
       if Global /= Report.Ident_Int(4) then
          Report.Failed ("Wrong value for global variable - B1");
       end if;
       Global := 0;
       X(Report.Ident_Int(2)).E;    -- Global set to 4.
       if Global /= Report.Ident_Int(4) then
          Report.Failed ("Wrong value for global variable - B2");
       end if;
    end Case_B;

    Case_C : declare -- Record component
       type Rec is record
          X : Tsk(80);
       end record;
       R : Rec;
    begin
       R.X.E;    -- Global set to 80.
       if Global /= Report.Ident_Int(80) then
          Report.Failed ("Wrong value for global variable - C1");
       end if;
    end Case_C;

    Case_D : declare -- Parameter
       procedure Do_It (X : in out Tsk) is
       begin
          X.E;  -- Global set.
       end Do_It;
       Obj : Tsk(23);
    begin
       Do_It (Obj);
       if Global /= Report.Ident_Int(23) then
          Report.Failed ("Wrong value for global variable - D1");
       end if;
    end Case_D;

    Case_E : declare -- Function
       function F1 return Tsk is
       begin
          return T1 : Tsk(17);
       end F1;

       function F2 (Val : in Natural) return Tsk is
       begin
          return T2 : Tsk(Val);
       end F2;
    begin
       F1.E;            -- Global set to 17.
       if Global /= Report.Ident_Int(17) then
          Report.Failed ("Wrong value for global variable - E1");
       end if;

       F2(16).E;        -- Global set to 16.
       if Global /= Report.Ident_Int(16) then
          Report.Failed ("Wrong value for global variable - E2");
       end if;

       F2(Report.Ident_Int(44)/2).E;   -- Global set to 22.
       if Global /= Report.Ident_Int(22) then
          Report.Failed ("Wrong value for global variable - E3");
       end if;
    end Case_E;

    Report.Result;

end C413006;
