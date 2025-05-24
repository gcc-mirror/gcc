-- C990001.A

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
--     Check that the prefix of 'Terminated and 'Callable can be:
--     * A task object (including a function that returns a task object);
--     * An object which has a named access to a task type;
--     * An object which has an anonymous named access to a task type;
--     * A function that returns an object of a named access to a task type;
--     * A function that returns an object of an anonymous access to a task
--       type.
--
--     Check that the 'Callable and 'Terminated attributes return the
--     correct values.

-- CHANGE HISTORY:
--    19 Mar 2007   RLB   Initial version, from remains of ACVC tests
--                        C38202A and C99004A. (The latter test has an
--                        untestable objective and had to be withdrawn;
--                        but it is the primary test of the correct operation
--                        of these attributes, and the other test is located
--                        in the wrong chapter.)
--    25 Jan 2008   RLB   Added function result cases from C34008A. Replaced
--                        absolute delay values with ones from Impdef.

with Report, Impdef;
procedure C990001 is

     type Enum is (A, B, C, D);

     TC_Error_Names : constant array (Enum) of String (1 .. 17) :=
                                        (A => "Before Activation",
                                         B => "During Activation",
                                         C => "During Execution ",
                                         D => "After Execution  " );

     function Check (S : String; Call, B1, Term, B2 : Boolean;
                     E : Enum) return Boolean is
     begin
          if Call /= B1 then
               Report.Failed ("Incorrect value for " & S & "'Callable " &
                    TC_Error_Names (E) & " of task");
          end IF;

          if Term /= B2 then
               Report.Failed ("Incorrect value for " & S & "'Terminated " &
                    TC_Error_Names (E) & " of task");
          end IF;

          return Report.Ident_Bool (True);
     end Check;


     procedure Consume (B : in Boolean) is
         -- Prevent optimization of the generator of B.
     begin
         if not B then
              Report.Failed ("Don't optimize B");
         end if;
     end Consume;


begin
     Report.Test ("C990001", "Check that 'Terminated and 'Callable return " &
                       "correct values, and their prefix can be an object " &
                       "of an access to a task type");

     -- Check attributes for a task object:
     declare

          task type TT is
               entry E;
          end TT;

          T2 : TT;

          A2 : Boolean := Check ("T2", T2'Callable, True,
                                  T2'Terminated, False, A);

          task Main_Task is
               entry E (Integer range 1 .. 2);
          end Main_Task;

          task body TT is
               B2 : Boolean := Check ("T2", T2'Callable, True,
                                       T2'Terminated, False, B);
          begin
               Consume (B2);
               Consume (Check ("T2", T2'Callable, True,
                               T2'Terminated, False, C));
               Main_Task.E (1);
               Main_Task.E (2);
          end TT;

          task body Main_Task is
          begin
               Accept E (1);
               Abort T2;
               delay Impdef.Clear_Ready_Queue;
               Consume (Check ("T2", T2'Callable, False,
                               T2'Terminated, True, D));
          end Main_Task;

     begin
          null;
     end;

     -- Check attributes for a function returning a task object
     -- (also check "current instance" using type name):
     declare

          task type MT;

          function Func return MT is
          begin
              return X : MT;
          end Func;


          task body MT is
               B3 : Boolean := Check ("MT", MT'Callable, True,
                                      MT'Terminated, False, B);
          begin
               Consume (B3);
               Consume (Check ("MT", MT'Callable, True,
                               MT'Terminated, False, C));
               delay Impdef.Clear_Ready_Queue;
          end MT;

     begin
          Consume (Check ("Func", Func'Callable, True,
                           Func'Terminated, False, C));
             -- This will create two tasks that we have to wait
             -- for the termination of here.
     end;


     -- Check attributes for accesses to a task object which is allocated:
     declare
          task type Tsk is
               entry Go_On;
          end Tsk;

          task Driver is
               entry Tsk_Done;
          end Driver;

          type P_Type is access Tsk;
          P : P_Type;

          Tsk_Created : Boolean := False;

          function F1 return P_Type is
          begin
               return P;
          end F1;

          task body Tsk is
               I : Integer range 0 .. 2;
          begin
               accept Go_On;
               I := Report.Ident_Int(5);      -- Constraint_Error raised.
               Report.Failed ("Constraint_Error not raised in task " &
                       "Tsk " & INTEGER'IMAGE(I));
          exception
               when Constraint_Error =>
                    Driver.Tsk_Done;
               when others =>
                    Report.Failed ("Wrong exception raised in task Tsk");
                    Driver.Tsk_Done;
          end Tsk;

          task body Driver is
               Counter : Integer := 1;
          begin
               P := new Tsk;               -- Activate P.all (F1.all).
               Consume (Check ("P", P'Callable, True,
                               P'Terminated, False, C));
               Consume (Check ("F1", F1'Callable, True,
                               F1'Terminated, False, C));

               F1.all.Go_On;
               accept Tsk_Done;
               while (not F1'Terminated and Counter <= 30) loop
                    delay Impdef.Switch_to_New_Task;
                    Counter := Counter + 1;
               end loop;

               if Counter > 30 then
                    Report.Failed ("Task Tsk not terminated in sufficient time");
               end if;

               Consume (Check ("P", P'Callable, False,
                               P'Terminated, True, D));
               Consume (Check ("F1", F1'Callable, False,
                               F1'Terminated, True, D));
          end Driver;

     begin
          null;
     end;

     -- Check attributes for accesses to a task object created with
     -- the access attribute:
     declare

          task type TT is
               entry E;
          end TT;

          package Pkg1 is
               T1 : aliased TT;
          end Pkg1;

          type Named_Access_to_TT is access all TT;

          function FN return Named_Access_to_TT is
          begin
               return Pkg1.T1'Access;
          end FN;

          function FA return access TT is
          begin
               return Pkg1.T1'Access;
          end FA;

	  Obj1 : Named_Access_to_TT := Pkg1.T1'Access;

	  Obj2 : access TT := Pkg1.T1'Access;

          package Pkg2 is
               A1 : Boolean := Check ("FA", FA'Callable, True,
                                      FA'Terminated, False, A);
               A2 : Boolean := Check ("FN.all", FN.all'Callable, True,
                                      FN.all'Terminated, False, A);
               A3 : Boolean := Check ("Obj1", Obj1'Callable, True,
                                      Obj1'Terminated, False, A);
               A4 : Boolean := Check ("Obj2", Obj2'Callable, True,
                                      Obj2'Terminated, False, A);
          end Pkg2;

          task Main_Task is
               entry E (Integer range 1 .. 2);
          end Main_Task;

          task body TT is
               B1 : Boolean := Check ("FA.all", FA.all'Callable, True,
                                      FA.all'Terminated, False, B);
               B2 : Boolean := Check ("FN", FN'Callable, True,
                                      FN'Terminated, False, B);
               B3 : Boolean := Check ("Obj1", Obj1'Callable, True,
                                      Obj1'Terminated, False, B);
               B4 : Boolean := Check ("Obj2", Obj2'Callable, True,
                                      Obj2'Terminated, False, B);
          begin
               Consume (B1);
               Consume (B2);
               Consume (B3);
               Consume (B4);
               Consume (Check ("FA", FA'Callable, True,
                                FA'Terminated, False, C));
               Consume (Check ("FN", FN'Callable, True,
                                FN'Terminated, False, C));
               Consume (Check ("Obj1.all", Obj1.all'Callable, True,
                                Obj1.all'Terminated, False, C));
               Consume (Check ("Obj2.all", Obj2.all'Callable, True,
                                Obj2.all'Terminated, False, C));
               Main_Task.E (1);
               Main_Task.E (2);
          end TT;

          package body Pkg1 is
          begin
               null;
          end;

          task body Main_Task is
          begin
               accept E (1);
               abort Pkg1.T1;
               delay Impdef.Clear_Ready_Queue;
               Consume (Check ("FA", FA'Callable, False,
                                FA'Terminated, True, D));
               Consume (Check ("FN", FN'Callable, False,
                                FN'Terminated, True, D));
               Consume (Check ("Obj1", Obj1'Callable, False,
                                Obj1'Terminated, True, D));
               Consume (Check ("Obj2", Obj2'Callable, False,
                                Obj2'Terminated, True, D));
          end Main_Task;

     begin
          Consume (Pkg2.A1);
          Consume (Pkg2.A2);
          Consume (Pkg2.A3);
          Consume (Pkg2.A4);
     end;

     Report.Result;
end C990001;
