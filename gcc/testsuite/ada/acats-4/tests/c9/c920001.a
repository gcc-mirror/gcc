-- C920001.A
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
--      Check activation (and non-activation) of tasks declared as
--      (part of) return objects in extended return statements.
--
-- TEST DESCRIPTION:
--      This test checks activation of tasks by setting a flag in the
--      elaboration of the declarative part of the task.
--
--      This test is intended to check cases noted by Binding Interpretation
--      AI05-0045-1. The test is not really usage oriented - by nature.
--
-- CHANGE HISTORY:
--      10 Jan 12   JPR     ACVC 3.0.
--      14 Apr 14   RLB     Renamed to add to ACATS 4.0; corrected various
--                          comments. Used Impdef constants rather than hard
--                          coded delays. Removed priorities (can't use in
--                          core tests).
--
--!

with System;
with Report;
with Impdef;
procedure C920001 is
   protected Flag is
      procedure Set (To : Boolean);
      function Is_True return  Boolean;
   private
      State : Boolean := False;
   end Flag;

   protected body Flag is
      procedure Set (To : Boolean) is
      begin
	 State := To;
      end Set;

      function Is_True return  Boolean is
      begin
	 return State;
      end Is_True;
   end Flag;

   task type TT;

   task body TT is
      function Trace_Elaboration return Integer is
      begin
	 Flag.Set (True);
	 return 0;
      end Trace_Elaboration;

      I : Integer := Trace_Elaboration;
   begin
      null;
   end TT;

   type Rec (D : Boolean) is
      record
	 case D is
	    when True =>
	       T : TT;
	    when False =>
	       null;
	 end case;
      end record;

begin
   Report.Test ("C920001",
		"Check activation (and non activation) of tasks" &
		  " declared as (part of) return objects in extended" &
		  " return statements");

   -- Normal case
   -- Check that task stand-alone object is activated only after function return
   Flag.Set (False);
   declare
      function F return TT is
      begin
	 return T : TT do
	   delay ImpDef.Switch_To_New_Task;
	   if Flag.Is_True then
	      Report.Failed ("Task activated before return of function");
	   end if;
	 end return;
      end F;

      T : TT := F;
   begin
      delay ImpDef.Switch_To_New_Task;
      if not Flag.Is_True then
	 Report.Failed ("Task not activated after function return");
      end if;
   end;

   -- Normal case
   -- Check that task subcomponent is activated only after function return
   Flag.Set (False);
   declare
      function F return Rec is
      begin
	 return R : Rec (True) do
	   delay ImpDef.Switch_To_New_Task;
	   if Flag.Is_True then
	      Report.Failed ("Task activated before return of function (1)");
	   end if;
	 end return;
      end F;

      R : Rec := F;
   begin
      delay ImpDef.Switch_To_New_Task;
      if not Flag.Is_True then
	 Report.Failed ("Task not activated after function return");
      end if;
   end;

   -- Return left by goto
   -- Check that task is not activated
   Flag.Set (False);
   declare
      function F return Rec is
      begin
	 return R : Rec (True) do
	   delay ImpDef.Switch_To_New_Task;
	   if Flag.Is_True then
	      Report.Failed ("Task activated before return of function (2)");
	   end if;
	   goto Outside;
	 end return;

	 <<Outside>> return R : Rec (False);
      end F;

      R : Rec := F;
   begin
      if R.D then
	 Report.Failed ("Wrong object returned");
      end if;

      delay ImpDef.Switch_To_New_Task;
      if Flag.Is_True then
	 Report.Failed ("Ghost task activated after function return");
      end if;
   end;

   -- Return left by exception
   -- Check that task is not activated
   Flag.Set (False);
   declare
      function F return Rec is
      begin
	 return R : Rec (True) do
	   delay ImpDef.Switch_To_New_Task;
	   if Flag.Is_True then
	      Report.Failed ("Task activated before return of function (2)");
	   end if;
	   raise Constraint_Error;
	 end return;
	 Report.Failed ("Constraint_Error not raised");
      exception
	 when Constraint_Error =>
	    return R : Rec (False) do
	      return;
	    end return;
	 when others =>
	    Report.Failed ("Wrong exception raised");
	    return R: Rec (False);
      end F;

      R : Rec := F;
   begin
      if R.D then
	 Report.Failed ("Wrong object returned");
      end if;

      delay ImpDef.Switch_To_New_Task;
      if Flag.Is_True then
	 Report.Failed ("Ghost task activated after function return");
      end if;
   end;

   Report.Result;
exception
   when others =>
      Report.Failed ("exception in main");
end C920001;
