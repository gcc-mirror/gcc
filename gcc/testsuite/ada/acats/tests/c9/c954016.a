-- C954016.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that when a task that is called by a requeue is aborted, the 
--      original caller receives Tasking_Error and the requeuing task is 
--      unaffected.
--
-- TEST DESCRIPTION:
--      The Intermediate task requeues a call from the Original_Caller to the
--      Receiver.  While the Receiver is in the accept body for this
--      rendezvous the Main aborts it.  Check that Tasking_Error is raised in
--      the Original_Caller, that the Receiver does, indeed, get aborted and
--      the Intermediate task is undisturbed.
--      There are several delay loops in this test any one of which could 
--      cause it to hang which would constitute failure.  
--      
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      25 Nov 95   SAIC    Replaced shared global variable with protected
--                          object for ACVC 2.0.1
--
--!

with Report;
with ImpDef;

procedure C954016 is

   TC_Original_Caller_Complete  : Boolean := false;
   TC_Intermediate_Complete     : Boolean := false;


   protected type Shared_Boolean (Initial_Value : Boolean := False) is
      procedure Set_True;
      procedure Set_False;
      function  Value return Boolean;
   private
      Current_Value : Boolean := Initial_Value;
   end Shared_Boolean;

   protected body Shared_Boolean is
      procedure Set_True is
      begin
         Current_Value := True;
      end Set_True;

      procedure Set_False is
      begin
         Current_Value := False;
      end Set_False;

      function Value return Boolean is
      begin
         return Current_Value;
      end Value;
   end Shared_Boolean;
 
   TC_Receiver_in_Accept        : Shared_Boolean (False);


   task Original_Caller is
      entry Start;
   end Original_Caller;

   task Intermediate is
      entry Input;
      entry TC_Abort_Process_Complete;
   end Intermediate;
   
   task Receiver is
      entry Input;
      entry TC_Never_Called;
   end Receiver;

   
   task body Original_Caller is
   begin 
      accept Start;    -- wait for the trigger from Main

      Intermediate.Input;
      Report.Failed ("Tasking_Error not raised in Original_Caller task");

   exception
      when tasking_error => 
               TC_Original_Caller_Complete := true;     -- expected behavior
      when others        => 
               Report.Failed ("Unexpected Exception in Original_Caller task");
   end Original_Caller;
   
   
   task body Intermediate is
   begin
      accept Input do
         -- Within this accept call another task
         requeue Receiver.Input with abort;
      end Input;

      -- Wait for Main to ensure that the abort housekeeping is finished
      accept TC_Abort_Process_Complete;

      TC_Intermediate_Complete := true;

   exception
      when others => 
                Report.Failed ("Unexpected exception in Intermediate task");
   end Intermediate;


   task body Receiver is
   begin
      accept Input do
         TC_Receiver_in_Accept.Set_True;
         -- Hang within the accept body to allow Main to abort this task
         accept TC_Never_Called;  
      end Input;
   exception
      when others  =>
            Report.Failed ("Unexpected Exception in Receiver Task");
                                                          
   end Receiver;
   
   
begin
   Report.Test ("C954016", "Requeue: abort the called task");

   Original_Caller.Start;

   -- Wait till the rendezvous with Receiver is started
   while not TC_Receiver_in_Accept.Value loop
      delay ImpDef.Minimum_Task_Switch;
   end loop;

   -- At this point the Receiver is guaranteed to be in its accept
   --
   abort Receiver;

   -- Wait for the whole of the abort process to complete
   while not ( Original_Caller'terminated and Receiver'terminated ) loop
      delay ImpDef.Minimum_Task_Switch;
   end loop;

   -- Inform the Intermediate task that the process is complete to allow
   -- it to continue to completion itself
   Intermediate.TC_Abort_Process_Complete;

   -- Wait for everything to settle before reporting the result
   while not ( Intermediate'terminated ) loop
      delay ImpDef.Minimum_Task_Switch;
   end loop;


   if not ( TC_Original_Caller_Complete and TC_Intermediate_Complete ) then
      Report.Failed ("Proper paths not traversed");
   end if;

   Report.Result;

end C954016;
