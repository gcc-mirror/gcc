-- C954018.A
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
--      Check that if a task is aborted while a requeued call is queued 
--      on one of its entries the original caller receives Tasking_Error
--      and the requeuing task is unaffected.
--         This test uses: Requeue to an entry in a different task
--                         Parameterless call
--                         Requeue with abort
--
-- TEST DESCRIPTION:
--      The Intermediate task requeues a call from the Original_Caller to the
--      Receiver on an entry with a guard that is always false.  While the 
--      Original_Caller is still queued the Receiver is aborted.
--      Check that Tasking_Error is raised in the Original_Caller, that the 
--      Receiver does, indeed, get aborted and the Intermediate task 
--      is undisturbed.
--      There are several delay loops in this test any one of which could 
--      cause it to hang and thus indicate failure.  
--      
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


with Report;
with ImpDef;


procedure C954018 is


   -- Protected object to control the shared test variables
   --
   protected TC_State is
      function  On_Entry_Queue return Boolean;
      procedure Set_On_Entry_Queue; 
      function  Original_Caller_Complete return Boolean;
      procedure Set_Original_Caller_Complete;
      function  Intermediate_Complete return Boolean;
      procedure Set_Intermediate_Complete;
   private
      On_Entry_Queue_Flag           : Boolean := false;
      Original_Caller_Complete_Flag : Boolean := false;
      Intermediate_Complete_Flag    : Boolean := false;
   end TC_State;
   --
   --
   protected body TC_State is
      function On_Entry_Queue return Boolean is
      begin 
         return On_Entry_Queue_Flag;
      end On_Entry_Queue;
      
      procedure Set_On_Entry_Queue is
      begin
         On_Entry_Queue_Flag := true; 
      end Set_On_Entry_Queue; 

      function Original_Caller_Complete return Boolean is
      begin
         return Original_Caller_Complete_Flag;
      end Original_Caller_Complete;

      procedure Set_Original_Caller_Complete is
      begin
         Original_Caller_Complete_Flag := true;
      end Set_Original_Caller_Complete;

      function Intermediate_Complete return Boolean is
      begin
         return Intermediate_Complete_Flag;
      end Intermediate_Complete;

      procedure Set_Intermediate_Complete is
      begin
         Intermediate_Complete_Flag := true;
      end Set_Intermediate_Complete;

   end TC_State;

   --================================

   task Original_Caller is
      entry Start;
   end Original_Caller;

   task Intermediate is
      entry Input;
      entry TC_Abort_Process_Complete;
   end Intermediate;
   
   task Receiver is
      entry Input;
   end Receiver;

   
   task body Original_Caller is
   begin 
      accept Start;    -- wait for the trigger from Main

      Intermediate.Input;
      Report.Failed ("Tasking_Error not raised in Original_Caller task");

   exception
      when tasking_error => 
               TC_State.Set_Original_Caller_Complete; -- expected behavior
      when others        => 
               Report.Failed ("Unexpected Exception in Original_Caller task");
   end Original_Caller;
   
   
   task body Intermediate is
   begin
      accept Input do
         -- Within this accept call another task
         TC_State.Set_On_Entry_Queue;
         requeue Receiver.Input with abort;  
         Report.Failed ("Requeue did not complete the Accept");
      end Input;

      -- Wait for Main to ensure that the abort housekeeping is finished
      accept TC_Abort_Process_Complete;

      TC_State.Set_Intermediate_Complete;

   exception
      when others => 
              Report.Failed ("Unexpected exception in Intermediate task");
   end Intermediate;


   task body Receiver is
   begin
      loop
         select
            -- A call to Input will be placed on the queue and never serviced
            when Report.Equal (1,2) =>     -- Always false
            accept Input do
               Report.Failed ("Receiver in Accept");  
            end Input;
         or 
            delay ImpDef.Minimum_Task_Switch;
         end select;
      end loop;
   exception
      when others =>
            Report.Failed ("Unexpected Exception in Receiver Task");
                                                          
   end Receiver;
   
   
begin

   Report.Test ("C954018", "Requeue: abort the called task" &
                           " while Caller is still queued");

   Original_Caller.Start;


   -- This is the main part of the test  

   -- Wait for the requeue
   while not TC_State.On_Entry_Queue loop
      delay ImpDef.Minimum_Task_Switch;
   end loop;

   -- Delay long enough to ensure that the requeue has "arrived" on 
   -- the entry queue.  Note: TC_State.Set_On_Entry_Queue is called the
   -- statement before the requeue
   --
   delay ImpDef.Switch_To_New_Task;

   -- At this point the Receiver is guaranteed to have the requeue on
   -- the entry queue
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
   

   if not ( TC_State.Original_Caller_Complete and 
            TC_State.Intermediate_Complete )       then
      Report.Failed ("Proper paths not traversed");
   end if;

   Report.Result;

end C954018;
