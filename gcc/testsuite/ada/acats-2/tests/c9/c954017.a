-- C954017.A
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
--      Check that when an exception is raised in the rendezvous of a task 
--      that was called by a requeue the exception is propagated to the 
--      original caller and that the requeuing task is unaffected.  
--
-- TEST DESCRIPTION:
--      The Intermediate task requeues a call from the Original_Caller to the
--      Receiver.  While the Receiver is in the accept body for this
--      rendezvous a Constraint_Error exception is raised.  Check that the 
--      exception is propagated to the Original_Caller, that the Receiver's 
--      normal exception logic is employed and that the Intermediate task
--      is undisturbed.
--      There are several delay loops in this test any one of which could 
--      cause it to hang (and thus fail).
--      
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      25 Nov 95   SAIC    Fixed shared global variable problem for
--                          ACVC 2.0.1
--
--!

with Report;
with ImpDef;


procedure C954017 is

   TC_Original_Caller_Complete   : Boolean := false;
   TC_Intermediate_Complete      : Boolean := false;
   TC_Receiver_Complete          : Boolean := false;
   TC_Exception                  : Exception;


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
 
   TC_Exception_Process_Complete : Shared_Boolean (False);

   task Original_Caller is
      entry Start;
   end Original_Caller;

   task Intermediate is
      entry Input;
   end Intermediate;
   
   task Receiver is
      entry Input;
   end Receiver;

   
   task body Original_Caller is
   begin 
      accept Start;    -- wait for the trigger from Main

      Intermediate.Input;
      Report.Failed ("Exception not propagated to Original_Caller");

   exception
      when TC_Exception => 
               TC_Original_Caller_Complete := true;     -- Expected behavior
      when others        => 
               Report.Failed ("Unexpected Exception in Original_Caller task");
   end Original_Caller;
   
   
   task body Intermediate is
   begin
      accept Input do
         -- Within this accept call another task
         requeue Receiver.Input with abort;
      end Input;

      -- Wait for Main to ensure that the exception housekeeping is finished
      while not TC_Exception_Process_Complete.Value loop
         delay ImpDef.Minimum_Task_Switch;
      end loop;

      TC_Intermediate_Complete := true;

   exception
      when others => 
                  Report.Failed ("Unexpected exception in Intermediate task");
   end Intermediate;


   task body Receiver is
   --
   begin
      accept Input do
         null;  -- the user code for the rendezvous is stubbed out

         -- Test Control: Raise an exception in the destination task which
         -- should then be propagated
         raise TC_Exception;

      end Input;
   exception
      when TC_Exception =>
            TC_Receiver_Complete := true;  -- expected behavior
      when others        =>
            Report.Failed ("Unexpected Exception in Receiver Task");
   end Receiver;
   
   
begin

   Report.Test ("C954017", "Requeue: exception processing");

   Original_Caller.Start;   -- Start the test after the Report.Test
   
   -- Wait for the whole of the exception process to complete
   while not ( Original_Caller'terminated and Receiver'terminated ) loop
      delay ImpDef.Minimum_Task_Switch;
   end loop;

   -- Inform the Intermediate task that the process is complete to allow
   -- it to continue to completion itself
   TC_Exception_Process_Complete.Set_True;

   -- Wait for everything to settle before reporting the result
   while not ( Intermediate'terminated ) loop
      delay ImpDef.Minimum_Task_Switch;
   end loop;


   if not ( TC_Original_Caller_Complete and 
            TC_Intermediate_Complete    and
            TC_Receiver_Complete)       then
      Report.Failed ("Proper paths not traversed");
   end if;

   Report.Result;

end C954017;
