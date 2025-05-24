-- C951002.A
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
--      Check that an entry and a procedure within the same protected object
--      will not be executed simultaneously.
--
-- TEST DESCRIPTION:
--      Two tasks are used.  The first calls an entry who's barrier is set
--      and is thus queued.  The second calls a procedure in the same 
--      protected object.  This procedure clears the entry barrier of the 
--      first then executes a lengthy compute bound procedure.  This is 
--      intended to allow a multiprocessor, or a time-slicing implementation 
--      of a uniprocessor, to (erroneously) permit the first task to continue
--      while the second is still computing.  Flags in each process in the 
--      PO are checked to ensure that they do not run out of sequence or in 
--      parallel.  
--      In the second part of the test another entry and procedure are used
--      but in this case the procedure is started first.  A different task
--      calls the entry AFTER the procedure has started.  If the entry 
--      completes before the procedure the test fails.
--
--      This test will not be effective on a uniprocessor without time-slicing
--      It is designed to increase the chances of failure on a multiprocessor,
--      or a uniprocessor with time-slicing, if the entry and procedure in a 
--      Protected Object are not forced to acquire a single execution 
--      resource.  It is not guaranteed to fail.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with ImpDef;

procedure C951002 is
   
   -- These global error flags are used for failure conditions within
   -- the protected object.  We cannot call Report.Failed (thus Text_io)
   -- which would result in a bounded error.
   --
   TC_Error_01 : Boolean := false;
   TC_Error_02 : Boolean := false;
   TC_Error_03 : Boolean := false;
   TC_Error_04 : Boolean := false;
   TC_Error_05 : Boolean := false;
   TC_Error_06 : Boolean := false;

begin

   Report.Test ("C951002", "Check that a procedure and an entry body " &
                           "in a protected object will not run concurrently");

   declare -- encapsulate the test
      
      task Credit_Message is
         entry TC_Start;
      end Credit_Message;

      task Credit_Task is
         entry TC_Start;
      end Credit_Task;

      task Debit_Message is
         entry TC_Start;
      end Debit_Message;

      task Debit_Task is
         entry TC_Start;
      end Debit_Task;

      --====================================

      protected Hold is

         entry Wait_for_CR_Underload;
         procedure Clear_CR_Overload;
         entry Wait_for_DB_Underload;
         procedure Set_DB_Overload;
         procedure Clear_DB_Overload;
         --
         function TC_Message_is_Queued return Boolean;

      private
         Credit_Overloaded     : Boolean := true;  -- Test starts in overload
         Debit_Overloaded      : Boolean := false; 
         -- 
         TC_CR_Proc_Finished   : Boolean := false;
         TC_CR_Entry_Finished  : Boolean := false;
         TC_DB_Proc_Finished   : Boolean := false;
         TC_DB_Entry_Finished  : Boolean := false;
      end Hold;
      --====================
      protected body Hold is
   
         entry Wait_for_CR_Underload when not Credit_Overloaded is
         begin
            -- The barrier must only be re-evaluated at the end of the 
            -- of the execution of the procedure, also while the procedure
            -- is executing this entry body must not be executed
            if not TC_CR_Proc_Finished then
               TC_Error_01 := true;  -- Set error indicator
            end if;
            TC_CR_Entry_Finished := true;
         end Wait_for_CR_Underload ;
   
         -- This is the procedure which should NOT be able to run in 
         -- parallel with the entry body
         --
         procedure Clear_CR_Overload is
         begin

            -- The entry body must not be executed until this procedure
            -- is completed.  
            if TC_CR_Entry_Finished then
               TC_Error_02 := true;  -- Set error indicator
            end if;
            Credit_Overloaded := false;   -- clear the entry barrier

            -- Execute an implementation defined compute bound routine which 
            -- is designed to run long enough to allow a task switch on a
            -- time-sliced uniprocessor, or for a multiprocessor to pick up
            -- another task.
            -- 
            ImpDef.Exceed_Time_Slice;
            
            -- Again, the entry body must not be executed until the current 
            -- procedure is completed.  
            --
            if TC_CR_Entry_Finished then
               TC_Error_03 := true;  -- Set error indicator
            end if;
            TC_CR_Proc_Finished := true;

         end Clear_CR_Overload;
   
         --============
         -- The following subprogram and entry body are used in the second
         -- part of the test
         
         entry Wait_for_DB_Underload when not Debit_Overloaded is
         begin
            -- By the time the task that calls this entry is allowed access to
            -- the queue the barrier, which starts off as open, will be closed
            -- by the Set_DB_Overload procedure.  It is only reopened 
            -- at the end of the test
            if not TC_DB_Proc_Finished then
               TC_Error_04 := true;  -- Set error indicator
            end if;
            TC_DB_Entry_Finished := true;
         end Wait_for_DB_Underload ;
   
   
         procedure Set_DB_Overload is
         begin
            -- The task timing is such that this procedure should be started
            -- before the entry is called.  Thus the entry should be blocked
            -- until the end of this procedure which then sets the barrier
            --
            if TC_DB_Entry_Finished then
               TC_Error_05 := true;  -- Set error indicator
            end if;

            -- Execute an implementation defined compute bound routine which 
            -- is designed to run long enough to allow a task switch on a
            -- time-sliced uniprocessor, or for a multiprocessor to pick up
            -- another task
            -- 
            ImpDef.Exceed_Time_Slice;
            
            Debit_Overloaded := true;   -- set the entry barrier

            if TC_DB_Entry_Finished then
               TC_Error_06 := true;  -- Set error indicator
            end if;
            TC_DB_Proc_Finished := true;

         end Set_DB_Overload;
   
         procedure Clear_DB_Overload is
         begin
            Debit_Overloaded := false;  -- open the entry barrier
         end Clear_DB_Overload;

         function TC_Message_is_Queued return Boolean is
         begin
   
            -- returns true when one message arrives on the queue
            return (Wait_for_CR_Underload'Count = 1);   
                                                    
         end TC_Message_is_Queued ;

      end Hold;

      --====================================

      task body Credit_Message is
      begin
         accept TC_Start;
         --::  some application processing.  Part of the process finds that
         --    the Overload threshold has been exceeded for the Credit
         --    application.  This message task queues itself on a queue
         --    waiting till the overload in no longer in effect 
         Hold.Wait_for_CR_Underload;
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Credit_Message Task");
      end Credit_Message;

      task body Credit_Task is
      begin
         accept TC_Start;
         --  Application code here (not shown) determines that the
         --  underload threshold has been reached
         Hold.Clear_CR_Overload;
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Credit_Task");
      end Credit_Task;

      --============== 

      -- The following two tasks are used in the second part of the test

      task body Debit_Message is
      begin
         accept TC_Start;
         --::  some application processing.  Part of the process finds that
         --    the Overload threshold has been exceeded for the Debit 
         --    application.  This message task queues itself on a queue
         --    waiting till the overload is no longer in effect 
         --
         Hold.Wait_for_DB_Underload;
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Debit_Message Task");
      end Debit_Message;

      task body Debit_Task is
      begin
         accept TC_Start;
         --  Application code here (not shown) determines that the
         --  underload threshold has been reached
         Hold.Set_DB_Overload;
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Debit_Task");
      end Debit_Task;
   
   begin -- declare

      Credit_Message.TC_Start;
      
      -- Wait until the message is queued on the entry before starting
      -- the Credit_Task
      while not Hold.TC_Message_is_Queued loop
         delay ImpDef.Minimum_Task_Switch;   
      end loop;
      --
      Credit_Task.TC_Start;

      -- Ensure the first part of the test is complete before continuing
      while not (Credit_Message'terminated and Credit_Task'terminated) loop
         delay ImpDef.Minimum_Task_Switch;   
      end loop;

      --======================================================
      -- Second part of the test


      Debit_Task.TC_Start;
      
      -- Delay long enough to allow a task switch to the Debit_Task and
      -- for it to reach the accept statement and call Hold.Set_DB_Overload
      -- before starting Debit_Message
      --
      delay ImpDef.Switch_To_New_Task;

      Debit_Message.TC_Start;

      while not Debit_Task'terminated loop
         delay ImpDef.Minimum_Task_Switch;   
      end loop;    
  
      Hold.Clear_DB_Overload;  -- Allow completion 
   
   end; -- declare (encapsulation)

   if TC_Error_01 then
      Report.Failed ("Wait_for_CR_Underload executed out of sequence");
   end if;
   if TC_Error_02 then
      Report.Failed ("Credit: Entry executed before procedure");
   end if;
   if TC_Error_03 then
      Report.Failed ("Credit: Entry executed in parallel");
   end if;
   if TC_Error_04 then
      Report.Failed ("Wait_for_DB_Underload executed out of sequence");
   end if;
   if TC_Error_05 then
      Report.Failed ("Debit: Entry executed before procedure");
   end if;
   if TC_Error_06 then
      Report.Failed ("Debit: Entry executed in parallel");
   end if;
   
   Report.Result;

end C951002;
