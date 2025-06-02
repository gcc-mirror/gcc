-- C954010.A
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
--      Check that a requeue within an accept statement does not block.
--      This test uses: Requeue to an entry in a different task
--                      Parameterless call
--                      Requeue with abort
--
-- TEST DESCRIPTION:
--      In the Distributor task, requeue two successive calls on the entries
--      of two separate target tasks.  Verify that the target tasks are
--      run in parallel proving that the first requeue does not block
--      while the first target rendezvous takes place.                 
--
--      This series of tests uses a simulation of a transaction driven 
--      processing system.  Line Drivers accept input from an external source
--      and build them into transaction records.  These records are then 
--      encapsulated in message tasks which remain extant for the life of the
--      transaction in the system. The message tasks put themselves on the 
--      input queue of a Distributor which, from information in the 
--      transaction and/or system load conditions forwards them to other 
--      operating tasks. These in turn might forward the transactions to yet 
--      other tasks for further action.  The  routing is, in real life, 
--      dynamic and unpredictable at the time of message generation. All 
--      rerouting in this  model is done by means of requeues.
--
--      This test is directed towards the BLOCKING of the REQUEUE only
--      If the original caller does not block, the outcome of the test will
--      not be affected.  If the original caller does not continue after
--      the return, the test will not pass.  
--      If the requeue gets placed on the wrong entry a failing test could
--      pass (eg. if the first message is delivered to the second
--      computation task and the second message to the first) - a check for
--      this condition is made in other tests
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with ImpDef;

procedure C954010 is

   -- Mechanism to count the number of Message tasks completed
   protected TC_Tasks_Completed is
      procedure Increment;
      function  Count return integer;
   private
      Number_Complete : integer := 0;
   end TC_Tasks_Completed;
   --
   TC_Expected_To_Complete   : constant integer := 2;


   task type Message_Task;
   type acc_Message_Task is access Message_Task;

   task Line_Driver is
      entry Start;
   end Line_Driver;

   task Distributor is 
      entry Input;
   end Distributor;

   task Credit_Computation is
      entry Input;
   end Credit_Computation;

   task Debit_Computation is
      entry Input;
      entry TC_Artificial_Rendezvous_1;      -- test purposes only
      entry TC_Artificial_Rendezvous_2;      -- test purposes only
   end Debit_Computation;

   
   -- Mechanism to count the number of Message tasks completed
   protected body TC_Tasks_Completed is
      procedure Increment is
      begin
         Number_Complete := Number_Complete + 1;
      end Increment;

      function Count return integer is
      begin
         return Number_Complete;
      end Count;
   end TC_Tasks_Completed;



   -- Assemble messages received from an external source
   --   Creates a message task for each and sends this to a Distributor 
   --   for appropriate disposal around the network of tasks
   --      Such a task would normally be designed to loop continuously
   --      creating the messages as input is received.  Simulate this 
   --      but limit it to two dummy messages for this test and allow it
   --      to terminate at that point
   --
   task body Line_Driver is

   begin

      accept Start;      -- Wait for trigger from main

      for i in 1..2 loop
         declare
            -- create a new message task 
            N : acc_Message_Task := new Message_Task; 
         begin
            -- preparation code
            null; -- stub

         end;   -- declare
      end loop;

   exception
      when others => 
         Report.Failed ("Unexpected exception in Line_Driver");
   end Line_Driver;


   task body Message_Task is
   begin
      -- Queue up on Distributor's Input queue
      Distributor.Input;
      
      -- After the required computations have been performed
      -- return the message appropriately (probably to an output
      -- line driver
      null;            -- stub

      -- Increment to show completion of this task
      TC_Tasks_Completed.Increment;

   exception
      when others => 
         Report.Failed ("Unexpected exception in Message_Task");

   end Message_Task;


   -- Dispose each input message to the appropriate computation tasks
   --    Normally this would be according to some parameters in the entry
   --    but this simple test is using parameterless entries.  
   --
   task body Distributor is
     Last_was_for_Credit_Computation : Boolean := false;   -- switch
   begin
      loop
         select
            accept Input do
               -- Determine to which task the message should be 
               -- distributed
               --      For this test arbitrarily send the first to 
               --      Credit_Computation and the second to Debit_Computation
               if Last_was_for_Credit_Computation then
                  requeue Debit_Computation.Input with abort;
               else 
                  Last_was_for_Credit_Computation := true;
                  requeue Credit_Computation.Input with abort;
               end if;
            end Input;
         or
            terminate;
         end select;
      end loop;

   exception
      when others => 
         Report.Failed ("Unexpected exception in Distributor");
   end Distributor;
                                          
                                                
   -- Computation task.  After the computation is performed the rendezvous
   -- in the original message task is completed.                              
   task body Credit_Computation is
   begin
      loop
         select 
            accept Input do
               -- Perform the computations required for this message
               --
               null;     -- stub

               --   For the test:
               --   Artificially rendezvous with Debit_Computation.
               --   If the first requeue in Distributor has blocked 
               --   waiting for the current rendezvous to complete then the
               --   second message will not be sent to Debit_Computation 
               --   which will still be waiting on its Input accept.  
               --   This task will HANG
               --
               Debit_Computation.TC_Artificial_Rendezvous_1;
               --
            end Input;
         or
            terminate;
         end select;
      end loop;
   exception
      when others => 
         Report.Failed ("Unexpected exception in Credit_Computation");
   end Credit_Computation;

                                                
   -- Computation task.  After the computation is performed the rendezvous
   -- in the original message task is completed.                              
   task body Debit_Computation is
      Message_Count      : integer := 0;
      TC_AR1_is_complete : Boolean := false;
   begin
      loop
         select
            accept Input do
               -- Perform the computations required for this message
               null;    -- stub
            end Input;
            Message_Count := Message_Count + 1;
         or
            -- Guard until the rendezvous with the message for this task
            -- has completed
            when Message_Count > 0 =>
            accept TC_Artificial_Rendezvous_1;    -- see comments in
                                                  -- Credit_Computation above
            TC_AR1_is_complete := true; 
         or
            -- Completion rendezvous with the main procedure
            when TC_AR1_is_complete =>
            accept TC_Artificial_Rendezvous_2;
         or
            terminate;
         end select;
      end loop;
   exception
      when others => 
         Report.Failed ("Unexpected exception in Debit_Computation");


   end Debit_Computation;


begin -- c954010
   Report.Test ("C954010", "Requeue in an accept body does not block");

   Line_Driver.Start;

   -- Ensure that both messages were delivered to the computation tasks
   -- This shows that both requeues were effective.
   -- 
   Debit_Computation.TC_Artificial_Rendezvous_2;

   -- Ensure that the message tasks completed
   while (TC_Tasks_Completed.Count < TC_Expected_To_Complete) loop
      delay ImpDef.Minimum_Task_Switch;   
   end loop;

   Report.Result;

end C954010;
