-- C954014.A
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
--     Check that a requeue is not canceled and that the requeueing 
--     task is unaffected when a calling task is aborted. Check that the
--     abort is deferred until the entry call is complete.
--     Specifically, check requeue to an entry in a different task,
--     requeue where the entry call has parameters, and requeue
--     without the abort option.
--
-- TEST DESCRIPTION
--     In the Driver create a task that places a call on the
--     Distributor.  In the Distributor requeue this call on the Credit task.
--     Abort the calling task when it is known to be in rendezvous with the
--     Credit task. (We arrange this by using artificial synchronization
--     points in the Driver and the accept body of the Credit task) Ensure
--     that the abort is deferred (the task is not terminated) until the
--     accept body completes.   Afterwards, send one extra message through
--     the Distributor to check that the requeueing task has not been
--     disrupted. 
--
--     This series of tests uses a simulation of a transaction driven
--     processing system.  Line Drivers accept input from an external source
--     and build them into transaction records.  These records are then
--     encapsulated in message tasks which remain extant for the life of the
--     transaction in the system.  The message tasks put themselves on the
--     input queue of a Distributor which, from information in the
--     transaction and/or system load conditions forwards them to other
--     operating tasks. These in turn might forward the transactions to yet
--     other tasks for further action.  The routing is, in real life, dynamic
--     and unpredictable at the time of message generation.  All rerouting in
--     this  model is done by means of requeues.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      25 Nov 95   SAIC    Replaced global variables with protected objects
--                          for ACVC 2.0.1.
--
--!

with Report;
with ImpDef;
         
procedure C954014 is
 
   -- Arbitrary test values
   Credit_Return : constant := 1;
   Debit_Return  : constant := 2;


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
 

   TC_Debit_Message_Complete  : Shared_Boolean (False);
   
   -- Synchronization flags for handshaking between the Line_Driver
   -- and the Accept body in the Credit Task
   TC_Handshake_A : Shared_Boolean (False);
   TC_Handshake_B : Shared_Boolean (False);
   TC_Handshake_C : Shared_Boolean (False);
   TC_Handshake_D : Shared_Boolean (False);
   TC_Handshake_E : Shared_Boolean (False);
   TC_Handshake_F : Shared_Boolean (False);


   type Transaction_Code is (Credit, Debit);

   type Transaction_Record;
   type acc_Transaction_Record is access Transaction_Record;
   type Transaction_Record is 
      record
         ID               : integer := 0;
         Code             : Transaction_Code := Debit;
         Account_Number   : integer := 0;
         Stock_Number     : integer := 0;
         Quantity         : integer := 0;
         Return_Value     : integer := 0;
         TC_Message_Count : integer := 0; 
         TC_Thru_Distrib  : Boolean;
      end record;

   
   task type Message_Task is 
      entry Accept_Transaction (In_Transaction : acc_Transaction_Record);
   end Message_Task;
   type acc_Message_Task is access Message_Task;

   task Line_Driver is
      entry start;
   end Line_Driver;   

   task Distributor is 
      entry Input(Transaction : acc_Transaction_Record);
   end Distributor;

   task Credit_Computation is
      entry Input(Transaction : acc_Transaction_Record);
   end Credit_Computation;

   task Debit_Computation is
      entry Input(Transaction : acc_Transaction_Record);
   end Debit_Computation;


   -- Assemble messages received from an external source
   --   Creates a message task for each. The message tasks remain extant
   --   for the life of the messages in the system.
   --      TC: The Line Driver task would normally be designed to loop
   --      continuously creating the messages as input is received.  Simulate
   --      this  but limit it to two dummy messages for this test and use
   --      special artificial handshaking checks with the Credit accept body
   --      to control the test. Allow it to terminate at the end
   --
   task body Line_Driver is
      Current_ID : integer := 1;
      TC_First_message_sent: Boolean := false;

      procedure Build_Credit_Record 
                              ( Next_Transaction : acc_Transaction_Record ) is
         Dummy_Account : constant integer := 100;
      begin
            Next_Transaction.ID := Current_ID;
            Next_Transaction.Code := Credit; 

            Next_Transaction.Account_Number := Dummy_Account;
            Current_ID := Current_ID + 1;
      end Build_Credit_Record;     


      procedure Build_Debit_Record 
                              ( Next_Transaction : acc_Transaction_Record ) is
         Dummy_Account : constant integer := 200;
      begin
            Next_Transaction.ID := Current_ID;
            Next_Transaction.Code := Debit; 

            Next_Transaction.Account_Number := Dummy_Account;
            Current_ID := Current_ID + 1;
      end Build_Debit_Record;     

   begin
   
      accept Start;       -- Wait for trigger from main

      for i in 1..2 loop  -- TC: arbitrarily limit to one credit message
                          --     and one debit, then complete
         declare 
            -- Create a task for the next message
            Next_Message_Task : acc_Message_Task := new Message_Task;
            -- Create a record for it
            Next_Transaction : acc_Transaction_Record := 
                                                   new Transaction_Record;
         begin
            if not TC_First_Message_Sent then
               -- send out the first message which will be aborted
               Build_Credit_Record ( Next_Transaction );            
               Next_Message_Task.Accept_Transaction ( Next_Transaction );  
               TC_First_Message_Sent := true;

               -- Wait for Credit task to get into the accept body
               --   The call from the Message Task has been requeued by
               --   the distributor
               while not TC_Handshake_A.Value loop
                  delay ImpDef.Minimum_Task_Switch;
               end loop;

               -- Abort the calling task; the Credit task is guaranteed to 
               -- be in the accept body
               abort Next_Message_Task.all;     -- We are still in this declare
                                                -- block
              
               -- Inform the Credit task that the abort has been initiated
               TC_Handshake_B.Set_True;
               
               -- Now wait for the "acknowledgment" from the Credit task
               -- this ensures a complete task switch (at least) 
               while not TC_Handshake_C.Value loop
                  delay ImpDef.Minimum_Task_Switch;
               end loop;

               -- The aborted task must not terminate till the accept body
               -- has completed
               if Next_Message_Task'terminated then
                  Report.Failed ("The abort was not deferred");
               end if;

               -- Inform the Credit task that the termination has been checked
               TC_Handshake_D.Set_True;
               
               -- Now wait for the completion of the accept body in the 
               -- Credit task
               while not TC_Handshake_E.Value loop
                  delay ImpDef.Minimum_Task_Switch;
               end loop;
               
               while not ( Next_Message_Task'terminated ) loop
                  delay ImpDef.Minimum_Task_Switch;
               end loop;

               -- Indicate to the Main program that this section is complete
               TC_Handshake_F.Set_True;

            else
               -- The main part of the test is complete. Send one Debit message
               -- as further exercise of the Distributor to ensure it has not
               -- been affected by the abort of the requeue;
               Build_Debit_Record ( Next_Transaction );            
               Next_Message_Task.Accept_Transaction ( Next_Transaction );  
            end if;
         end;   -- declare
      end loop;

   exception
      when others => 
         Report.Failed ("Unexpected exception in Line_Driver");
   end Line_Driver;

   

   task body Message_Task is

      TC_Original_Transaction_Code : Transaction_Code;  
      This_Transaction : acc_Transaction_Record := new Transaction_Record;

   begin
      
      accept Accept_Transaction (In_Transaction : acc_Transaction_Record) do
         This_Transaction.all := In_Transaction.all;
      end Accept_Transaction;      

      -- Note the original code to ensure correct return
      TC_Original_Transaction_Code := This_Transaction.Code;
                                                                  
      -- Queue up on Distributor's Input queue
      Distributor.Input ( This_Transaction );
      -- This task will now wait for the requeued rendezvous 
      -- to complete before proceeding
                                             
      -- After the required computations have been performed
      -- return the Transaction_Record appropriately (probably to an output
      -- line driver)
      null;            -- stub
      
      -- For the test check that the return values are as expected
      if TC_Original_Transaction_Code /= This_Transaction.Code then
         -- Incorrect rendezvous
         Report.Failed ("Message Task: Incorrect code returned");
      end if;

      if This_Transaction.Code = Credit then
         -- The only Credit message was the one that should have been aborted
         Report.Failed ("Abort was not effective");
      else
         if This_Transaction.Return_Value     /= Debit_Return or   
            This_Transaction.TC_Message_Count /= 1            or not
            This_Transaction.TC_Thru_Distrib       then
               Report.Failed ("Expected path not traversed");
         end if;
         TC_Debit_Message_Complete.Set_True;
      end if;

   exception
      when others => 
         Report.Failed ("Unexpected exception in Message_Task");

   end Message_Task;



   -- Dispose each input Transaction_Record to the appropriate 
   -- computation tasks
   --
   task body Distributor is

   begin
      loop
         select
            accept Input (Transaction : acc_Transaction_Record) do

               -- Indicate that the  message did pass through the
               -- Distributor Task
               Transaction.TC_Thru_Distrib := true;

               -- Pass this transaction on the appropriate computation
               -- task
               case Transaction.Code is 
                  when Credit =>
                     requeue Credit_Computation.Input;   -- without abort
                  when Debit => 
                     requeue Debit_Computation.Input;    -- without abort
               end case;
            end Input;
         or
            terminate;
         end select;
      end loop;

   exception
      when others => 
         Report.Failed ("Unexpected exception in Distributor");
   end Distributor;
                                          


   -- Computation task.
   --   Note:  After the computation is performed in this task and the 
   --          accept body is completed the rendezvous in the original
   --          message task is completed.                              
   task body Credit_Computation is
      Message_Count   : integer := 0;
   begin
      loop
         select 
            accept Input ( Transaction : acc_Transaction_Record) do
               -- Perform the computations required for this transaction
               --
               null;     -- stub

               -- The rest of this code is for Test Control
               --
               if not Transaction.TC_Thru_Distrib then
                  Report.Failed 
                         ("Credit Task: Wrong queue, Distributor bypassed");
               end if;
               if Transaction.code /= Credit then
                  Report.Failed
                         ("Credit Task: Requeue delivered to the wrong queue");
               end if;

               -- for the test plug a known value and count
               Transaction.Return_Value := Credit_Return;
               -- one, and only one message should pass through
               if Message_Count /= 0 then
                  Report.Failed ("Aborted Requeue was not canceled -1");
               end if;
               Message_Count := Message_Count + 1;
               Transaction.TC_Message_Count := Message_Count;
                        
               -- Having done the basic housekeeping we now need to signal
               -- that we are in the accept body of the credit task.  The
               -- message has arrived and the Line Driver may now abort the
               -- calling task
               TC_Handshake_A.Set_True;

               -- Now wait for the Line Driver to inform us the calling 
               -- task has been aborted
               while not TC_Handshake_B.Value loop
                  delay ImpDef.Minimum_Task_Switch;
               end loop;

               -- The abort has taken place
               -- Inform the Line Driver that we are still running in the
               -- accept body
               TC_Handshake_C.Set_True;

               -- Now wait for the Line Driver to digest this information
               while not TC_Handshake_D.Value loop
                  delay ImpDef.Minimum_Task_Switch;
               end loop;

               -- The Line driver has checked that the caller is not terminated
               -- We can now complete the accept
               
            end Input;
            -- We are out of the accept
            TC_Handshake_E.Set_True; 
                   
         or
            terminate;
         end select;
      end loop;
   exception
      when others => 
         Report.Failed ("Unexpected exception in Credit_Computation");
   end Credit_Computation;



   -- Computation task.
   --   Note:  After the computation is performed in this task and the 
   --          accept body is completed the rendezvous in the original
   --          message task is completed.                              
   task body Debit_Computation is
      Message_Count   : integer := 0;
   begin
      loop
         select
            accept Input (Transaction : acc_Transaction_Record) do
               -- Perform the computations required for this message
               --
               null;      -- stub

               -- The rest of this code is for Test Control
               --
               if not Transaction.TC_Thru_Distrib then
                  Report.Failed 
                         ("Debit Task: Wrong queue, Distributor bypassed");
               end if;
               if Transaction.code /= Debit then
                  Report.Failed
                         ("Debit Task: Requeue delivered to the wrong queue");
               end if;

               -- for the test plug a known value and count
               Transaction.Return_Value := Debit_Return;
               -- one, and only one, message should pass through
               Message_Count := Message_Count + 1;
               Transaction.TC_Message_Count := Message_Count;
            end Input;            
         or
            terminate;
         end select;
      end loop;
   exception
      when others => 
         Report.Failed ("Unexpected exception in Debit_Computation");


   end Debit_Computation;


begin -- c954014
   Report.Test ("C954014", "Abort a task that has a call" &
                                          " requeued_without_abort");

   Line_Driver.Start;   -- Start the test

   -- Wait for the message tasks to complete before reporting the result
   --
   while not (TC_Handshake_F.Value                  -- abort not effective?
              and TC_Debit_Message_Complete.Value   -- Distributor affected?
              and TC_Handshake_E.Value ) loop       -- accept not completed?
      delay ImpDef.Minimum_Task_Switch;
   end loop;

   Report.Result;

end C954014;
