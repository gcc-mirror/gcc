-- C954013.A
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
--      Check that a requeue is cancelled and that the requeuing task is
--      unaffected when the calling task is aborted. 
--      Specifically, check requeue to an entry in a different task,
--      requeue where the entry has parameters, and requeue with abort.
--
-- TEST DESCRIPTION:
--      Abort a task that has a call requeued to the entry queue of another
--      task.  We do this by sending two messages to the Distributor which 
--      requeues them to the Credit task.  In the accept body of the Credit 
--      task we wait for the second message to arrive then check that an 
--      abort of the second message task does result in the requeue being 
--      removed.  The Line Driver task which generates the messages and the 
--      Credit task communicate artificially in this test to arrange for the 
--      proper timing of the messages and the abort.  One extra message is 
--      sent to the Debit task to ensure that the Distributor is still viable
--      and has been unaffected by the abort.
--
--      This series of tests uses a simulation of a transaction driven 
--      processing system.  Line Drivers accept input from an external source
--      and build them into transaction records.  These records are then 
--      encapsulated in message tasks which remain extant for the life of the
--      transaction in the system.  The message tasks put themselves on the 
--      input queue of a Distributor which, from information in the 
--      transaction and/or system load conditions forwards them to other 
--      operating tasks. These in turn might forward the transactions to yet 
--      other tasks for further action.  The routing is, in real life, dynamic
--      and unpredictable at the time of message generation.  All rerouting in
--      this  model is done by means of requeues.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      25 Nov 95   SAIC    Fixed shared global variable problems for
--                          ACVC 2.0.1
--
--!

with Report;
with ImpDef;
         
procedure C954013 is
 

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
   TC_Credit_Message_Complete : Shared_Boolean (False);


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
         TC_Thru_Dist     : Boolean := false;
      end record;

   
   task type Message_Task is 
      entry Accept_Transaction (In_Transaction : acc_Transaction_Record);
   end Message_Task;
   type acc_Message_Task is access Message_Task;

   task Line_Driver is
      entry Start;   
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

   -- This protected object is here for Test Control purposes only
   protected TC_Prt is 
      procedure Set_First_Has_Arrived;
      procedure Set_Second_Has_Arrived;
      procedure Set_Abort_Has_Completed;
      function  First_Has_Arrived   return Boolean;
      function  Second_Has_Arrived  return Boolean;
      function  Abort_Has_Completed return Boolean;
   private 
      First_Flag, Second_Flag, Abort_Flag : Boolean := false;
   end TC_Prt;

   protected body TC_Prt is

      Procedure Set_First_Has_Arrived is
      begin
         First_Flag := true;
      end Set_First_Has_Arrived;

      Procedure Set_Second_Has_Arrived is
      begin
         Second_Flag := true;
      end Set_Second_Has_Arrived;

      Procedure Set_Abort_Has_Completed is
      begin
         Abort_Flag := true;
      end Set_Abort_Has_Completed;
   
      Function First_Has_Arrived return boolean is
      begin
         return First_Flag;
      end First_Has_Arrived;

      Function Second_Has_Arrived return boolean is
      begin
         return Second_Flag;
      end Second_has_Arrived;

      Function Abort_Has_Completed return boolean is
      begin
         return Abort_Flag;
      end Abort_Has_Completed;
   
   end TC_PRT;

   -- Assemble messages received from an external source
   --   Creates a message task for each. The message tasks remain extant
   --   for the life of the messages in the system.
   --      TC: The Line Driver task would normally be designed to loop
   --      continuously creating the messages as input is received.  Simulate
   --      this  but limit it to three dummy messages for this test and use
   --      special artificial checks to pace the messages out under controlled
   --      conditions for the test; allow it to terminate at the end
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

      for i in 1..3 loop  -- TC: arbitrarily limit to two credit messages
                          --     and one debit, then complete
         declare 
            -- Create a task for the next message
            Next_Message_Task : acc_Message_Task := new Message_Task;
            -- Create a record for it
            Next_Transaction : acc_Transaction_Record :=
                                                       new Transaction_Record;
         begin
            if not TC_First_Message_Sent then
               -- send out the first message to start up the Credit task
               Build_Credit_Record ( Next_Transaction );            
               Next_Message_Task.Accept_Transaction ( Next_Transaction );  
               TC_First_Message_Sent := true;
            elsif not TC_Prt.Abort_Has_Completed then
               -- We have not yet processed the second message
               -- Wait to send the second message until we know the first
               -- has arrived at the Credit task and that task is in the 
               -- accept body 
               while not TC_Prt.First_Has_Arrived loop
                  delay ImpDef.Minimum_Task_Switch;   
               end loop;

               -- We can now send the second message
               Build_Credit_Record( Next_Transaction );
               Next_Message_Task.Accept_Transaction ( Next_Transaction );  
               
               -- Now wait for the second to arrive on the Credit input queue
               while not TC_Prt.Second_Has_Arrived loop
                  delay ImpDef.Minimum_Task_Switch;   
               end loop;
               
               -- At this point: The Credit task is in the accept block 
               -- dealing with the first message and the second message is
               -- is on the input queue
               abort Next_Message_Task.all;  -- Note: we are still in the 
                                             -- declare block for the 
                                             -- second message task

               -- Make absolutely certain that all the actions
               -- associated with the abort have been completed, that the
               -- task has gone from Abnormal right through to
               -- Termination.  All requeues that are to going to be
               -- cancelled will have been by the point of Termination.
               while not Next_Message_Task.all'terminated loop
                  delay ImpDef.Minimum_Task_Switch;   
               end loop;


               -- We now signal the Credit task that the abort has taken place
               -- so that it can check that the entry queue is empty as the
               -- requeue should have been cancelled
               TC_Prt.Set_Abort_Has_Completed;
            else
               -- The main part of the test is complete. Send one Debit message
               -- as further exercise of the Distributor to ensure it has not
               -- been affected by the cancellation of the requeue.
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
         if This_Transaction.Return_Value     /= Credit_Return or   
            This_Transaction.TC_Message_Count /= 1             or not
            This_Transaction.TC_Thru_Dist                      then 
               Report.Failed ("Expected path not traversed");
         end if;
         TC_Credit_Message_Complete.Set_True;
      else
         if This_Transaction.Return_Value     /= Debit_Return  or   
            This_Transaction.TC_Message_Count /= 1             or not
            This_Transaction.TC_Thru_Dist                      then 
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
               -- Show that this message did pass through the Distributor Task
               Transaction.TC_Thru_Dist := true;

               -- Pass this transaction on the the appropriate computation
               -- task
               case Transaction.Code is 
                  when Credit =>
                     requeue Credit_Computation.Input with abort;
                  when Debit => 
                     requeue Debit_Computation.Input with abort;
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
               if not Transaction.TC_Thru_Dist then 
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
                  Report.Failed ("Aborted Requeue was not cancelled -1");
               end if;
               Message_Count := Message_Count + 1;
               Transaction.TC_Message_Count := Message_Count;
         
               
               -- Having done the basic housekeeping we now need to signal
               -- that we are in the accept body of the credit task.  The
               -- first message has arrived and the Line Driver may now send
               -- the second one
               TC_Prt.Set_First_Has_Arrived;

               -- Now wait for the second to arrive

               while Input'Count = 0 loop
                  delay ImpDef.Minimum_Task_Switch;   
               end loop;
               -- Second message has been requeued - the Line driver may 
               -- now abort the calling task
               TC_Prt.Set_Second_Has_Arrived;

               -- Now wait for the Line Driver to signal that the abort of
               -- the first task is complete - the requeue should be cancelled
               -- at this time
               while not TC_Prt.Abort_Has_Completed loop
                  delay ImpDef.Minimum_Task_Switch;   
               end loop;
               
               if Input'Count /=0 then
                  Report.Failed ("Aborted Requeue was not cancelled -2");
               end if;
               -- We can now complete the rendezvous with the first caller
            end Input;
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
               if not Transaction.TC_Thru_Dist then 
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


begin -- c954013

   Report.Test ("C954013", "Abort a task that has a call requeued");

   Line_Driver.Start;   -- start the test

   -- Wait for the message tasks to complete before calling Report.Result.
   -- Although two Credit tasks are generated one is aborted so only
   -- one completes, thus a single flag is sufficient
   -- Note: the test will hang here if there is a problem with the 
   -- completion of the tasks
   while not (TC_Credit_Message_Complete.Value and 
              TC_Debit_Message_Complete.Value) loop
      delay ImpDef.Minimum_Task_Switch;   
   end loop;

   Report.Result;

end C954013;
