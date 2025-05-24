-- C954020.A
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
--      Check that a call to a protected entry can be requeued to a task
--      entry. Check that the requeue is placed on the correct entry; that the
--      original caller waits for the completion of the requeue and continues
--      after the requeued rendezvous.  Check that the requeue does not block.
--      Specifically, check a requeue with abort from a protected entry to
--      an entry in a task.
--
-- TEST DESCRIPTION:
--
--      In the Distributor protected object, requeue two successive calls on
--      the entries of two separate target tasks.  Each task in each of the 
--      paths adds identifying information in the transaction being passed. 
--      This information is checked by the Message tasks on completion
--      ensuring that the requeues have been placed on the correct queues.
--      There is an artificial guard on the Credit Task to ensure that the
--      input is queued; this guard is released by the Debit task which 
--      handles its input immediately.  This ensures that we have one of the
--      requeued items actually queued for later handling and also verifies
--      that the requeuing process (in the protected object) is not blocked.
--
--      This series of tests uses a simulation of a transaction driven 
--      processing system.  Line Drivers accept input from an external source
--      and build them into transaction records.  These records are then 
--      encapsulated in message tasks which remain extant for the life of the
--      transaction in the system. The message tasks put themselves on the 
--      input queue of a Distributor object which, from information in the
--      transaction and/or system load conditions forwards them to other 
--      operating tasks. These in turn might forward the transactions to yet 
--      other tasks for further action.  The routing is, in real life, 
--      dynamic and unpredictable at the time of message generation. All 
--      rerouting in this  model is done by means of requeues.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      06 Nov 95   SAIC    Fixed problems for ACVC 2.0.1
--
--!

with Report;
with ImpDef;
         
procedure C954020 is
   Verbose : constant Boolean := False;
 
   -- Arbitrary test values
   Credit_Return : constant := 1;
   Debit_Return  : constant := 2;

   protected type Message_Status is
      procedure Set_Complete;
      function Complete return Boolean;
   private
      Is_Complete : Boolean := False;
   end Message_Status;

   protected body Message_Status is
      procedure Set_Complete is
      begin
         Is_Complete := True;
      end Set_Complete;

      function Complete return Boolean is
      begin
         return Is_Complete;
      end Complete;
   end Message_Status;

   TC_Debit_Message : Message_Status;
   TC_Credit_Message : Message_Status;

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

   task Credit_Computation is
      entry Input(Transaction : acc_Transaction_Record);
   end Credit_Computation;

   task Debit_Computation is
      entry Input(Transaction : acc_Transaction_Record);
   end Debit_Computation;

   protected Time_Lock is
      procedure Credit_Start;
      function  Credit_Enabled return Boolean;
   private
      Credit_OK : Boolean := false;
   end Time_Lock;

   protected body Time_Lock is
      procedure Credit_Start is
      begin
         Credit_OK := true;
      end Credit_Start;

      function  Credit_Enabled return Boolean is 
      begin
         return Credit_OK;
      end Credit_Enabled;
   end Time_Lock;



   protected Distributor is
      entry Input (Transaction : acc_Transaction_Record);
   end Distributor;
   --
   --
   -- Dispose each input Transaction_Record to the appropriate
   -- computation tasks
   --
   protected body Distributor is
      entry Input (Transaction : acc_Transaction_Record) when true is
                                                      -- barrier is always open
      begin
         -- Test Control: Set the indicator in the message to show it has
         -- passed through the Distributor object
         Transaction.TC_thru_Dist := true;
             
         -- Pass this transaction on to the appropriate computation
         -- task
         case Transaction.Code is
            when Credit =>
               requeue Credit_Computation.Input with abort;
            when Debit =>
               requeue Debit_Computation.Input with abort;
         end case;
      end Input;
   end Distributor;




   -- Assemble messages received from an external source
   --   Creates a message task for each. The message tasks remain extant
   --   for the life of the messages in the system.
   --      The Line Driver task would normally be designed to loop continuously
   --      creating the messages as input is received.  Simulate this 
   --      but limit it to two dummy messages for this test and allow it
   --      to terminate at that point
   --
   task body Line_Driver is
      Current_ID : integer := 1;
      TC_Last_was_for_credit : Boolean := false;

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

      accept Start;      -- Wait for trigger from Main

      for i in 1..2 loop  -- arbitrarily limit to two messages for the test
         declare 
            -- Create a task for the next message
            Next_Message_Task : acc_Message_Task := new Message_Task;
            -- Create a record for it
            Next_Transaction : acc_Transaction_Record 
                                                 := new Transaction_Record;
         begin
            if TC_Last_was_for_credit then
               Build_Debit_Record ( Next_Transaction );            
            else
               Build_Credit_Record( Next_Transaction );
               TC_Last_was_for_credit := true;
            end if;
            Next_Message_Task.Accept_Transaction ( Next_Transaction );  
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

      if Verbose then
         Report.Comment ("message task got " &
                         Transaction_Code'Image (This_Transaction.Code));
      end if;

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

      
      -- The following is all Test Control Code

      -- Check that the return values are as expected
      if TC_Original_Transaction_Code /= This_Transaction.Code then
         -- Incorrect rendezvous
         Report.Failed ("Message Task: Incorrect code returned");
      end if;

      if This_Transaction.Code = Credit then
         if This_Transaction.Return_Value  /= Credit_Return or   
            This_Transaction.TC_Message_Count /= 1          or 
            not This_Transaction.TC_thru_Dist       then
               Report.Failed ("Expected path not traversed");
         end if;
         TC_Credit_Message.Set_Complete;
      else
         if This_Transaction.Return_Value  /= Debit_Return or   
            This_Transaction.TC_Message_Count /= 1         or 
            not This_Transaction.TC_thru_Dist       then
               Report.Failed ("Expected path not traversed");
         end if;
         TC_Debit_Message.Set_Complete;
      end if;

   exception
      when others => 
         Report.Failed ("Unexpected exception in Message_Task");

   end Message_Task;



   -- Computation task.
   --   Note:  After the computation is performed in this task and the 
   --          accept body is completed the rendezvous in the original
   --          message task is completed.                              
   --                                           
   task body Credit_Computation is
      Message_Count   : integer := 0;
   begin
      loop
         select 
            when  Time_Lock.Credit_enabled =>
            accept Input ( Transaction : acc_Transaction_Record) do
               -- Perform the computations required for this transaction
               null;      -- stub

               if Verbose then
                  Report.Comment ("Credit_Computation in accept");
               end if;

               -- For the test: 
               if not Transaction.TC_thru_Dist then
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
               Message_Count := Message_Count + 1;
               Transaction.TC_Message_Count := Message_Count;

            end Input;            
            exit;   -- only handle 1 transaction
         else
            -- poll until we can accept credit transaction
            delay ImpDef.Clear_Ready_Queue;
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
   --                                                
   task body Debit_Computation is
      Message_Count   : integer := 0;
   begin
      loop
         select
            accept Input (Transaction : acc_Transaction_Record) do
               -- Perform the computations required for this message
               null;      -- stub

               if Verbose then
                  Report.Comment ("Debit_Computation in accept");
               end if;

               -- For the test: 
               if not Transaction.TC_thru_Dist then
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
               -- for the test: once we have completed the only Debit
               -- message release the Credit Messages which are queued
               -- on the Credit Input queue
               Time_Lock.Credit_Start;
            end Input;            
         or
            terminate;
         end select;
      end loop;
   exception
      when others => 
         Report.Failed ("Unexpected exception in Debit_Computation");


   end Debit_Computation;


begin -- C954020

   Report.Test ("C954020", "Requeue, with abort, from protected entry " &
                                                         "to task entry");

   Line_Driver.Start;        -- Start the test

   -- Ensure that the message tasks complete before reporting the result
   while not (TC_Credit_Message.Complete and TC_Debit_Message.Complete) loop
      delay ImpDef.Minimum_Task_Switch;   
   end loop;

   Report.Result;

end C954020;
