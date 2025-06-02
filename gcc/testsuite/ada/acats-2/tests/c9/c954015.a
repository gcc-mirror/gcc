-- C954015.A
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
--      Check that requeued calls to task entries may, in turn, be requeued.  
--      Check that the intermediate requeues are not blocked and that the
--      original caller remains blocked until the last requeue is complete.
--         This test uses: 
--                      Call with parameters
--                      Requeue with abort
--      
-- TEST DESCRIPTION
--      A call is placed on the input queue of the Distributor.  The
--      Distributor requeues to the Credit task; the Credit task requeues to a
--      secondary task which, in turn requeues to yet another task.  This
--      continues down the chain.  At the furthest point of the chain the
--      rendezvous is completed.  To verify the action, the furthest task
--      waits in the accept statement for a second message to arrive before
--      completing.  This second message can only arrive if none of the earlier
--      tasks in the chain are blocked waiting for completion.   Apart from
--      the two Credit messages which are used to check the requeue chain one
--      Debit message is sent to validate the mix.
--
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
--
--!


with Report;
with ImpDef;
         
procedure C954015 is
 
   -- Arbitrary test values
   Credit_Return : constant := 1;
   Debit_Return  : constant := 2;

   -- Mechanism to count the number of Credit Message tasks completed
   protected TC_Tasks_Completed is
      procedure Increment;
      function  Count return integer;
   private
      Number_Complete : integer := 0;
   end TC_Tasks_Completed;
   
   TC_Expected_To_Complete   : constant integer := 3;


   -- Values added to the Return_Value indicating passage through the
   -- particular task
   TC_Credit_Value :  constant integer := 1;
   TC_Sub_1_Value  :  constant integer := 2;
   TC_Sub_2_Value  :  constant integer := 3;
   TC_Sub_3_Value  :  constant integer := 4;
   TC_Sub_4_Value  :  constant integer := 5;
   --
   TC_Full_Value   :  integer := TC_Credit_Value + TC_Sub_1_Value + 
                                 TC_Sub_2_Value  + TC_Sub_3_Value +
                                 TC_Sub_4_Value;

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
         TC_Thru_Distrib  : Boolean := false;
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

   -- The following are almost identical for the purpose of the test
   task Credit_Sub_1 is
      entry Input(Transaction : acc_Transaction_Record);
   end Credit_Sub_1;
   --
   task Credit_Sub_2 is
      entry Input(Transaction : acc_Transaction_Record);
   end Credit_Sub_2;
   --
   task Credit_Sub_3 is
      entry Input(Transaction : acc_Transaction_Record);
   end Credit_Sub_3;

   -- This is the last in the chain
   task Credit_Sub_4 is
      entry Input(Transaction : acc_Transaction_Record);
   end Credit_Sub_4;
  
   
   -- Mechanism to count the number of Message tasks completed (Credit)
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
   --   Creates a message task for each. The message tasks remain extant
   --   for the life of the messages in the system.
   --      The Line Driver task would normally be designed to loop continuously
   --      creating the messages as input is received.  Simulate this 
   --      but limit it to the number of dummy messages needed for this 
   --      test and allow it to terminate at that point. 
   --
   task body Line_Driver is
      Current_ID : integer := 1;
      TC_Last_was_for_credit : Boolean := false;

       -- Arbitrary limit for the number of messages sent for this test
      type TC_Trans_Range is range 1..3;

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

      accept Start;   -- wait for trigger from Main
      
      -- Arbitrarily limit the loop to the number needed for this test only
      for Transaction_Numb in TC_Trans_Range  loop
         declare 
            -- Create a task for the next message
            Next_Message_Task : acc_Message_Task := new Message_Task;
            -- Create a record for it
            Next_Transaction : acc_Transaction_Record := 
                                                new Transaction_Record;
         begin
            -- Artificially send out in the order required
            case Transaction_Numb is
               when 1 => 
                  Build_Credit_Record( Next_Transaction );
               when 2 =>
                  Build_Credit_Record( Next_Transaction );
               when 3 =>
                  Build_Debit_Record ( Next_Transaction );            
            end case;

            -- Present the record to the message task
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
         if This_Transaction.Return_Value  /= TC_Full_Value or not  
            This_Transaction.TC_Thru_Distrib       then
               Report.Failed ("Expected path not traversed - CR");
         end if;
         if 
            This_Transaction.TC_Message_Count not in 1..2 then
               Report.Failed ("Incorrect Message Count");
         end if;
      else
         if This_Transaction.Return_Value  /= Debit_Return or   
            This_Transaction.TC_Message_Count /= 1            or not
            This_Transaction.TC_Thru_Distrib     then
               Report.Failed ("Expected path not traversed - DB");
         end if;
      end if;
      TC_Tasks_Completed.Increment;
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
               -- Show that the message did pass through the Distributor Task
               Transaction.TC_Thru_Distrib := true;

               -- Pass this transaction on to the appropriate computation
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
   --   Note:  After the computation is performed in this task the message is
   --   passed on for further processing to some subsidiary task.  The choice
   --   of subsidiary task is made according to criteria not specified in
   --   this test.  
   --                                           
   task body Credit_Computation is
      Message_Count   : integer := 0;   
   begin
      loop
         select 
            accept Input ( Transaction : acc_Transaction_Record) do
               -- Perform the computations required for this transaction
               null;      -- stub

               -- For the test: 
               if not Transaction.TC_Thru_Distrib then
                  Report.Failed 
                         ("Credit Task: Wrong queue, Distributor bypassed");
               end if;
               if Transaction.code /= Credit then
                  Report.Failed
                         ("Credit Task: Requeue delivered to the wrong queue");
               end if;

               -- for the test, plug a known value and count
               Transaction.Return_Value := TC_Credit_Value;
               Message_Count := Message_Count + 1;
               Transaction.TC_Message_Count := Message_Count;

               -- Depending on transaction content send it on to the
               -- some other task for further processing
               -- TC: Arbitrarily send the message on to Credit_Sub_1
               requeue Credit_Sub_1.Input with abort;
            end Input;            
         or
            terminate;
         end select;
      end loop;
   exception
      when others => 
         Report.Failed ("Unexpected exception in Credit_Computation");
   end Credit_Computation;



   task body Credit_Sub_1 is
   begin
      loop
         select
            accept Input(Transaction : acc_Transaction_Record) do
               -- Process this transaction
               null;   -- stub
            
               -- Add the value showing passage through this task
               Transaction.Return_Value := 
                                    Transaction.Return_Value + TC_Sub_1_Value;
               -- Depending on transaction content send it on to the
               -- some other task for further processing
               -- Arbitrarily send the message on to Credit_Sub_2
               requeue Credit_Sub_2.Input with abort;  
            end Input;
         or 
            terminate;
         end select;
      end loop;
   exception
      when others => 
         Report.Failed ("Unexpected exception in Credit_Sub_1");

   end Credit_Sub_1;

   task body Credit_Sub_2 is
   begin
      loop
         select
            accept Input(Transaction : acc_Transaction_Record) do
               -- Process this transaction
               null;   -- stub
            
               -- Add the value showing passage through this task
               Transaction.Return_Value := 
                                    Transaction.Return_Value + TC_Sub_2_Value;
               -- Depending on transaction content send it on to the
               -- some other task for further processing
               -- Arbitrarily send the message on to Credit_Sub_3
               requeue Credit_Sub_3.Input with abort;  
            end Input;
         or 
            terminate;
         end select;
      end loop;
   exception
      when others => 
         Report.Failed ("Unexpected exception in Credit_Sub_2");
   end Credit_Sub_2;

   task body Credit_Sub_3 is
   begin
      loop
         select
            accept Input(Transaction : acc_Transaction_Record) do
               -- Process this transaction
               null;   -- stub
            
               -- Add the value showing passage through this task
               Transaction.Return_Value := 
                                    Transaction.Return_Value + TC_Sub_3_Value;
               -- Depending on transaction content send it on to the
               -- some other task for further processing
               -- Arbitrarily send the message on to Credit_Sub_4
               requeue Credit_Sub_4.Input with abort;  
            end Input;
         or 
            terminate;
         end select;
      end loop;
   exception
      when others => 
         Report.Failed ("Unexpected exception in Credit_Sub_3");
   end Credit_Sub_3;

   -- This is the last in the chain of tasks to which transactions will
   -- be requeued
   --
   task body Credit_Sub_4 is
   
      TC_First_Message : Boolean := true;

   begin
      loop
         select
            accept Input(Transaction : acc_Transaction_Record) do
               -- Process this transaction
               null;   -- stub
            
               -- Add the value showing passage through this task
               Transaction.Return_Value := 
                                    Transaction.Return_Value + TC_Sub_4_Value;
               -- TC: stay in the accept body dealing with the first message
               -- until the second arrives.  If any of the requeues are 
               -- blocked the test will hang here indicating failure
               if TC_First_Message then
                  while Input'count = 0 loop
                     delay ImpDef.Minimum_Task_Switch;
                  end loop;
               TC_First_Message := false;
               end if;
               -- for the second message, just complete the rendezvous
            end Input;
         or 
            terminate;
         end select;
      end loop;
   exception
      when others => 
         Report.Failed ("Unexpected exception in Credit_Sub_4");
   end Credit_Sub_4;


                                                
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

               -- For the test: 
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


begin 

   Report.Test ("C954015", "Test multiple levels of requeue to task entry");

   Line_Driver.Start;  -- Start the test

   -- Ensure that the message tasks completed before calling Result
   while (TC_Tasks_Completed.Count < TC_Expected_To_Complete) loop
      delay ImpDef.Minimum_Task_Switch;
   end loop;

   Report.Result;

end C954015;
