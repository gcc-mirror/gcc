-- C954012.A
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
--     Check a requeue within an accept body to another entry in the same task
--     Specifically, check a call with parameters and a requeue with abort.
--
-- TEST DESCRIPTION:
--      One transaction is sent through to check the paths. After
--      processing this the Credit task sets the "overloaded" indicator.  Once
--      this indicator is set the Distributor queues low priority transactions
--      on a Wait_for_Underload queue in the same task using a requeue.  The
--      Distributor still delivers high priority transactions.  After two high
--      priority transactions have been processed by the Credit task it clears
--      the overload condition.  The low priority transactions should now be
--      delivered.
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
--      25 Nov 95   SAIC    Fixed shared global variable problem for
--                          ACVC 2.0.1
--      14 Mar 03   RLB     Fixed a race condition and an incorrect termination
--                          condition in the test.
--!

with Report;
with ImpDef;
with Ada.Calendar;

procedure C954012 is

   function "=" (X,Y: Ada.Calendar.Time) return Boolean
                                                   renames Ada.Calendar."=";

   -- Arbitrary test values
   Credit_Return : constant := 1;
   Debit_Return  : constant := 2;


   -- This is used as an "initializing" time for the messages as they are
   -- created.  As they pass through the Distributor they get a time_stamp
   -- of the current time. An arbitrary base time is chosen.
   --    TC: this fact is used, incidentally, to check that the messages have,
   --    indeed, passed through the Distributor as expected.
   --
   Base_Time : Ada.Calendar.Time := Ada.Calendar.Time_of(1959,3,9);


   -- Mechanism to count the number of Credit Message tasks completed
   protected TC_Tasks_Completed is
      procedure Increment;
      function  Count return integer;
   private
      Number_Complete : integer := 0;
   end TC_Tasks_Completed;

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

   TC_Debit_Message_Complete    : Shared_Boolean (False);
   -- Handshaking mechanism between the Line Driver and the Credit task
   TC_First_Message_Has_Arrived : Shared_Boolean (False);
   Credit_Overloaded : Shared_Boolean (False);

   TC_Credit_Messages_Expected  : constant integer := 5;

   type Transaction_Code is (Credit, Debit);
   type Transaction_Priority is (High, Low);

   type Transaction_Record;
   type acc_Transaction_Record is access Transaction_Record;
   type Transaction_Record is
      record
         ID               : integer := 0;
         Code             : Transaction_Code := Debit;
         Priority         : Transaction_Priority := High;
         Account_Number   : integer := 0;
         Stock_Number     : integer := 0;
         Quantity         : integer := 0;
         Return_Value     : integer := 0;
         Message_Count    : integer := 0;          -- for test
         Time_Stamp       : Ada.Calendar.Time := Base_Time;
      end record;


   task type Message_Task is
      entry Accept_Transaction (In_Transaction : acc_Transaction_Record);
   end Message_Task;
   type acc_Message_Task is access Message_Task;

   task Line_Driver is
      entry Start;
   end Line_Driver;

   task Distributor is
      entry Input (Transaction : acc_Transaction_Record);
      entry Wait_for_Underload (Transaction : acc_Transaction_Record);
      entry TC_Credit_OK;
   end Distributor;

   task Credit_Computation is
      entry Input(Transaction : acc_Transaction_Record);
   end Credit_Computation;

   task Debit_Computation is
      entry Input(Transaction : acc_Transaction_Record);
   end Debit_Computation;


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
   --      but limit it to the required number of dummy messages needed for
   --      this test and allow it to terminate at that point.  Artificially
   --      alternate High and Low priority Credit transactions for this test.
   --
   task body Line_Driver is
      Current_ID       : integer := 1;
      Current_Priority : Transaction_Priority := High;

      -- Artificial: number of messages required for this test
      type TC_Trans_Range is range 1..6;

      procedure Build_Credit_Record
                              ( Next_Transaction : acc_Transaction_Record ) is
         Dummy_Account : constant integer := 100;
      begin
            Next_Transaction.ID := Current_ID;
            Next_Transaction.Code := Credit;
            Next_Transaction.Priority := Current_Priority;

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

      accept Start;   -- Wait for trigger from Main

      for Transaction_Numb in TC_Trans_Range loop  -- TC: limit the loop
         declare
            -- Create a task for the next message
            Next_Message_Task : acc_Message_Task := new Message_Task;
            -- Create a record for it
            Next_Transaction : acc_Transaction_Record
                                                 := new Transaction_Record;
         begin
            if Transaction_Numb = TC_Trans_Range'first then
               -- Send the first Credit message
               Build_Credit_Record ( Next_Transaction );
               Next_Message_Task.Accept_Transaction ( Next_Transaction );
               -- TC: Wait until the first message has been received by the
               -- Credit task and it has set the Overload indicator for the
               -- Distributor
               while not TC_First_Message_Has_Arrived.Value loop
                  delay ImpDef.Minimum_Task_Switch;
               end loop;
            elsif Transaction_Numb = TC_Trans_Range'last then
               -- For this test send the last transaction to the Debit task
               -- to improve the mix
               Build_Debit_Record( Next_Transaction );
               Next_Message_Task.Accept_Transaction ( Next_Transaction );
            else
               -- TC: Alternate high and low priority transactions
               if Current_Priority = High then
                  Current_Priority := Low;
               else
                  Current_Priority := High;
               end if;
               Build_Credit_Record( Next_Transaction );
               Next_Message_Task.Accept_Transaction ( Next_Transaction );
            end if;
         end;   -- declare
      end loop;

      -- TC: Wait for Credit_Overloaded to be cleared, then insure that the
      -- Distributor has evalated all tasks. Otherwise, some tasks may never
      -- be evaluated.
      while Credit_Overloaded.Value loop
        delay ImpDef.Minimum_Task_Switch;
      end loop;
      Distributor.TC_Credit_OK;

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
         if This_Transaction.Return_Value  /= Credit_Return or
            This_Transaction.Time_Stamp     = Base_Time     then
               Report.Failed ("Expected path not traversed");
         end if;
            TC_Tasks_Completed.Increment;
      else
         if This_Transaction.Return_Value  /= Debit_Return or
            This_Transaction.Message_Count /= 1             or
            This_Transaction.Time_Stamp     = Base_Time     then
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
               -- Time_Stamp the messages with the current time
               --    TC: Used, incidentally, by the test to check that the
               --    message did pass through the Distributor Task
               Transaction.Time_Stamp := Ada.Calendar.Clock;

               -- Pass this transaction on to the appropriate computation
               -- task but temporarily hold low-priority transactions under
               -- overload conditions
               case Transaction.Code is
                  when Credit =>
                     if Credit_Overloaded.Value and
                        Transaction.Priority = Low then
                        requeue Wait_for_Underload with abort;
                     else
                        requeue Credit_Computation.Input with abort;
                     end if;
                  when Debit =>
                     requeue Debit_Computation.Input with abort;
               end case;
            end Input;
         or
            when not Credit_Overloaded.Value =>
            accept Wait_for_Underload (Transaction : acc_Transaction_Record) do
               requeue Credit_Computation.Input with abort;
            end Wait_for_Underload;
         or
            accept TC_Credit_OK;
               -- We need this to insure that we evaluate the guards at least
               -- once when Credit_Overloaded is False. Otherwise, tasks
               -- could stay queued on Wait_for_Underload forever (starvation).
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
   --
   task body Credit_Computation is

      Message_Count   : integer := 0;

   begin
      loop
         select
            accept Input ( Transaction : acc_Transaction_Record) do
               if Credit_Overloaded.Value and
                  Transaction.Priority = Low then
                  -- We should not be getting any Low Priority messages. They
                  -- should be waiting on the Distributor's Wait_for_Underload
                  -- queue
                  Report.Failed
                     ("Credit Task: Low priority transaction during overload");
               end if;
               -- Perform the computations required for this transaction
               null; -- stub

               -- For the test:
               if Transaction.Time_Stamp = Base_Time then
                  Report.Failed
                         ("Credit Task: Wrong queue, Distributor bypassed");
               end if;
               if Transaction.code /= Credit then
                  Report.Failed
                         ("Credit Task: Requeue delivered to the wrong queue");
               end if;

               -- The following is all Test Control code:
               Transaction.Return_Value := Credit_Return;
               Message_Count := Message_Count + 1;
               --
               -- Now take special action depending on which Message
               if Message_Count = 1 then
                  -- After the first message :
                  Credit_Overloaded.Set_True;
                  -- Now flag the Line_Driver that the second and subsequent
                  -- messages may now be sent
                  TC_First_Message_Has_Arrived.Set_True;
               end if;
               if Message_Count = 3 then
                  -- The two high priority transactions created subsequent
                  -- to the overload have now been processed
                  Credit_Overloaded.Set_False;
               end if;
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
               if Transaction.Time_Stamp = Base_Time then
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
               Transaction.Message_Count := Message_Count;
            end Input;
         or
            terminate;
         end select;
      end loop;
   exception
      when others =>
         Report.Failed ("Unexpected exception in Debit_Computation");


   end Debit_Computation;


begin -- c954012
   Report.Test ("C954012", "Requeue within an accept body" &
                           " to another entry in the same task");

   Line_Driver.Start;  -- Start the test

   -- Ensure that the message tasks complete before reporting the result
   while (TC_Tasks_Completed.Count < TC_Credit_Messages_Expected)
         or (not TC_Debit_Message_Complete.Value) loop
      delay ImpDef.Minimum_Task_Switch;
   end loop;

   Report.Result;

end C954012;
