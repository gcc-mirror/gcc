-- C954023.A
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
--     Check that a requeue within a protected entry to a family of entries 
--     in a different protected object is queued correctly
--                      Call with parameters
--                      Requeue with abort
--
-- TEST DESCRIPTION: 
--      One transaction is sent through to check the paths.  After processing
--      this, the Credit task sets the "overloaded" indicator.  Once this
--      indicator is set the Distributor (a protected object) queues lower
--      priority transactions on a family of queues (Wait_for_Underload) in
--      another protected object using a requeue.  The Distributor still
--      delivers high priority transactions. After two more high priority
--      transactions have been processed by the Credit task the artificial
--      test code clears the overload condition to the threshold level that
--      allows only the items on the Medium priority queue of the family to be
--      released.  When these have been processed and checked the test code
--      then lowers the priority threshold once again, allowing the Low
--      priority items from the last queue in the family to be released,
--      processed and checked. Note: the High priority queue in the family is 
--      not used.   
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
         
procedure C954023 is

   -- Artificial: number of messages required for this test
   subtype TC_Trans_Range is integer range 1..8;

   TC_Credit_Messages_Expected  : constant integer 
                                          := TC_Trans_Range'Last - 1;

   TC_Debit_Message_Complete    : Boolean := false;


   -- Mechanism for handshaking between tasks
   protected TC_PO is
      procedure Increment_Tasks_Completed_Count;
      function  Tasks_Completed_Count return integer;
      function  First_Message_Has_Arrived return Boolean;
      procedure  Set_First_Message_Has_Arrived;
   private
      Number_Complete : integer := 0;
         Message_Arrived_Flag  : Boolean := false;
   end TC_PO;
   -- 
   protected body TC_PO is
      procedure Increment_Tasks_Completed_Count is
      begin
         Number_Complete := Number_Complete + 1;
      end Increment_Tasks_Completed_Count;
  
      function Tasks_Completed_Count return integer is
      begin
         return Number_Complete;
      end Tasks_Completed_Count;

      function  First_Message_Has_Arrived return Boolean is
         begin
            return Message_Arrived_Flag;
         end First_Message_Has_Arrived;

      procedure  Set_First_Message_Has_Arrived is
         begin
            Message_Arrived_Flag := true;
      end Set_First_Message_Has_Arrived;

   end TC_PO;

begin
   
   Report.Test ("C954023", "Requeue from within a protected object" &
                      " to a family of entries in another protected object");

 
   declare -- encapsulate the test
   
      -- Arbitrary test values
      Credit_Return : constant := 1;
      Debit_Return  : constant := 2;

      type Transaction_Code is (Credit, Debit);
      type App_Priority     is (Low, Medium, High);
      type Priority_Block   is array (App_Priority) of Boolean;

      type Transaction_Record;
      type acc_Transaction_Record is access Transaction_Record;
      type Transaction_Record is 
         record
            ID               : integer := 0;
            Code             : Transaction_Code := Debit;
            Priority         : App_Priority := High;
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
   
      protected Distributor is
         procedure Set_Credit_Overloaded;
         procedure Clear_Overload_to_Medium;
         procedure Clear_Overload_to_Low;
         entry     Input (Transaction : acc_Transaction_Record);
      private
         Credit_Overloaded : Boolean := false;
      end Distributor;
   
      protected Hold is
         procedure Release_Medium;
         procedure Release_Low;
         -- Family of entry queues indexed by App_Priority
         entry Wait_for_Underload (App_Priority)
                                     (Transaction : acc_Transaction_Record);
      private
         Release : Priority_Block := (others => false);
      end Hold;
   
      task Credit_Computation is
         entry Input(Transaction : acc_Transaction_Record);
      end Credit_Computation;
   
      task Debit_Computation is
         entry Input(Transaction : acc_Transaction_Record);
      end Debit_Computation;
   
      --
      -- Dispose each input Transaction_Record to the appropriate
      -- computation tasks
      --
      protected body Distributor is
   
         procedure Set_Credit_Overloaded is
         begin
            Credit_Overloaded := true;
         end Set_Credit_Overloaded;
   
         procedure Clear_Overload_to_Medium is
         begin
            Credit_Overloaded := false;
            Hold.Release_Medium;       -- Release all held messages on Medium
                                       -- priority queue
         end Clear_Overload_to_Medium;
     
         procedure Clear_Overload_to_Low is
         begin
            Credit_Overloaded := false;
            Hold.Release_Low;          -- Release all held messages on Low
                                       -- priority queue
         end Clear_Overload_to_Low;
     
   
   
         entry Input (Transaction : acc_Transaction_Record)   when true is
                                                   -- barrier is always open
         begin
            -- Test Control: Set the indicator in the message to show it has
            -- passed through the Distributor object
            Transaction.TC_thru_Distrib := true;
    
            -- Pass this transaction on to the appropriate computation
            -- task but temporarily hold low-priority transactions under
            -- overload conditions
            case Transaction.Code is
               when Credit =>
                  if Credit_Overloaded and Transaction.Priority /= High then
                     -- use the appropriate queue in the family 
                     requeue Hold.Wait_for_Underload(Transaction.Priority)
                                                         with abort;
                  else
                     requeue Credit_Computation.Input with abort;
                 end if;
               when Debit =>
                 requeue Debit_Computation.Input with abort;
            end case;
         end Input;
      end Distributor;
   
   
      -- Low priority Message tasks are held on the Wait_for_Underload queue
      -- while the Credit computation system is overloaded.  Once the Credit
      -- system reached underload send all queued messages immediately
      --
      protected body Hold is
         
         -- Once these are executed the barrier conditions for the entries
         -- are evaluated 
         procedure Release_Medium is
         begin 
            Release(Medium) := true;
         end Release_Medium;
         --
         procedure Release_Low is
         begin 
            Release(Low) := true;
         end Release_Low;
   
         -- This is a family of entry queues indexed by App_Priority
         entry Wait_for_Underload  (for AP in App_Priority)
                                   (Transaction : acc_Transaction_Record)
                                                when Release(AP) is
         begin
            requeue Credit_Computation.Input with abort;
            if Wait_for_Underload(AP)'count = 0 then
               -- Queue is purged.  Set up to hold next batch
               Release(AP) := false;
            end if; 
         end Wait_for_Underload;
   
      end Hold;
      
   
   
   
      -- Assemble messages received from an external source
      --   Creates a message task for each. The message tasks remain extant
      --   for the life of the messages in the system.
      --      The Line Driver task would normally be designed to loop 
      --      creating the messages as input is received.  Simulate this 
      --      but limit it to the required number of dummy messages needed for
      --      this test and allow it to terminate at that point.  Artificially
      --      cycle the generation of High medium and Low priority Credit 
      --      transactions for this test. Send out one final Debit message
      --
      task body Line_Driver is
         Current_ID       : integer := 1;
         Current_Priority : App_Priority := High;      
   
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
         
         for Transaction_Numb in TC_Trans_Range loop  -- TC: limit the loop
            declare 
               -- Create a task for the next message
               Next_Message_Task : acc_Message_Task := new Message_Task;
               -- Create a record for it
               Next_Transaction : acc_Transaction_Record := 
                                                   new Transaction_Record;
            begin
               if Transaction_Numb = TC_Trans_Range'first then
                  -- Send the first Credit message
                  Build_Credit_Record ( Next_Transaction );
                  Next_Message_Task.Accept_Transaction ( Next_Transaction );  
                  -- TC: Wait until the first message has been received by the
                  -- Credit task and it has set the Overload indicator for the 
                  -- Distributor
                  while not TC_PO.First_Message_Has_Arrived loop
                     delay ImpDef.Minimum_Task_Switch;   
                  end loop;
               elsif Transaction_Numb = TC_Trans_Range'last then
                  -- For this test send the last transaction to the Debit task
                  -- to improve the mix
                  Build_Debit_Record( Next_Transaction );
                  Next_Message_Task.Accept_Transaction ( Next_Transaction );  
               else
                  -- TC: Cycle generation of  high medium and low priority 
                  -- transactions
                  if Current_Priority = High then
                     Current_Priority := Medium;
                  elsif  
                     Current_Priority = Medium then
                     Current_Priority := Low;
                  else
                     Current_Priority := High;
                  end if;
                  Build_Credit_Record( Next_Transaction );
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
         
         accept Accept_Transaction(In_Transaction : acc_Transaction_Record) do
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
            if This_Transaction.Return_Value  /= Credit_Return   or
            not This_Transaction.TC_thru_Distrib                 then
               Report.Failed ("Expected path not traversed - Credit");
            end if;
            TC_PO.Increment_Tasks_Completed_Count;
         else
            if This_Transaction.Return_Value  /= Debit_Return or   
               This_Transaction.TC_Message_Count /= 1         or
               not This_Transaction.TC_thru_Distrib           then
                  Report.Failed ("Expected path not traversed - Debit");
            end if;
            TC_Debit_Message_Complete := true;
         end if;
   
      exception
         when others => 
            Report.Failed ("Unexpected exception in Message_Task");
      end Message_Task;
   
   
   
   
     
      -- Computation task.  After the computation is performed the rendezvous
      -- in the original message task is completed.                              
      task body Credit_Computation is
   
         Message_Count   : integer := 0;
         
      begin
         loop
            select 
               accept Input ( Transaction : acc_Transaction_Record) do
                  
                  -- Perform the computations required for this transaction
                  null; -- stub
   
                  
                  -- The following is all Test Control code:
                 
                  if not Transaction.TC_thru_Distrib then
                     Report.Failed 
                            ("Credit Task: Wrong queue, Distributor bypassed");
                  end if;
   
                  if Transaction.code /= Credit then
                     Report.Failed
                         ("Credit Task: Requeue delivered to the wrong queue");
                  end if;
   
                  -- This is checked by the Message_Task:
                  Transaction.Return_Value := Credit_Return;
   
                  -- Now take special action depending on which Message.
                  -- Note: The count gives the order in which the messages are
                  --       arriving at this task NOT the order in which they
                  --       were originally generated and sent out.
   
                  Message_Count := Message_Count + 1;
   
                  if Message_Count < 4 then 
                     -- This is one of the first three messages which must 
                     -- be High priority because we will set "Overload" after 
                     -- the first, which is known to be High. The lower
                     -- priority should be waiting on the queues
                     if Transaction.Priority /= High then  
                     Report.Failed 
                        ("Credit Task: Lower priority trans. during overload");
                     end if;
                     if Message_Count = 1 then 
                        -- After the first message :
                        Distributor.Set_Credit_Overloaded;
                        -- Now flag the Line_Driver that the second and 
                        -- subsequent messages may now be sent
                        TC_PO.Set_First_Message_Has_Arrived;
                     elsif
                        Message_Count = 3 then
                        -- The two high priority transactions created 
                        -- subsequent to the overload have now been processed,
                        --  release the Medium priority items
                        Distributor.Clear_Overload_to_Medium; 
                     end if;
                  elsif Message_Count < 6 then
                     -- This must be one of the Medium priority messages
                     if Transaction.Priority /= Medium then  
                     Report.Failed 
                           ("Credit Task: Second group not Medium Priority");
                     end if;
                     if Message_Count = 5 then 
                        -- The two medium priority transactions
                        -- have now been processed - release the
                        -- Low priority items
                        Distributor.Clear_Overload_to_Low; 
                     end if;
                  elsif Message_Count < TC_Trans_Range'Last then
                     -- This must be one of the Low priority messages
                     if Transaction.Priority /= Low then  
                     Report.Failed 
                           ("Credit Task: Third group not Low Priority");
                     end if;
                  else
                     -- Too many transactions have arrived.  Duplicates?
                     -- the Debit transaction?
                     Report.Failed 
                           ("Credit Task: Too many transactions");
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
                  if not Transaction.TC_thru_Distrib then
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
   

   begin -- declare 

      null;

   end; -- declare (test encapsulation)

   if (TC_PO.Tasks_Completed_Count /= TC_Credit_Messages_Expected) 
                                    and not TC_Debit_Message_Complete then
      Report.Failed ("Incorrect number of Message Tasks completed");
   end if;

   Report.Result;

end C954023;
