-- C910001.A
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
--      Check that tasks may have discriminants.  Specifically, check where
--      the subtype of the discriminant is a discrete subtype and where it is
--      an access subtype.  Check the case where the default values of the
--      discriminants are used.
--
-- TEST DESCRIPTION:
--      A task is defined with two discriminants, one a discrete subtype and
--      another that is an access subtype.  Tasks are created with various
--      values for discriminants and code within the task checks that these
--      are passed in correctly.  One instance of a default is used.  The
--      values passed to the task as the discriminants are taken from an
--      array of test data and the values received are checked against the
--      same array.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;

procedure C910001 is
 

   type App_Priority is range 1..10;
   Default_Priority : App_Priority := 5;

   type Message_ID is range 1..10_000; 

   type TC_Number_of_Messages is range 1..5;

   type TC_rec is record
      TC_ID       : Message_ID;
      A_Priority  : App_Priority;
      TC_Checked  : Boolean;
   end record;

   -- This table is used to create the messages and to check them
   TC_table : array (1..TC_Number_of_Messages'Last) of TC_Rec := 
                              (  ( 10, 6, false ),
                                 ( 20, 2, false ),
                                 ( 30, 9, false ),
                                 ( 40, 1, false ),
                                 ( 50, Default_Priority, false ) );

begin -- C910001

   Report.Test ("C910001", "Check that tasks may have discriminants");

      
   declare     -- encapsulate the test

      type Transaction_Record is 
         record
            ID               : Message_ID;
            Account_Number   : integer := 0;
            Stock_Number     : integer := 0;
            Quantity         : integer := 0;
            Return_Value     : integer := 0;
         end record;
      -- 
      type acc_Transaction_Record is access Transaction_Record;


      task type Message_Task
                   (In_Message  : acc_Transaction_Record := null;
                    In_Priority : App_Priority := Default_Priority) is
         entry Start;
      end Message_Task;
      type acc_Message_Task is access Message_Task;
      --
      --
      task body Message_Task is
         This_Message   : acc_Transaction_Record := In_Message;
         This_Priority  : App_Priority           := In_Priority; 
         TC_Match_Found : Boolean                := false;
      begin 
         accept Start;
         -- In the example envisioned this task would then queue itself
         -- upon some Distributor task which would send it off (requeue) to
         -- the message processing tasks according to the priority of the 
         -- message and the current load on the system.  For the test we
         -- just verify the data passed in as discriminants and exit the task
         -- 
         -- Check for the special case of default discriminants
         if This_Message = null then
            -- The default In_Message has been passed, check that the
            -- default priority was also passed
            if This_Priority /= Default_Priority then
               Report.Failed ("Incorrect Default Priority");
            end if;
            if TC_Table (TC_Number_of_Messages'Last).TC_Checked then
               Report.Failed ("Duplicate Default messages");
            else
               -- Mark that default has been seen
               TC_Table (TC_Number_of_Messages'Last).TC_Checked := True;
            end if; 
            TC_Match_Found := true;
         else
            -- Check the data against the table
            for i in TC_Number_of_Messages loop
               if TC_Table(i).TC_ID = This_Message.ID then
                  -- this is the right slot in the table
                  if TC_Table(i).TC_checked then
                     -- Already checked
                     Report.Failed ("Duplicate Data");
                  else 
                     TC_Table(i).TC_checked := true;
                  end if;
                  TC_Match_Found := true;
                  if TC_Table(i).A_Priority /= This_Priority then
                     Report.Failed ("ID/Priority mismatch");
                  end if;
                  exit;
               end if;
            end loop;
         end if;

         if not TC_Match_Found then
            Report.Failed ("No ID match in table");
         end if;

         -- Allow the task to terminate

      end Message_Task;


      -- The Line Driver task accepts data from an external source and 
      -- builds them into a transaction record.  It then generates a
      -- message task.  This message "contains" the record and is given 
      -- a priority according to the contents of the message.  The priority 
      -- and transaction records are passed to the task as discriminants.
      --    In this test we use a dummy record.  Only the ID is of interest 
      --    so we pick that and the required priority from an array of 
      --    test data.  We artificially limit the endless driver-loop to 
      --    the number of messages required for the test and add a special
      --    case to check the defaults.
      --
      task Driver_Task;
      --
      task body Driver_Task is
      begin

         -- Create all but one of the required tasks
         --
         for i in 1..TC_Number_of_Messages'Last - 1   loop
            declare
               -- Create a record for the next message
               Next_Transaction : acc_Transaction_Record := 
                                                new Transaction_Record;
               -- Create a task for the next message
               Next_Message_Task : acc_Message_Task := 
                                 new Message_Task( Next_Transaction,  
                                                   TC_Table(i).A_Priority );

            begin
               -- Artificially plug the ID with the next from the table
               --    In reality the whole record would be built here
               Next_Transaction.ID := TC_Table(i).TC_ID;
               
               -- Ensure the task does not start executing till the 
               -- transaction record is properly constructed
               Next_Message_Task.Start;

            end;  -- declare
         end loop;

         -- For this subtest create one task with the default discriminants
         --
         declare 

            -- Create the task 
            Next_Message_Task : acc_Message_Task := new Message_Task;

         begin
            
            Next_Message_Task.Start;

         end; -- declare


      end Driver_Task;

   begin    
      null;
   end;     -- encapsulation

   -- Now verify that all the tasks executed and checked in
   for i in TC_Number_of_Messages loop
      if not TC_Table(i).TC_Checked then 
         Report.Failed 
                 ("Task" & integer'image(integer (i) ) & " did not verify");
      end if;
   end loop; 
   Report.Result;

end C910001;
