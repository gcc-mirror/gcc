-- C954019.A
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
--      Check that when a requeue is to the same entry the items go to the
--      right queue and that they are placed back on the end of the queue.
--
-- TEST DESCRIPTION:
--      Simulate part of a message handling application where the messages are
--      composed of several segments.  The sequence of the segments within the
--      message is specified by Seg_Sequence_No.   The segments are handled by
--      different tasks  and finally forwarded to an output driver.  The
--      segments can arrive in any order but must be assembled into the proper
--      sequence for final output.  There is a Sequencer task interposed
--      before the Driver.  This takes the segments of the message off the
--      Ordering_Queue and those that are in the right order it sends on to
--      the driver; those that are out of order it places back on the end of
--      the queue.
--
--      The test just simulates the arrival of the segments at the Sequencer.
--      The task generating the segments handshakes with the Sequencer during
--      the  "Await Arrival" phase  ensuring that the three segments of a
--      message arrive in REVERSE order (the End-of-Message segment arrives
--      first and the Header last).  In the first cycle the sequencer pulls
--      segments off the queue and puts them back on the end till it
--      encounters the header.  It checks the sequence of the ones it pulls
--      off in case the segments are being put back on in the wrong part of
--      the queue. Having cycled once through it no longer verifies the
--      sequence - it just executes the "application" code for the correct
--      order for dispatch to the driver.
-- 
--      In this simple example no attempt is made to address segments of
--      another message arriving or any other error conditions (such as
--      missing segments, timing etc.)
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      19 Dec 94   SAIC    Remove parameter from requeue statement
--
--!

with Report;
with ImpDef;

procedure C954019 is
begin


   Report.Test ("C954019", "Check Requeue to the same Accept");
   
   declare  -- encapsulate the test

      type Segment_Sequence is range 1..8;
      Header : constant Segment_Sequence := Segment_Sequence'first;

      type Message_Segment is record
         ID              : integer;            -- Message ID
         Seg_Sequence_No : Segment_Sequence;   -- Within the message
         Alpha           : string (1..128);
         EOM             : Boolean := false;   -- true for final msg segment
      end record;
      type acc_Message_Segment is access Message_Segment;

      task TC_Simulate_Arrival;
   
      task type Carrier_Task is
         entry Input ( Segment : acc_Message_Segment );
      end Carrier_Task;
      type acc_Carrier_Task is access Carrier_Task;
   
      task Sequencer is
         entry Ordering_Queue ( Segment : acc_Message_Segment );
         entry TC_Handshake_1;
         entry TC_Handshake_2;
      end Sequencer;
   
      task Output_Driver is
         entry Input ( Segment : acc_Message_Segment );
      end Output_Driver;
   

      -- Simulate the arrival of three message segments in REVERSE order
      --
      task body TC_Simulate_Arrival is
         begin

            for i in 1..3 loop
               declare
                  -- Create a task for the next message segment
                  Next_Segment_Task : acc_Carrier_Task := new Carrier_Task;
                  -- Create a record for the next segment
                  Next_Segment : acc_Message_Segment := new Message_Segment;
               begin
                  if i = 1 then 
                     -- Build the EOM segment as the first to "send"
                     Next_Segment.Seg_Sequence_No := Header + 2;
                     Next_Segment.EOM := true;
                  elsif i = 2 then
                     -- Wait for the first segment to arrive at the Sequencer
                     -- before "sending" the second
                     Sequencer.TC_Handshake_1;
                     -- Build the segment
                     Next_Segment.Seg_Sequence_No := Header + 1;
                  else 
                     -- Wait for the second segment to arrive at the Sequencer
                     -- before "sending" the third
                     Sequencer.TC_Handshake_2;
                     -- Build the segment. The last segment in order to 
                     -- arrive will be the "header" segment
                     Next_Segment.Seg_Sequence_No := Header;
                  end if;
                  -- pass the record to its carrier
                  Next_Segment_Task.Input ( Next_Segment );
               end;
            end loop;
      exception
         when others => 
              Report.Failed ("Unexpected Exception in TC_Simulate_Arrival");
      end TC_Simulate_Arrival;
   

      -- One of these is generated for each message segment and the flow 
      -- of the segments through the system is controlled by the calls the
      -- task makes and the requeues of those calls
      --
      task body Carrier_Task is
         This_Segment : acc_Message_Segment := new Message_Segment;
      begin
         accept Input ( Segment : acc_Message_Segment ) do
            This_Segment.all := Segment.all;
         end Input;
         null; --:: stub.  Pass the segment around the application as needed

         -- Now output the segment to the Output_Driver.  First we have to 
         -- go through the Sequencer.  
         Sequencer.Ordering_Queue ( This_Segment );
      exception
         when others => 
              Report.Failed ("Unexpected Exception in Carrier_Task");
      end Carrier_Task;
   

      -- Pull segments off the Ordering_Queue and deliver them in the correct
      -- sequence to the Output_Driver.
      --
      task body Sequencer is
         Next_Needed : Segment_Sequence := Header;
   
         TC_Await_Arrival     : Boolean := true;
         TC_First_Cycle       : Boolean := true;  
         TC_Expected_Sequence : Segment_Sequence := Header+2;
      begin
         loop
            select
               accept Ordering_Queue ( Segment : acc_Message_Segment ) do

                  --=====================================================
                  -- This part is all Test_Control code

                  if TC_Await_Arrival then
                     -- We have to arrange that the segments arrive on the 
                     -- queue in the right order, so we handshake with the 
                     -- TC_Simulate_Arrival task to "send" only one at 
                     -- a time
                     accept TC_Handshake_1;   -- the first  has arrived
                                              -- and has been pulled off the
                                              -- queue

                     -- Wait for the second to arrive (the first has already
                     -- been pulled off the queue
                     while Ordering_Queue'count < 1 loop
                        delay ImpDef.Minimum_Task_Switch;
                     end loop;
                     --
                     accept TC_Handshake_2;   -- the second has arrived

                     -- Wait for the third to arrive
                     while Ordering_Queue'count < 2 loop
                        delay ImpDef.Minimum_Task_Switch;
                     end loop;

                     -- Subsequent passes through the loop, bypass this code
                     TC_Await_Arrival := false; 
                              

                  end if; -- await arrival

                  if TC_First_Cycle then 
                     -- Check the order of the original three 
                     if Segment.Seg_Sequence_No /= TC_Expected_Sequence then
                        -- The segments are not being pulled off in the 
                        -- expected sequence.  This could occur if the 
                        -- requeue is not putting them back on the end.
                        Report.Failed ("Sequencer: Segment out of sequence");
                     end if; -- sequence check
                     -- Decrement the expected sequence
                     if TC_Expected_Sequence /= Header then
                        TC_Expected_Sequence := TC_Expected_Sequence - 1;
                     else 
                        TC_First_Cycle := false; -- This is the Header - the
                                                 -- first two segments are
                                                 -- back on the queue
                                                 
                     end if; -- decrementing
                  end if; -- first pass
                  --=====================================================

                  -- And this is the Application code
                  if Segment.Seg_Sequence_No = Next_Needed then
                     if Segment.EOM then
                        Next_Needed := Header;  -- reset for next message
                     else
                        Next_Needed := Next_Needed + 1;
                     end if;
                     requeue Output_Driver.Input with abort;
                     Report.Failed ("Requeue did not complete accept body");
                  else
                     -- Not the next needed - put it back on the queue
                     requeue Sequencer.Ordering_Queue;
                     Report.Failed ("Requeue did not complete accept body");
                  end if;   
               end Ordering_Queue;
            or
               terminate;
            end select;
         end loop;
      exception
         when others => 
              Report.Failed ("Unexpected Exception in Sequencer");
      end Sequencer;
   

      task body Output_Driver is
         This_Segment : acc_Message_Segment := new Message_Segment;

         TC_Expected_Sequence : Segment_Sequence := Segment_Sequence'first;
         TC_Segment_Total     : integer := 0;
         TC_Expected_Total    : integer := 3;
      begin
         loop
            -- Note: normally we would expect this Accept to be in a select 
            -- with terminate.  For the test we exit the loop on completion
            -- to give better control
            accept Input ( Segment : acc_Message_Segment ) do 
               This_Segment.all := Segment.all;
            end Input;
   
            null;  --::: stub - output the next segment of the message

            -- The following is all test control code
            --
            if This_Segment.Seg_Sequence_No /= TC_Expected_Sequence then
               Report.Failed ("Output_Driver: Segment out of sequence");
            end if;
            TC_Expected_Sequence := TC_Expected_Sequence + 1;

            -- Now count the number of segments
            TC_Segment_Total := TC_Segment_Total + 1;

            -- Check the number and exit loop when complete
            -- There must be exactly TC_Expected_Total in number and
            --    the last one must be EOM
            --    (test will hang if < TC_Expected_Total arrive
            --    without EOM)
            if This_Segment.EOM then
               -- This is the last segment.
               if TC_Segment_Total /= TC_Expected_Total then
                  Report.Failed ("EOM and wrong number of segments");
               end if;
               exit;   -- the loop and terminate the task
            elsif TC_Segment_Total = TC_Expected_Total then
               Report.Failed ("No EOM found");
               exit;
            end if;
         end loop;
      exception
         when others => 
              Report.Failed ("Unexpected Exception in Output_Driver");
      end Output_Driver;



   begin  

      null;

   end; -- encapsulation

   Report.Result;
   
end C954019;
