-- C954022.A
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
--      In an entry body requeue the call to the same entry.  Check that the
--      items go to the right queue and that they are placed back on the end 
--      of the queue
--
-- TEST DESCRIPTION:
--      Simulate part of a message handling application where the messages are
--      composed of several segments.  The sequence of the segments within the
--      message is specified by Seg_Sequence_No.   The segments are handled by
--      different tasks and finally forwarded to an output driver.  The
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
--      the queue.  Having cycled once through it no longer verifies the
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
--      07 Nov 95   SAIC    ACVC 2.0.1
--
--!

with Report;
with ImpDef;

procedure C954022 is

   -- These global Booleans are set when failure conditions inside Protected
   -- objects are encountered.  Report.Failed cannot be called within
   -- the object or a Bounded Error would occur
   --
   TC_Failed_1 : Boolean := false;
   TC_Failed_2 : Boolean := false;
   TC_Failed_3 : Boolean := false;

begin


   Report.Test ("C954022", "Check Requeue to the same Protected Entry");
   
   declare  -- encapsulate the test

      type Segment_Sequence is range 1..8;
      Header : constant Segment_Sequence := Segment_Sequence'first;

      type Message_Segment is record
         ID              : integer;            -- Message ID
         Seg_Sequence_No : Segment_Sequence;   -- Within the message
         Segs_In_Message : integer;            -- Total segs this message
         EOM             : Boolean := false;   -- true for final msg segment
         Alpha           : string (1..128);
      end record;
      type acc_Message_Segment is access Message_Segment;

      task TC_Simulate_Arrival;
   
      task type Carrier_Task is
         entry Input ( Segment : acc_Message_Segment );
      end Carrier_Task;
      type acc_Carrier_Task is access Carrier_Task;
   
      protected Sequencer is
         function  TC_Arrivals return integer;
         entry Input          ( Segment : acc_Message_Segment );
         entry Ordering_Queue ( Segment : acc_Message_Segment );
      private
         Number_of_Segments_Arrived  : integer := 0;
         Number_of_Segments_Expected : integer := 0;
         Next_Needed : Segment_Sequence := Header;
         All_Segments_Arrived : Boolean := false;
         Seen_EOM             : Boolean := false;

         TC_First_Cycle       : Boolean := true;
         TC_Expected_Sequence : Segment_Sequence := Header+2;

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
                     Next_Segment.Segs_In_Message := 3;
                     Next_Segment.EOM := true;
                  elsif i = 2 then
                     -- Wait for the first segment to arrive at the Sequencer
                     -- before "sending" the second
                     while Sequencer.TC_Arrivals < 1 loop
                        delay ImpDef.Minimum_Task_Switch;
                     end loop;
                     -- Build the segment
                     Next_Segment.Seg_Sequence_No := Header +1;
                  else
                     -- Wait for the second segment to arrive at the Sequencer
                     -- before "sending" the third
                     while Sequencer.TC_Arrivals < 2 loop
                        delay ImpDef.Minimum_Task_Switch;
                     end loop;                  
                     -- Build the segment. The last segment (in order) to
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
         Sequencer.Input ( This_Segment );
      exception
         when others => 
              Report.Failed ("Unexpected Exception in Carrier_Task");
      end Carrier_Task;
   
      -- Store segments on the Ordering_Queue then deliver them in the correct
      -- sequence to the Output_Driver.
      --
      protected body Sequencer is

         function  TC_Arrivals return integer is
         begin
            return Number_of_Segments_Arrived;
         end TC_Arrivals;

         
         -- Segments arriving at the Input queue are counted and checked
         -- against the total number of segments for the message.  They
         -- are requeued onto the ordering queue where they are held until
         -- all the segments have arrived.
         entry Input ( Segment : acc_Message_Segment ) when true is
         begin
            -- check for EOM, if so get the number of segments in the message
            -- Note: in this portion of code no attempt is made to address
            -- reset for new message , end conditions, missing segments, 
            -- segments of a different message etc.
            Number_of_Segments_Arrived := Number_of_Segments_Arrived + 1;
            if Segment.EOM then
               Number_of_Segments_Expected := Segment.Segs_In_Message;
               Seen_EOM := true;
            end if;

            if Seen_EOM then
               if Number_of_Segments_Arrived = Number_of_Segments_Expected then
                  -- This is the last segment for this message
                  All_Segments_Arrived := true;    -- clear the barrier
               end if;
            end if;

            requeue Ordering_Queue;

            -- At this exit point the entry queue barriers are evaluated

         end Input;


         entry Ordering_Queue ( Segment : acc_Message_Segment )
                                                  when All_Segments_Arrived is
            begin

            --=====================================================
            -- This part is all Test_Control code

            if TC_First_Cycle then 
               -- Check the order of the original three
               if Segment.Seg_Sequence_No /= TC_Expected_Sequence then
                  -- The segments are not being pulled off in the 
                  -- expected sequence.  This could occur if the 
                  -- requeue is not putting them back on the end.
                  TC_Failed_3 := true;
               end if; -- sequence check
               -- Decrement the expected sequence
               if TC_Expected_Sequence /= Header then
                  TC_Expected_Sequence := TC_Expected_Sequence - 1;
               else 
                  TC_First_Cycle := false; -- This is the Header - the
                                           -- first two segments are
                                           -- back on the queue
               end if; -- decrementing
            end if; -- first cycle 
            --=====================================================

            -- And this is the Application code
            if Segment.Seg_Sequence_No = Next_Needed then
               if Segment.EOM then
                  Next_Needed := Header;  -- reset for next message
                  -- :: other resets not shown
               else
                  Next_Needed := Next_Needed + 1;
               end if;
               requeue Output_Driver.Input  with abort;
               -- set to Report Failed - Requeue did not complete entry body
               TC_Failed_1 := true;
            else
               -- Not the next needed - put it back on the queue
               --    NOTE: here we are requeueing to the same entry
               requeue Sequencer.Ordering_Queue;
               -- set to Report Failed - Requeue did not complete entry body
               TC_Failed_2 := true;
            end if;
         end Ordering_Queue;
      end Sequencer;
   

      task body Output_Driver is
         This_Segment : acc_Message_Segment := new Message_Segment;

         TC_Expected_Sequence : Segment_Sequence := Segment_Sequence'first;
         TC_Segment_Total : integer := 0;
         TC_Expected_Total : integer := 3;
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

   if TC_Failed_1 then
      Report.Failed ("Requeue did not complete entry body - 1");
   end if;

   if TC_Failed_2 then
      Report.Failed ("Requeue did not complete entry body - 2");
   end if;

   if TC_Failed_3 then
      Report.Failed ("Sequencer: Segment out of sequence");
   end if;

   Report.Result;
   
end C954022;
