-- C940013.A
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
--      Check that items queued on a protected entry are handled FIFO and that
--      the  'count attribute of that entry reflects the length of the queue.
-- 
-- TEST DESCRIPTION:
--      Use a small subset of the freeway ramp simulation shown in other
--      tests.  With the timing pulse off (which prevents items from being
--      removed from the queue) queue up a small number of calls.  Start the
--      timing pulse and, at the first execution of the entry code, check the
--      'count attribute. Empty the queue.   Pass the items being removed from
--      the queue to the Ramp_Sensor_01 task; there check that the items are
--      arriving in FIFO order.  Check the final 'count value  
--      
--      Send another batch of items at a rate which will, if the delay timing
--      of the implementation is reasonable, cause the queue length to
--      fluctuate in both directions.   Again check that all items arrive
--      FIFO.  At the end check that the 'count returned to zero reflecting
--      the empty queue.  
--      
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with ImpDef;
with Ada.Calendar;
                
procedure C940013 is

   TC_Failed_1 : Boolean := false;

begin

   Report.Test ("C940013", "Check that queues on protected entries are " &
                           "handled FIFO and that 'count is correct");

   declare  -- encapsulate the test

      function "+" (Left : Ada.Calendar.Time; Right: Duration)
                            return Ada.Calendar.Time renames Ada.Calendar."+";

      -- Weighted load given to each potential problem area and accumulated
      type Load_Factor is range 0..8;
      Clear_Level    : constant Load_Factor := 0;
      Minimum_Level  : constant Load_Factor := 1;
      Moderate_Level : constant Load_Factor := 2;
      Serious_Level  : constant Load_Factor := 4;
      Critical_Level : constant Load_Factor := 6;

      TC_Expected_Passage_Total : constant integer := 624;

      -- For this test give each vehicle an integer ID incremented
      -- by one for each successive vehicle.  In reality this would be
      -- a more complex alpha-numeric ID assigned at pickup time.
      type Vehicle_ID is range 1..5000;
      Next_ID : Vehicle_ID := Vehicle_ID'first;  

      -- In reality this would be about 5 seconds. The default value of 
      -- this constant in the implementation defined package is similar
      -- but could, of course be considerably different - it would not 
      -- affect the test 
      -- 
      Pulse_Time_Delta : duration := ImpDef.Clear_Ready_Queue;


      task Pulse_Task;       -- task to generate a pulse for each ramp

      -- Carrier task. One is created for each vehicle arriving at the ramp
      task type Vehicle is
         entry Get_ID (Input_ID : in Vehicle_ID);
      end Vehicle;      
      type acc_Vehicle is access Vehicle;

      task Ramp_Sensor_01 is
         entry  Accept_Vehicle (Input_ID : in Vehicle_ID);
         entry  TC_First_Three_Handled;
         entry  TC_All_Done;
      end Ramp_Sensor_01;

      protected Pulse_State is 
         procedure Start_Pulse;
         procedure Stop_Pulse;
         function Pulsing return Boolean;
      private
         State : Boolean := false;   -- start test will pulse off
      end Pulse_State;

      protected body Pulse_State is 

         procedure Start_Pulse is
         begin
            State := true;
         end Start_Pulse;

         procedure Stop_Pulse is
         begin 
            State := false;
         end Stop_Pulse;

         function Pulsing return Boolean is
         begin 
            return State;
         end Pulsing;
     
      end Pulse_State;

      --================================================================
      protected Test_Ramp is 
      
         function Meter_in_use_State    return Boolean; 
         procedure Time_Pulse_Received;
         entry Wait_at_Meter;
         procedure TC_Passage (Pass_Point : Integer);
         function TC_Get_Passage_Total return integer;
         function TC_Get_Count return integer;
   
      private

         Release_One_Vehicle : Boolean := false;
         -- For this test have Meter_in_Use already set
         Meter_in_Use        : Boolean := true;

         TC_Wait_at_Meter_First : Boolean := true;
         TC_Entry_Queue_Count   : integer := 0; -- 'count of Wait_at_Meter
         TC_Passage_Total       : integer := 0;
         TC_Pass_Point_WAM      : integer := 23;

      end Test_Ramp;  
      --================================================================
      protected body Test_Ramp is
   
         -- External call for Meter_in_Use
         function Meter_in_Use_State return Boolean is
         begin
            return Meter_in_Use;
         end Meter_in_Use_State;
   
         -- Trace the paths through the various routines by totalling the
         -- weighted call parameters
         procedure TC_Passage (Pass_Point : Integer) is
         begin
            TC_Passage_Total := TC_Passage_Total + Pass_Point;
         end TC_Passage;
   
         -- For the final check of the whole test
         function TC_Get_Passage_Total return integer is
         begin
            return TC_Passage_Total;
         end TC_Get_Passage_Total;
   
         function TC_Get_Count return integer is
         begin
            return TC_Entry_Queue_Count;
         end TC_Get_Count;
   

         -- Here each Vehicle task queues itself awaiting release
         --
         entry Wait_at_Meter when Release_One_Vehicle is
         -- EXAMPLE OF ENTRY WITH BARRIERS AND PERSISTENT SIGNAL
         begin
            --
            TC_Passage ( TC_Pass_Point_WAM );   -- note passage
            -- For this test three vehicles are queued before the first
            -- is released.  If the queueing mechanism is working correctly
            -- the first time we pass through here the entry'count should
            -- reflect this
            if TC_Wait_at_Meter_First then
               if Wait_at_Meter'count /= 2 then
                  TC_Failed_1 := true;
               end if;
               TC_Wait_at_Meter_First := false;
            end if;
            TC_Entry_Queue_Count := Wait_at_Meter'count;  -- note for later
               
            Release_One_Vehicle := false;   -- Consume the signal
            null; -- stub ::: Decrement count of number of vehicles on ramp 
         end Wait_at_Meter;      

   
         procedure Time_Pulse_Received is
            Load : Load_factor := Minimum_Level;   -- for this version of the 
            Freeway_Breakdown : Boolean := false;  -- test, freeway is Minimum
         begin
            -- if broken down, no vehicles are released
            if not Freeway_Breakdown then 
               if Load < Moderate_Level then
                  Release_One_Vehicle := true;
               end if;
               null;    -- stub  ::: If other levels, release every other
                        --           pulse, every third pulse  etc.
            end if;
         end Time_Pulse_Received;
       
      end Test_Ramp;  
      --================================================================

      -- Simulate the arrival of a vehicle at the Ramp_Receiver and the
      -- generation of an accompanying carrier task
      procedure New_Arrival is
         Next_Vehicle_Task: acc_Vehicle := new Vehicle;
         TC_Pass_Point : constant integer := 3; 
      begin
         Next_ID := Next_ID + 1;
         Next_Vehicle_Task.Get_ID(Next_ID);
         Test_Ramp.TC_Passage ( TC_Pass_Point );  -- Note passage through here
         null;
      end New_arrival;


      -- Carrier task. One is created for each vehicle arriving at the ramp
      task body Vehicle is
         This_ID : Vehicle_ID;
         TC_Pass_Point_2 : constant integer := 21;
      begin
         accept Get_ID (Input_ID : in Vehicle_ID) do 
            This_ID := Input_ID;
         end Get_ID;

         if Test_Ramp.Meter_in_Use_State then  
            Test_Ramp.TC_Passage ( TC_Pass_Point_2 );  -- note passage
            null;  -- stub::: Increment count of number of vehicles on ramp 
            Test_Ramp.Wait_at_Meter;      -- Queue on the meter entry
         end if;

         -- Call to the first in the series of the Ramp_Sensors
         --     this "passes" the vehicle from one sensor to the next
         --     Each sensor will requeue the call to the next thus this
         --     rendezvous will only be completed as the vehicle is released
         --     by the last sensor on the ramp.
         Ramp_Sensor_01.Accept_Vehicle (This_ID);
      exception
         when others => 
               Report.Failed ("Unexpected exception in Vehicle Task");
      end Vehicle;

      task body Ramp_Sensor_01 is
         TC_Pass_Point : constant integer := 31; 
         This_ID    : Vehicle_ID;
         TC_Last_ID : Vehicle_ID := Vehicle_ID'first;
      begin
         loop
            select
               accept Accept_Vehicle (Input_ID : in Vehicle_ID) do 
                  null;   -- stub:::: match up with next Real-Time notification
                          -- from the sensor.  Requeue to next ramp sensor
                  This_ID := Input_ID;
                  
                  -- The following is all Test_Control code
                  Test_Ramp.TC_Passage ( TC_Pass_Point );  -- note passage
                  -- The items arrive in the order they are taken from
                  -- the Wait_at_Meter entry queue
                  if ( This_ID - TC_Last_ID ) /= 1 then
                     -- The tasks are being queued (or unqueued) in the
                     -- wrong order
                     Report.Failed
                              ("Queueing on the Wait_at_Meter queue failed");
                  end if;
                  TC_Last_ID := This_ID;    -- for the next check
                  if TC_Last_ID = 4 then
                     -- rendezvous with the test driver
                     accept TC_First_Three_Handled;
                  elsif TC_Last_ID = 9 then
                     -- rendezvous with the test driver
                     accept TC_All_Done;
                  end if;
               end Accept_Vehicle;
            or
               terminate;
            end select;
         end loop;
      exception
         when others => 
               Report.Failed ("Unexpected exception in Ramp_Sensor_01");
      end Ramp_Sensor_01;


      -- Task transmits a synchronizing "pulse" to all ramps
      --
      task body Pulse_Task is
         Pulse_Time : Ada.Calendar.Time;
      begin
         While not Pulse_State.Pulsing loop
            -- Starts up in the quiescent state
            delay ImpDef.Minimum_Task_Switch; 
         end loop;
         Pulse_Time := Ada.Calendar.Clock;
         While Pulse_State.Pulsing loop
            delay until Pulse_Time;  
            Test_Ramp. Time_Pulse_Received;   -- Transmit pulse to test_ramp
            -- ::::::::::  and to all the other ramps
            Pulse_Time := Pulse_Time + Pulse_Time_Delta; -- calculate next
         end loop;
      exception
         when others => 
               Report.Failed ("Unexpected exception in Pulse_Task");
      end Pulse_Task;


   begin -- declare

      -- Test driver.  This is ALL test control code

      -- Arrange to queue three vehicles on the Wait_at_Meter queue.  The 
      -- timing pulse is quiescent so the queue will build
      for i in 1..3 loop
         New_Arrival; 
      end loop;

      delay Pulse_Time_Delta;  -- ensure all is settled

      Pulse_State.Start_Pulse;     -- Start the timing pulse, the queue will 
                                   -- be serviced
      
      -- wait here until the first three are complete
      Ramp_Sensor_01.TC_First_Three_Handled;

      if Test_Ramp.TC_Get_Count /= 0 then
         Report.Failed ("Intermediate Wait_at_Entry'count is incorrect");
      end if;

      -- generate new arrivals at a rate that will make the queue increase
      -- and decrease "randomly"
      for i in 1..5 loop
         New_Arrival; 
         delay Pulse_Time_Delta/2;
      end loop;

      -- wait here till all have been handled
      Ramp_Sensor_01.TC_All_Done;

      if Test_Ramp.TC_Get_Count /= 0 then
         Report.Failed ("Final Wait_at_Entry'count is incorrect");
      end if;

      Pulse_State.Stop_Pulse;       -- finish test


      if TC_Expected_Passage_Total /= Test_Ramp.TC_Get_Passage_Total then
         Report.Failed ("Unexpected paths taken");
      end if;


   end; -- declare

   if TC_Failed_1 then 
      Report.Failed ("Wait_at_Meter'count incorrect");
   end if;

   Report.Result;

end C940013;
