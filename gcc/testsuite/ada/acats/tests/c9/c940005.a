-- C940005.A 
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
--      Check that the body of a protected function can have internal calls
--      to other protected functions and that the body of a protected
--      procedure can have internal calls to protected procedures and to 
--      protected functions.
--
-- TEST DESCRIPTION:
--      Simulate a meter at a freeway on-ramp which, when real-time sensors
--      determine that the freeway is becoming saturated, triggers stop lights
--      which control the access of vehicles to prevent further saturation. 
--      Each on-ramp is represented by a protected object - in this case only
--      one is shown (Test_Ramp).  The routines to sample and alter the states
--      of the various sensors, to queue the vehicles on the meter and to
--      release them are all part of the  protected object and can be shared
--      by various tasks. Apart from the function/procedure tests this example
--      has a mix of other tasking features. 
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      13 Nov 95   SAIC    Updated and fixed bugs ACVC 2.0.1
--
--!


with Report;
with ImpDef;
with Ada.Calendar;
                
procedure C940005 is

begin

   Report.Test ("C940005", "Check internal calls of protected functions" &
                        " and procedures");

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

      -- Weighted loads given to each  Sample Point (pure weights, not levels)
      Local_Overload_wt         : constant Load_Factor := 1;
      Next_Ramp_in_Overload_wt  : constant Load_Factor := 1;
      Ramp_Junction_in_Overload_wt : constant Load_Factor :=2; --higher wght
      -- ::::  other weighted loads
          
      TC_Multiplier : integer := 1;  -- changed half way through
      TC_Expected_Passage_Total : constant integer := 486;

      -- This is the time between synchronizing pulses to the ramps.
      -- In reality one would expect a time of 5 to 10 seconds.  In
      -- the interests of speeding up the test suite a shorter time
      -- is used
      Pulse_Time_Delta : constant duration := ImpDef.Switch_To_New_Task;

      -- control over stopping tasks
      protected Control is
         procedure Stop_Now;
         function Stop return Boolean;
      private
         Halt : Boolean := False;
      end Control;

      protected body Control is
         procedure Stop_Now is
         begin
            Halt := True;
         end Stop_Now;

         function Stop return Boolean is
         begin
            return Halt;
         end Stop;
      end Control;

      task Pulse_Task;       -- task to generate a pulse for each ramp

      -- Carrier task. One is created for each vehicle arriving at the ramp
      task type Vehicle;      
      type acc_Vehicle is access Vehicle;

      --================================================================
      protected Test_Ramp is
         function Next_Ramp_in_Overload return Load_Factor;
         function Local_Overload        return Load_Factor;
         function Freeway_Overload      return Load_Factor;
         function Freeway_Breakdown     return Boolean;
         function Meter_in_use_State    return Boolean; 
         procedure Set_Local_Overload;
         procedure Add_Meter_Queue;
         procedure Subtract_Meter_Queue;
         procedure Time_Pulse_Received;
         entry Wait_at_Meter;
         procedure TC_Passage (Pass_Point : Integer);
         function TC_Get_Passage_Total return integer;
         -- ::::::::: many routines are not shown (for example none of the
         --            clears, none of the real-time-sensor handlers)
   
      private

         Release_One_Vehicle : Boolean := false;
         Meter_in_Use        : Boolean := false;
         Fwy_Break_State     : Boolean := false;
               
   
         Ramp_Count : integer range 0..20 := 0;
         Ramp_Count_Threshold : integer := 15;
   
         -- Current state of the various Sample Points
         Local_State     : Load_Factor := Clear_Level;
         Next_Ramp_State : Load_Factor := Clear_Level;
            -- ::::  other Sample Point states not shown
   
         TC_Passage_Total : integer := 0;
      end Test_Ramp;  
      --================================================================
      protected body Test_Ramp is
   
            procedure Start_Meter is
            begin
               Meter_in_Use := True;
               null;  -- stub  :::: trigger the metering hardware
            end Start_Meter;
         
         -- External call for Meter_in_Use
         function Meter_in_Use_State return Boolean is
         begin
            return Meter_in_Use;
         end Meter_in_Use_State;
   
         -- Trace the paths through the various routines by totaling the
         -- weighted call parameters
         procedure TC_Passage (Pass_Point : Integer) is
         begin
            TC_Passage_Total := TC_Passage_Total+(Pass_Point*TC_Multiplier);
         end TC_Passage;
   
         -- For the final check of the whole test
         function TC_Get_Passage_Total return integer is
         begin
            return TC_Passage_Total;
         end TC_Get_Passage_Total;
   
         -- These Set/Clear routines are triggered by real-time sensors that
         -- reflect traffic state
         procedure Set_Local_Overload is
         begin
            Local_State := Local_Overload_wt;
            if not Meter_in_Use then
               Start_Meter;   -- LOCAL INTERNAL PROCEDURE FROM PROCEDURE
            end if;
         end Set_Local_Overload;
   
         --::::: Set/Clear routines for all the other sensors not shown

         function Local_Overload return Load_Factor is
         begin
            return Local_State;
         end Local_Overload;
   
         function Next_Ramp_in_Overload return Load_Factor is
         begin
            return Next_Ramp_State;
         end Next_Ramp_in_Overload;
   
         -- ::::::::  other overload factor states not shown
   
         -- return the summation of all the load factors
         function Freeway_Overload return Load_Factor is
         begin
            return    Local_Overload                    -- EACH IS A CALL OF A
                      -- + :::: others                  -- FUNCTION FROM WITHIN
                      + Next_Ramp_in_Overload;          -- A FUNCTION
         end Freeway_Overload;

         -- Freeway Breakdown is defined as traffic moving < 5mph
         function Freeway_Breakdown return Boolean is
         begin
            return Fwy_Break_State;
         end Freeway_Breakdown;
   
         -- Keep count of vehicles currently on meter queue - we can't use
         -- the 'count because we need the outcall trigger
         procedure Add_Meter_Queue is
            TC_Pass_Point : constant integer := 22;
         begin
            Ramp_Count := Ramp_Count + 1;  
            TC_Passage ( TC_Pass_Point );  -- note passage through here
            if Ramp_Count > Ramp_Count_Threshold then
               null;  -- :::: stub, trigger surface street notification
            end if;
         end Add_Meter_Queue;  
         --
         procedure Subtract_Meter_Queue is
            TC_Pass_Point : constant integer := 24;
         begin
            Ramp_Count := Ramp_Count - 1;  
            TC_Passage ( TC_Pass_Point );  -- note passage through here
         end Subtract_Meter_Queue;  
   
         -- Here each Vehicle task queues itself awaiting release
         entry Wait_at_Meter when Release_One_Vehicle is
         -- EXAMPLE OF ENTRY WITH BARRIERS AND PERSISTENT SIGNAL
            TC_Pass_Point : constant integer := 23;
         begin
            TC_Passage ( TC_Pass_Point );   -- note passage through here
            Release_One_Vehicle := false;   -- Consume the signal
            -- Decrement number of vehicles on ramp 
            Subtract_Meter_Queue;  -- CALL PROCEDURE FROM WITHIN ENTRY BODY
         end Wait_at_Meter;      
   
   
         procedure Time_Pulse_Received is
            Load : Load_factor := Freeway_Overload; -- CALL MULTILEVEL
                                                    -- FUNCTION 
                                                    -- FROM WITHIN PROCEDURE
         begin
            -- if broken down, no vehicles are released
            if not Freeway_Breakdown then    -- CALL FUNCTION FROM A PROCEDURE
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
         Test_Ramp.TC_Passage ( TC_Pass_Point );  -- Note passage through here
         null;
      end New_arrival;


      -- Carrier task. One is created for each vehicle arriving at the ramp
      task body Vehicle is
         TC_Pass_point   : constant integer :=  1;
         TC_Pass_Point_2 : constant integer := 21;
         TC_Pass_Point_3 : constant integer :=  2;
      begin
         Test_Ramp.TC_Passage ( TC_Pass_Point );  -- note passage through here
         if Test_Ramp.Meter_in_Use_State then  
            Test_Ramp.TC_Passage ( TC_Pass_Point_2 );  -- note passage
            -- Increment count of number of vehicles on ramp 
            Test_Ramp.Add_Meter_Queue;    -- CALL a protected PROCEDURE
                                          -- which is also called from within
            -- enter the meter queue
            Test_Ramp.Wait_at_Meter;      -- CALL a protected ENTRY   
         end if;
         Test_Ramp.TC_Passage ( TC_Pass_Point_3 );  -- note passage thru here
         null;  --:::: call to the first in the series of the Ramp_Sensors
                --     this "passes" the vehicle from one sensor to the next
      exception
         when others => 
               Report.Failed ("Unexpected exception in Vehicle Task");
      end Vehicle;


      -- Task transmits a synchronizing "pulse" to all ramps
      --
      task body Pulse_Task is
         Pulse_Time : Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         While not Control.Stop loop
            delay until Pulse_Time;  
            Test_Ramp.Time_Pulse_Received;  -- causes INTERNAL CALLS
            -- ::::::::::  and to all the others
            Pulse_Time := Pulse_Time + Pulse_Time_Delta; -- calculate next
         end loop;
      exception
         when others => 
               Report.Failed ("Unexpected exception in Pulse_Task");
      end Pulse_Task;


   begin -- declare

      -- Test driver.  This is ALL test control code

      -- First simulate calls to the protected functions and procedures
      -- from without the protected object
      --
      -- CALL FUNCTIONS
      if Test_Ramp.Local_Overload /= Clear_Level then
         Report.Failed ("External Call to Local_Overload incorrect");
      end if;
      if Test_Ramp.Next_Ramp_in_Overload /= Clear_Level then
         Report.Failed ("External Call to Next_Ramp_in_Overload incorrect");
      end if;
      if Test_Ramp.Freeway_Overload /= Clear_Level then
         Report.Failed ("External Call to Freeway_Overload incorrect");
      end if;

      -- Now Simulate the arrival of a vehicle to verify path through test
      New_Arrival; 
      delay Pulse_Time_Delta*2;  -- allow it to pass through the complex

      TC_Multiplier := 5;  -- change the weights for the paths for the next
                           -- part of the test
      
      -- Simulate a real-time sensor reporting overload
      Test_Ramp.Set_Local_Overload;  -- CALL A PROCEDURE  (and change levels)

      -- CALL FUNCTIONS again
      if Test_Ramp.Local_Overload /= Minimum_Level then
         Report.Failed ("External Call to Local_Overload incorrect - 2");
      end if;
      if Test_Ramp.Freeway_Overload /= Minimum_Level then
         Report.Failed ("External Call to Freeway_Overload incorrect -2");
      end if;

      -- Now Simulate the arrival of another vehicle again causing
      -- INTERNAL CALLS but following different paths (queuing on the 
      -- meter etc.)
      New_Arrival; 
      delay Pulse_Time_Delta*2;  -- allow it to pass through the complex

      Control.Stop_Now;  -- finish test

      if TC_Expected_Passage_Total /= Test_Ramp.TC_Get_Passage_Total then
         Report.Failed ("Unexpected paths taken");
      end if;

   end; -- declare

   Report.Result;

end C940005;
