-- C960001.A 
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
--      Confirm that a simple Delay Until statement is performed.  Check
--      that the delay does not complete before the requested time and that it
--      does complete thereafter
--
-- TEST DESCRIPTION:
--      Simulate a task that sends a "pulse" at regular intervals.  The Delay
--      Until statement is used to avoid accumulated drift.  For the
--      test, we expect the delay to return very close to the requested time;
--      we use an additional Pulse_Time_Delta for the limit.  The test
--      driver (main) artificially limits the number of iterations by setting
--      the Stop_Pulse Boolean after a small number.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      15 Nov 95   SAIC    Fixed global variable problem for ACVC 2.0.1
--
--!

with Report;
with Ada.Calendar;
with ImpDef;
         
procedure C960001 is

begin

   Report.Test ("C960001", "Simple Delay Until");

   declare  -- To get the Report.Result after all has completed

      function "+" (Left : Ada.Calendar.Time; Right: Duration)
                            return Ada.Calendar.Time renames Ada.Calendar."+";
      function "<" (Left, Right : Ada.Calendar.Time)
                            return Boolean       renames Ada.Calendar."<";
      function ">" (Left, Right : Ada.Calendar.Time)
                            return Boolean       renames Ada.Calendar.">";

      TC_Loop_Count : integer range 0..4 := 0; 
     

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
 
      task Pulse_Task is 
         entry Trigger;
      end Pulse_Task;
   

      -- Task to synchronize all qualified receivers.  
      -- The entry Trigger starts the synchronization; Control.Stop 
      -- becoming true terminates the task.
      --
      task body Pulse_Task is

         Pulse_Time       : Ada.Calendar.Time;

         Pulse_Time_Delta : duration :=  ImpDef.Clear_Ready_Queue;

         TC_Last_Time : Ada.Calendar.Time;
         TC_Current   : Ada.Calendar.Time;


         -- This routine transmits a synchronizing "pulse" to
         -- all receivers
         procedure Pulse is
         begin
            null;  -- Stub
            Report.Comment (".......PULSE........");
         end Pulse;
         
      begin
         accept Trigger;

         Pulse_Time   := Ada.Calendar.Clock + Pulse_Time_Delta; 
         TC_Last_Time := Pulse_Time;

         while not Control.Stop loop
            delay until Pulse_Time;
            Pulse;

            -- Calculate time for next pulse.  Note: this is based on the
            -- last pulse time, not the time we returned from the delay
            --
            Pulse_Time := Pulse_Time + Pulse_Time_Delta;
            
            -- Test Control:
            TC_Current := Ada.Calendar.Clock;
            if TC_Current < TC_Last_Time then
               Report.Failed ("Delay expired before requested time");
            end if;
            if TC_Current > Pulse_Time then
               Report.Failed ("Delay too long");
            end if;               
            TC_Last_Time := Pulse_Time;
            TC_Loop_Count := TC_Loop_Count +1;
         end loop; 

      exception
         when others => 
               Report.Failed ("Unexpected exception in Pulse_Task");
      end Pulse_Task;



   begin -- declare

      Pulse_Task.Trigger;      -- Start test                            

      -- Artificially limit the number of iterations
      while TC_Loop_Count < 3 loop
         delay ImpDef.Minimum_Task_Switch;
      end loop;
      --
      Control.Stop_Now;      -- End test

   end; -- declare

   Report.Result;

end C960001;
