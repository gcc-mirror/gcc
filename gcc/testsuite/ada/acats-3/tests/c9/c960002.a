-- C960002.A 
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
--      Check that the simple "delay until" when the request time is "now" and
--      also some time already in the past is obeyed and returns immediately
--
-- TEST DESCRIPTION:
--      Simulate a task that sends a "pulse" at regular intervals.  The Delay
--      Until statement is used to avoid accumulated drift.  In this test
--      three simple situations simulating the start of drift are used:  the
--      next pulse being called for at the normal time, the next pulse being
--      called for at exactly the current time and then at some time which has
--      already past.  We assume the delay is within a While Loop and, to
--      simplify the test, we "unfold" the While Loop and  execute the Delays
--      in a serial fashion.  This loop is shown in test C960001.
--      It is not possible to test the actual immediacy of the expiration. We
--      can only check that it returns in a "reasonable" time.  In this case
--      we check that it expires before the next "pulse" should have been 
--      issued.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with ImpDef;

with Ada.Calendar;
with System;                                       
         
procedure C960002 is

begin

   Report.Test ("C960002", "Simple Delay Until with requested time being" &
                        " ""now"" and time already in the past");

   declare  -- To get the Report.Result after all has completed

      function "+" (Left : Ada.Calendar.Time; Right: Duration)
                            return Ada.Calendar.Time renames Ada.Calendar."+";
      function "-" (Left : Ada.Calendar.Time; Right: Duration)
                            return Ada.Calendar.Time renames Ada.Calendar."-";
      function "-" (Left, Right : Ada.Calendar.Time)
                            return duration      renames Ada.Calendar."-";
      function ">" (Left, Right : Ada.Calendar.Time)
                            return Boolean       renames Ada.Calendar.">";

      
      task Pulse_Task is 
         entry Trigger;
      end Pulse_Task;
   

      -- Task to synchronize all qualified receivers.  
      -- The entry Trigger starts the synchronization. 
      --
      task body Pulse_Task is
         Pulse_Time       : Ada.Calendar.Time;
         Pulse_Time_Delta : constant duration := ImpDef.Clear_Ready_Queue; 



         TC_Time_Back     : Ada.Calendar.Time;


         -- This routine transmits a synchronizing "pulse" to
         -- all receivers
         procedure Pulse is
         begin
            null;  -- Stub
            Report.Comment (".......PULSE........");      
         end Pulse;
         
      begin
         accept Trigger;
         Pulse;
         ---------------
         -- normal calculation for "next"
         Pulse_Time := Ada.Calendar.Clock + Pulse_Time_Delta; 

         -- TC:  unfold the "while" loop in C960001.  Four passes through 
         -- the loop are shown

            delay until Pulse_Time;  

            Pulse;
            ---------------
            -- TC: the normal calculation for "next" would be
            -- Pulse_Time := Pulse_Time + Pulse_Time_Delta;
            -- Instead of this normal pulse time calculation simulate
            -- the new pulse time to be exactly "now"  (or, as exactly as 
            -- we can)
            Pulse_Time := Ada.Calendar.Clock; 
            delay until  Ada.Calendar.Clock; 

            TC_Time_Back := Ada.Calendar.Clock;

            -- Now check for reasonableness
            if TC_Time_Back > Pulse_Time + Pulse_Time_Delta then
               Report.Failed 
                     ("""Now"" delayed for more than Pulse_Time_Delta - A");
            end if;
            Pulse;
            ---------------
            -- normal calculation for "next" would be
            Pulse_Time := Pulse_Time + Pulse_Time_Delta;

            -- TC: Instead of this, simulate the new calculated pulse time
            -- being already past
            Pulse_Time := Ada.Calendar.Clock - System.Tick;
            delay until Pulse_Time;

            TC_Time_Back := Ada.Calendar.Clock;

            -- Now check for reasonableness
            if TC_Time_Back > Pulse_Time + Pulse_Time_Delta then
               Report.Failed 
                     ("""Now"" delayed for more than Pulse_Time_Delta - B");
            end if;
            Pulse;
            ---------------
            -- normal calculation for "next"
            Pulse_Time := Pulse_Time + Pulse_Time_Delta;
            -- Now simulate getting back into synch
            delay until Pulse_Time;
            Pulse;
            ---------------
         -- This would be the end of the "while" loop

      exception
         when others => 
               Report.Failed ("Unexpected exception in Pulse_Task");
      end Pulse_Task;



   begin -- declare

      Pulse_Task.Trigger;      -- Start test                            

   end; -- declare

   Report.Result;

end C960002;
