-- C951001.A
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
--      Check that two procedures in a protected object will not be 
--      executed concurrently.
--
-- TEST DESCRIPTION:
--      A very simple example of two tasks calling two procedures in the same 
--      protected object is used.  Test control code has been added to the
--      procedures such that, whichever gets called first executes a lengthy
--      calculation giving sufficient time (on a multiprocessor or a
--      time-slicing machine) for the other task to get control and call the
--      other procedure.  The control code verifies that entry to the second
--      routine is postponed until the first is complete.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with ImpDef;

procedure C951001 is

   protected Ramp_31 is

      procedure Add_Meter_Queue;
      procedure Subtract_Meter_Queue;
      function  TC_Failed return Boolean;
   
   private

      Ramp_Count : integer range 0..20 := 4;  -- Start test with some
                                              -- vehicles on the ramp
   
      TC_Add_Started       : Boolean := false;
      TC_Subtract_Started  : Boolean := false;
      TC_Add_Finished      : Boolean := false;
      TC_Subtract_Finished : Boolean := false;
      TC_Concurrent_Running: Boolean := false;
      
   end Ramp_31;

   
   protected body Ramp_31 is

      function TC_Failed return Boolean is
      begin
         -- this indicator will have been set true if any instance
         -- of concurrent running has been proved
         return TC_Concurrent_Running;
      end TC_Failed;


      procedure Add_Meter_Queue is
      begin
         --==================================================
         -- This section is all Test_Control code
         TC_Add_Started := true;
         if TC_Subtract_Started then 
            if not TC_Subtract_Finished then
               TC_Concurrent_Running := true;
            end if;
         else
            -- Subtract has not started. 
            -- Execute a lengthy routine to give it a chance to do so
            ImpDef.Exceed_Time_Slice;

            if TC_Subtract_Started then 
               -- Subtract was able to start so we have concurrent 
               -- running and the test has failed
               TC_Concurrent_Running := true;
            end if;
         end if;
         TC_Add_Finished := true;
         --==================================================
         Ramp_Count := Ramp_Count + 1;
      end Add_Meter_Queue;

      procedure Subtract_Meter_Queue is
      begin
         --==================================================
         -- This section is all Test_Control code
         TC_Subtract_Started := true;
         if TC_Add_Started then 
            if not TC_Add_Finished then
               -- We already have concurrent running
               TC_Concurrent_Running := true;
            end if;
         else
            -- Add has not started. 
            -- Execute a lengthy routine to give it a chance to do so
            ImpDef.Exceed_Time_Slice;

            if TC_Add_Started then 
               -- Add was able to start so we have concurrent 
               -- running and the test has failed
               TC_Concurrent_Running := true;
            end if;
         end if;
         TC_Subtract_Finished := true;
         --==================================================
         Ramp_Count := Ramp_Count - 1;
      end Subtract_Meter_Queue;
   
   end Ramp_31;

begin

   Report.Test ("C951001", "Check that two procedures in a protected" &
                           " object will not be executed concurrently");

   declare -- encapsulate the test

      task Vehicle_1;
      task Vehicle_2;


      -- Vehicle_1 and Vehicle_2 are simulations of Instances of the task
      -- of type Vehicle in different stages of execution

      task body Vehicle_1 is
      begin
         null;  -- ::::: stub.  preparation code
         
         -- Add to the count of vehicles on the queue   
         Ramp_31.Add_Meter_Queue;

         null;  -- ::::: stub:  wait at the meter then pass to first sensor

         -- Reduce the count of vehicles on the queue
         null;  -- ::::: stub: Ramp_31.Subtract_Meter_Queue
      exception
         when others =>
               Report.Failed ("Unexpected Exception in Vehicle_1 task");
      end Vehicle_1;


      task body Vehicle_2 is
      begin
         null;  -- ::::: stub.  preparation code
         
         -- Add to the count of vehicles on the queue   
         null;  -- ::::: stub Ramp_31.Add_Meter_Queue;  

         null;  -- ::::: stub:  wait at the meter then pass to first sensor

         -- Reduce the count of vehicles on the queue
         Ramp_31.Subtract_Meter_Queue;
      exception
         when others =>
               Report.Failed ("Unexpected Exception in Vehicle_2 task");
      end Vehicle_2;

  

   begin
      null;
   end;   -- encapsulation

   if Ramp_31.TC_Failed then
      Report.Failed ("Concurrent Running detected");
   end if;

   Report.Result;

end C951001;
