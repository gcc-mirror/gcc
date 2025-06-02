-- C940010.A 
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
--      Check that if an exception is raised during the execution of an 
--      entry body it is propagated back to the caller
--
-- TEST DESCRIPTION:
--      Use a small fragment of code from the simulation of a freeway meter
--      used in c940007. Create three individual tasks which will be queued on
--      the entry as the barrier is set.  Release them one at a time.  A
--      procedure which is called within the entry has been modified for this
--      test to raise a different exception for each pass through.  Check that
--      all expected exceptions are raised and propagated.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


with Report;
with ImpDef;
                
procedure C940010 is

    TC_Failed_1 : Boolean := false; 

begin

   Report.Test ("C940010", "Check that an exception raised in an entry " &
                        "body is propagated back to the caller");

   declare  -- encapsulate the test

      TC_Defined_Error : Exception;    -- User defined exception
      TC_Expected_Passage_Total : constant integer := 669;
      TC_Int                    : constant integer := 5;

      -- Carrier tasks. One is created for each vehicle arriving at each ramp
      task type Vehicle_31;            -- For Ramp_31
      type acc_Vehicle_31 is access Vehicle_31;


      --================================================================
      protected Ramp_31 is

         function Meter_in_Use_State return Boolean;
         procedure Add_Meter_Queue;
         procedure Subtract_Meter_Queue;
         entry Wait_at_Meter;
         procedure Pulse;
         --
         procedure TC_Passage (Pass_Point : Integer);
         function TC_Get_Passage_Total return integer;
         function TC_Get_Current_Exception return integer;

      private

         Release_One_Vehicle : Boolean := false;
         Meter_in_Use        : Boolean := true;  -- TC: set true for this test
         --
         TC_Multiplier       : integer := 1;
         TC_Passage_Total    : integer := 0;
         -- Use this to cycle through the required exceptions
         TC_Current_Exception : integer range 0..3 := 0;

      end Ramp_31;  
      --================================================================
      protected body Ramp_31 is
               
   
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

         function TC_Get_Current_Exception return integer is
         begin 
            return TC_Current_Exception;
         end TC_Get_Current_Exception;

   
         -----------------

         function Meter_in_Use_State return Boolean is
         begin
            return Meter_in_Use;
         end Meter_in_Use_State;
       
         -- Simulate the effects of the regular signal pulse 
         procedure Pulse is
         begin
            Release_one_Vehicle := true;
         end Pulse;

         -- Keep count of vehicles currently on meter queue - we can't use
         -- the 'count because we need the outcall trigger
         procedure Add_Meter_Queue is
         begin
            null;    --::: stub
         end Add_Meter_Queue;  

         -- TC: This routine has been modified to raise the required 
         --     exceptions
         procedure Subtract_Meter_Queue is
            TC_Pass_Point1 : constant integer := 10;
            TC_Pass_Point2 : constant integer := 20;
            TC_Pass_Point3 : constant integer := 30;
            TC_Pass_Point9 : constant integer := 1000;  -- error
         begin
            -- Cycle through the required exceptions, one per call
            TC_Current_Exception := TC_Current_Exception + 1;
            case TC_Current_Exception is
               when 1 => 
                     TC_Passage (TC_Pass_Point1);  -- note passage through here
                     raise Storage_Error;    -- PREDEFINED EXCEPTION
               when 2 => 
                     TC_Passage (TC_Pass_Point2);  -- note passage through here
                     raise TC_Defined_Error;    -- USER DEFINED EXCEPTION
               when 3 => 
                     TC_Passage (TC_Pass_Point3);  -- note passage through here
                     -- RUN TIME EXCEPTION (Constraint_Error)
                     -- Add the value 3 to 5 then try to assign it to an object
                     -- whose range is 0..3  - this causes the exception.
                     -- Disguise the values which cause the Constraint_Error
                     -- so that the optimizer will not eliminate this code
                     --    Note: the variable is checked at the end to ensure
                     --    that the actual assignment is attempted.  Also note
                     --    the value remains at 3 as the assignment does not
                     --    take place.  This is the value that is checked at
                     --    the end of the test.
                     -- Otherwise the optimizer could decide that the result 
                     -- of the assignment was not used so why bother to do it?
                     TC_Current_Exception := 
                               Report.Ident_Int (TC_Current_Exception) + 
                               Report.Ident_Int (TC_Int);
               when others =>
                     -- Set flag for Report.Failed which cannot be called from
                     -- within a Protected Object 
                     TC_Failed_1 := True;
            end case;

            TC_Passage ( TC_Pass_Point9 );  -- note passage through here
         end Subtract_Meter_Queue;  
   
         -- Here each Vehicle task queues itself awaiting release
         entry Wait_at_Meter when Release_One_Vehicle is
         -- Example of entry with barriers and persistent signal
            TC_Pass_Point : constant integer := 2;
         begin
            TC_Passage ( TC_Pass_Point );   -- note passage through here
            Release_One_Vehicle := false;   -- Consume the signal
            -- Decrement number of vehicles on ramp 
            Subtract_Meter_Queue;  -- Call procedure from within entry body
         end Wait_at_Meter;      
   
      end Ramp_31;  
      --================================================================
        
      -- Carrier task. One is created for each vehicle arriving at Ramp_31
      task body Vehicle_31 is
         TC_Pass_Point_1 : constant integer := 100;
         TC_Pass_Point_2 : constant integer := 200;
         TC_Pass_Point_3 : constant integer := 300;
      begin
         if Ramp_31.Meter_in_Use_State then  
            -- Increment count of number of vehicles on ramp 
            Ramp_31.Add_Meter_Queue;    -- Call a protected procedure
                                          -- which is also called from within
            -- enter the meter queue
            Ramp_31.Wait_at_Meter;      -- Call a protected entry   
            Report.Failed ("Exception not propagated back");
         end if;
         null;  --:::: call to the first in the series of the Ramp_Sensors
                --     this "passes" the vehicle from one sensor to the next
      exception
         when Storage_Error =>
               Ramp_31.TC_Passage ( TC_Pass_Point_1 );  -- note passage
         when TC_Defined_Error =>  
               Ramp_31.TC_Passage ( TC_Pass_Point_2 );  -- note passage
         when Constraint_Error =>
               Ramp_31.TC_Passage ( TC_Pass_Point_3 );  -- note passage
         when others => 
               Report.Failed ("Unexpected exception in Vehicle Task");
      end Vehicle_31;

      -- Simulate the arrival of a vehicle at the Ramp_Receiver of Ramp_31
      -- and the generation of an accompanying carrier task
      procedure New_Arrival_31 is
         Next_Vehicle_Task_31: acc_Vehicle_31 := new Vehicle_31;
         TC_Pass_Point : constant integer := 1; 
      begin
         Ramp_31.TC_Passage ( TC_Pass_Point );  -- Note passage through here
         null;  --::: stub
      end New_arrival_31;



   begin -- declare

      -- Test driver.  This is ALL test control code

      -- Create three independent tasks which will queue themselves on the
      -- entry.  Each task will get a different exception
      New_Arrival_31;
      New_Arrival_31;
      New_Arrival_31;

      delay ImpDef.Clear_Ready_Queue;

      -- Set the barrier condition of the entry true, releasing one task
      Ramp_31.Pulse; 
      delay ImpDef.Clear_Ready_Queue;
      
      Ramp_31.Pulse; 
      delay ImpDef.Clear_Ready_Queue;
      
      Ramp_31.Pulse; 
      delay ImpDef.Clear_Ready_Queue;

      if (TC_Expected_Passage_Total /= Ramp_31.TC_Get_Passage_Total)  or
         -- Note: We are not really interested in this next check.  It is 
         --       here to ensure the earlier statements which raised the 
         --       Constraint_Error are not optimized out
         (Ramp_31.TC_Get_Current_Exception /= 3) then
            Report.Failed ("Unexpected paths taken");
      end if;
      
   end; -- declare

   if TC_Failed_1 then 
      Report.Failed ("Bad path through Subtract_Meter_Queue");
   end if;

   Report.Result;

end C940010;
