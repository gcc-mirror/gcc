-- C940011.A 
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
--      Check that, in the body of a protected object created by the execution
--      of an allocator, external calls to other protected objects via 
--      the access type are correctly performed
--
-- TEST DESCRIPTION:
--      Use a subset of the simulation of the freeway on-ramp described in
--      c940005.  In this case an array of access types is built with pointers
--      to successive ramps.  The external calls within the protected
--      objects are made via the index into the array.  Routines which refer
--      to the "previous" ramp and the "next" ramp are exercised.  (Note: The
--      first and last ramps are assumed to be dummies and no first/last
--      condition code is included)
--      
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


with Report;


procedure C940011 is

   type Ramp;     
   type acc_Ramp is access Ramp;

   subtype Ramp_Index is integer range 1..4;
   

   -- Weighted load given to each potential problem area and accumulated
   type Load_Factor is range 0..8;
   Clear_Level    : constant Load_Factor := 0;
   Moderate_Level : constant Load_Factor := 3;
   
   --================================================================
   -- Only the Routines that are used in this test are shown
   --
   protected type Ramp is

      procedure Set_Index (Index : Ramp_Index);
      procedure Set_Local_Overload (Sensor_Level : Load_Factor);
      function Local_Overload        return Load_Factor;
      procedure Notify;
      function Next_Ramp_Overload    return Load_Factor;

   private

      This_Ramp : Ramp_Index;

      Next_Ramp_Alert : Boolean  := false;  -- Next Ramp is in trouble?

      -- Current state of the various Sample Points
      Local_State     : Load_Factor := Clear_Level;
   
   end Ramp;
   --================================================================

   -- Build a set of Ramp objects and an array of pointers to them
   --
   Ramp_Array : array (Ramp_Index) of acc_Ramp := (Ramp_Index => new Ramp);

   --================================================================
   protected body Ramp is
   
      procedure Set_Index (Index : Ramp_Index) is
      begin
         This_Ramp := Index;            
      end Set_Index;

      -- These Set/Clear routines are triggered by real-time sensors that
      -- reflect traffic state
      procedure Set_Local_Overload(Sensor_Level : Load_Factor) is
      begin
         if Local_State = Clear_Level then
            -- Notify "previous" ramp to check this one for current state.
            -- Subsequent changes in state will not send an alert
            -- When the situation clears another routine performs the 
            -- all_clear notification. (not shown)
            -- EXTERNAL CALL OF PROCEDURE FROM PROCEDURE
            Ramp_Array(This_Ramp - 1).Notify;   -- index to previous ramp
         end if;
         Local_State := Sensor_Level;
         null;   --::::: Start local meter if not already started
      end Set_Local_Overload;

      function Local_Overload return Load_Factor is
      begin
         return Local_State;
      end Local_Overload;
   
      -- This is notification from the next ramp that it is in
      -- overload.  With this provision we only need to sample the next
      -- ramp during adverse conditions.  
      procedure Notify is
      begin
         Next_Ramp_Alert := true;
      end Notify;

      function Next_Ramp_Overload return Load_Factor is
      begin
         if Next_Ramp_Alert then
            -- EXTERNAL FUNCTION CALL FROM FUNCTION
            -- Get next ramp's current state
            return Ramp_Array(This_Ramp + 1).Local_Overload;
         else
            return Clear_Level;  
         end if;
      end Next_Ramp_Overload;
   end Ramp;

   --================================================================


begin


   Report.Test ("C940011", "Protected Objects created by allocators: " &
                     "external calls via access types");

   -- Initialize each Ramp 
   for i in Ramp_Index loop
      Ramp_Array(i).Set_Index (i);
   end loop;      

   -- Test driver.  This is ALL test control code
   
   -- Simulate calls to the protected functions and procedures
   -- external calls. (do not call the "dummy" end ramps)

   -- Simple Call
   if Ramp_Array(2).Next_Ramp_Overload /= Clear_level then
      Report.Failed ("Primary call incorrect");
   end if;
   
   -- Call which results in an external procedure call via the array
   -- index from within the protected object
   Ramp_Array(3).Set_Local_Overload (Moderate_Level);
   
   -- Call which results in an external function call via the array
   -- index from within the protected object
   if Ramp_Array(2).Next_Ramp_Overload /= Moderate_level then
      Report.Failed ("Secondary call incorrect");
   end if;
      
   Report.Result;

end C940011;
