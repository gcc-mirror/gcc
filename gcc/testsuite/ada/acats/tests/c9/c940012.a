-- C940012.A 
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
--      Check that a protected object can have discriminants
--
-- TEST DESCRIPTION:
--      Use a subset of the simulation of the freeway on-ramp described in
--      c940005.  In this case an array of access types is built with pointers
--      to successive ramps.  Each ramp has its Ramp_Number specified by 
--      discriminant and this corresponds to the index in the array.  The test
--      checks that the ramp numbers are assigned as expected then uses calls
--      to  procedures within the objects (ramps) to verify external calls to
--      ensure the structures are valid.  The external references within the
--      protected objects are made via the index into the array.  Routines
--      which refer to the "previous" ramp and the "next" ramp are exercised. 
--      (Note: The first and last ramps are assumed to be dummies and no
--      first/last condition code is included)
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


with Report;


procedure C940012 is

   type Ramp_Index is range 1..4;

   type Ramp;
   type a_Ramp is access Ramp;

   Ramp_Array : array (Ramp_Index) of a_Ramp;

   -- Weighted load given to each potential problem area and accumulated
   type Load_Factor is range 0..8;
   Clear_Level    : constant Load_Factor := 0;
   Moderate_Level : constant Load_Factor := 3;
   
   --================================================================
   -- Only the Routines that are used in this test are shown
   --
   protected type Ramp (Ramp_In : Ramp_Index) is

      function Ramp_Number           return Ramp_Index;
      function Local_Overload        return Load_Factor;
      function Next_Ramp_Overload    return Load_Factor;
      procedure Set_Local_Overload(Sensor_Level : Load_Factor);
      procedure Notify;

   private

      Next_Ramp_Alert : Boolean  := false;  -- Next Ramp is in trouble?

      -- Current state of the various Sample Points
      Local_State     : Load_Factor := Clear_Level;
   
   end Ramp;
   --================================================================
   protected body Ramp is
   
      function Ramp_Number return Ramp_Index is
      begin
         return Ramp_In;
      end Ramp_Number;

      -- These Set/Clear routines are triggered by real-time sensors that
      -- reflect traffic state
      procedure Set_Local_Overload(Sensor_Level : Load_Factor) is
      begin
         if Local_State = Clear_Level then
            -- Notify "previous" ramp to check this one for current state.
            -- Subsequent changes in state will not send an alert
            -- When the situation clears another routine performs the 
            -- all_clear notification. (not shown)
            Ramp_Array(Ramp_In - 1).Notify;   -- index to previous ramp
         end if;
         Local_State := Sensor_Level;
         null;   --::::: Start local meter if not already started
      end;

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
            -- Get next ramp's current state
            return Ramp_Array(Ramp_In + 1).Local_Overload;
         else
            return Clear_Level;  
         end if;
      end Next_Ramp_Overload;
   end Ramp;
   --================================================================
   
begin


   Report.Test ("C940012", "Check that a protected object " &
                        "can have discriminants");

   -- Build the ramps and populate the ramp array
   for i in Ramp_Index loop
      Ramp_Array(i) := new Ramp (i);
   end loop;      

   -- Test driver.  This is ALL test control code

   -- Check the assignment of the index
   for i in Ramp_Index loop
      if Ramp_Array(i).Ramp_Number /= i then
         Report.Failed ("Ramp_Number assignment incorrect");
      end if;
   end loop;      

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

end C940012;
