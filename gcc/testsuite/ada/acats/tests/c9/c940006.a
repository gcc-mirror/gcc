-- C940006.A 
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
--      Check that the body of a protected function can have external calls
--      to other protected functions and that the body of a protected
--      procedure can have external calls to protected procedures and to 
--      protected functions.
--
-- TEST DESCRIPTION:
--      Use a subset of the simulation of the freeway on-ramp described in
--      c940005.  In this case two protected objects are used but only a
--      minimum of routines are shown in each.  Both objects are hard coded
--      and detail two adjacent on-ramps (Ramp_31 & Ramp_32) with routines in 
--      each which use external calls to the  other.  

--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;

procedure C940006 is

begin

   Report.Test ("C940006", "Check external calls of protected functions" &
                        " and procedures");

   declare  -- encapsulate the test

      -- Weighted load given to each potential problem area and accumulated
      type Load_Factor is range 0..8;
      -- 
      Clear_Level    : constant Load_Factor := 0;
      Minimum_Level  : constant Load_Factor := 1;
      Moderate_Level : constant Load_Factor := 3;
      Serious_Level  : constant Load_Factor := 4;
      Critical_Level : constant Load_Factor := 6;

      --================================================================
      -- Only the Routines that are used in this test are shown
      --
      protected Ramp_31 is

         function Local_Overload        return Load_Factor;
         procedure Set_Local_Overload(Sensor_Level : Load_Factor);
         procedure Notify;
         function Next_Ramp_Overload    return Load_Factor;
         function Freeway_Overload      return Load_Factor;
         procedure Downstream_Ramps;
         function Get_DSR_Accumulate    return Load_Factor;

      private
         Next_Ramp_Alert : Boolean  := false;  -- Next Ramp is in trouble?

         -- Current state of the various Sample Points
         Local_State     : Load_Factor := Clear_Level;
         -- Accumulated load for next three downstream ramps
         DSR_Accumulate  : Load_Factor := Clear_Level;  
   
      end Ramp_31;  
      --================================================================
      -- Only the Routines that are used in this test are shown
      --
      protected Ramp_32 is
      
         function Local_Overload return Load_Factor;
         procedure Set_Local_Overload (Sensor_Level : Load_Factor); 

      private

         Local_State : Load_Factor := Clear_Level;

      end Ramp_32;  
      --================================================================
      protected body Ramp_31 is
   
         -- These Set/Clear routines are triggered by real-time sensors that
         -- reflect traffic state
         procedure Set_Local_Overload (Sensor_Level : Load_Factor) is
         begin
            -- Notify "previous" ramp to check this one for current state.
            -- Subsequent changes in state will not send an alert 
            null;   --::::: (see Ramp_32 for this code)
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
               return Ramp_32.Local_Overload;
            else
               return Clear_Level;  
            end if;
         end Next_Ramp_Overload;
   
         -- return the summation of all the load factors
         function Freeway_Overload return Load_Factor is
         begin
            return    Local_Overload                   
                      -- + :::: others                  
                      + Next_Ramp_Overload;             
         end Freeway_Overload;

         -- Snapshot the states of the next three downstream ramps
         procedure Downstream_Ramps is
         begin
            DSR_Accumulate := Ramp_32.Local_Overload;    -- EXTERNAL FUNCTION
            -- ::::         + Ramp_33.Local_Overload     -- FROM  PROCEDURE
            -- ::::         + Ramp_34.Local_Overload
         end Downstream_Ramps;

         -- Get last snapshot
         function Get_DSR_Accumulate return Load_Factor is
         begin
            return DSR_Accumulate;
         end Get_DSR_Accumulate;

      end Ramp_31;  
      --================================================================
      protected body Ramp_32 is

         function Local_Overload return Load_Factor is
         begin
            return Local_State;
         end;
   
   
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
               Ramp_31.Notify;
            end if;
            Local_State := Sensor_Level;
            null;   --::::: Start local meter if not already started
         end;

      end Ramp_32;  
      --================================================================



   begin -- declare

      -- Test driver.  This is ALL test control code
      -- Simulate calls to the protected functions and procedures
      -- from without the protected object, these will, in turn make the
      -- external calls.

      -- Check initial conditions, exercising the simple calls
      if not (Ramp_31.Local_Overload     = Clear_Level  and
              Ramp_31.Next_Ramp_Overload = Clear_Level  and
              Ramp_31.Freeway_Overload   = Clear_Level) and
              Ramp_32.Local_Overload     = Clear_Level    then
                  Report.Failed ("Initial Calls provided unexpected Results");
      end if;
      
      -- Simulate real-time sensors reporting overloads at a hardware level
      Ramp_31.Set_Local_Overload (1);
      Ramp_32.Set_Local_Overload (3);
     
      Ramp_31.Downstream_Ramps;   -- take the current snapshot

      if not (Ramp_31.Local_Overload     = Minimum_Level  and
              Ramp_31.Get_DSR_Accumulate = Moderate_Level and
              Ramp_31.Freeway_Overload   = Serious_Level)   then
         Report.Failed ("Secondary Calls provided unexpected Results");
      end if;
      
   end; -- declare

   Report.Result;

end C940006;
