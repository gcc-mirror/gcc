-- C974012.A
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
--      Check that the abortable part of an asynchronous select statement is
--      aborted if it does not complete before the triggering statement
--      completes, where the triggering statement is a call on a protected
--      entry which is queued.
--
-- TEST DESCRIPTION:
--      A fraction of in-line code is simulated.  A voltage deficiency causes
--      the routine to seek an alternate best-cost route on an electrical grid
--      system.  
--      
--      An asynchronous select is used with the triggering alternative being a
--      call to a protected entry with a barrier.  The abortable part is a
--      routine simulating the lengthy alternate path negotiation.  The entry
--      barrier would be cleared if the voltage deficiency is rectified before
--      the alternate can be found thus nullifying the need for the alternate.  
--      
--      The test simulates a return to normal in the middle of the
--      negotiation.  The barrier is cleared, the triggering alternative
--      completes first and the abortable part should be aborted.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


with Report;
with ImpDef;

procedure C974012 is

   subtype Grid_Path is string(1..21);
   subtype Deficiency is integer range 100..1_000;   -- in MWh

   New_Path         : Grid_Path;
   Dummy_Deficiency : Deficiency := 520;   
   Path_Available   : Boolean    := false;

   TC_Terminate_Negotiation_Executed  : Boolean := false;
   TC_Trigger_Completed               : Boolean := false;
   TC_Negotiation_Completed           : Boolean := false;

   protected Local_Deficit is
      procedure Set_Good_Voltage;
      procedure Bad_Voltage;
      entry Terminate_Negotiation;
   private
      Good_Voltage   : Boolean := false;   -- barrier
   end Local_Deficit;

   protected body Local_Deficit is

      procedure Set_Good_Voltage is
      begin
         Good_Voltage := true;
      end Set_Good_Voltage;

      procedure Bad_Voltage is
      begin
         Good_Voltage := false;
      end Bad_Voltage;

      -- Trigger is queued on this entry with barrier condition
      entry Terminate_Negotiation when Good_Voltage is
      begin
         -- complete the triggering call thus terminating grid_path
         -- negotiation.
         null; --::: stub - signal main board
         TC_Terminate_Negotiation_Executed := true;   -- show path traversal
      end Terminate_Negotiation;

   end Local_Deficit;


   -- Routine to find the most cost effective grid path for this 
   -- particular deficiency at this particular time
   --
   procedure Path_Negotiation (Requirement : in  Deficiency;
                               Best_Path   : out Grid_Path  ) is
      
      Dummy_Path : Grid_Path := "NYC.425_NY.227_NH.132";
      Match : Deficiency := Report.Ident_Int (Requirement); 
  
   begin
      -- 
      null; --::: stub
      --
      -- Simulate a lengthy path negotiation 
      for i in 1..5 loop
         delay ImpDef.Minimum_Task_Switch;   
         -- Part of the way through the negotiation simulate some external 
         -- event returning the voltage to acceptable level 
         if i = 3 then
            Local_Deficit.Set_Good_Voltage;   -- clear the barrier
         end if;
      end loop; 
      
      Best_Path := Dummy_Path;
      TC_Negotiation_Completed := true;
   
   end Path_Negotiation;

  

begin

   Report.Test ("C974012", "Asynchronous Select: Trigger is queued on a " &
                           "protected entry and completes before the " &
                           "abortable part");

   -- :::::::::   Fragment of code
   
   Local_Deficit.Bad_Voltage;      -- Set barrier condition

   -- For the given voltage deficiency start negotiating the best grid
   -- path.  If voltage returns to acceptable level cancel the negotiation
   --
   select
      -- Prepare to terminate the Path_Negotiation if voltage improves
      Local_Deficit.Terminate_Negotiation;
      TC_Trigger_Completed := true;
   then abort
      Path_Negotiation (Dummy_Deficiency, New_Path) ;
      Path_Available := true;
   end select;
   -- :::::::::

   if not TC_Terminate_Negotiation_Executed or else not
                                               TC_Trigger_Completed then 
      Report.Failed ("Unexpected test path taken");
   end if;

   if Path_Available or else TC_Negotiation_Completed then
      Report.Failed ("Abortable part was not aborted");
   end if;
   Report.Result;

end C974012;
