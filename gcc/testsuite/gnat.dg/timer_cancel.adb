-- { dg-do run }

with Ada.Real_Time.Timing_Events;
use Ada.Real_Time, Ada.Real_Time.Timing_Events;

procedure Timer_Cancel is

   E : Timing_Event;
   C : Boolean;

   protected Dummy is
      procedure Trigger (Event : in out Timing_Event);
   end Dummy;

   protected body Dummy is
      procedure Trigger (Event : in out Timing_Event) is
      begin
         null;
      end Trigger;
   end Dummy;

begin
   Set_Handler (E, Time_Last, Dummy.Trigger'Unrestricted_Access);

   if Time_Of_Event (E) /= Time_Last then
      raise Program_Error with "Event time not set correctly";
   end if;

   Cancel_Handler (E, C);

   if not C then
      raise Program_Error with "Event triggered already";
   end if;

   if Time_Of_Event (E) /= Time_First then
      raise Program_Error with "Event time not reset correctly";
   end if;
end Timer_Cancel;
