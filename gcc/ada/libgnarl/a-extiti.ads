------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . E X E C U T I O N _ T I M E . T I M E R S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This unit is not implemented in typical GNAT implementations that lie on
--  top of operating systems, because it is infeasible to implement in such
--  environments.

--  If a target environment provides appropriate support for this package,
--  then the Unimplemented_Unit pragma should be removed from this spec and
--  an appropriate body provided.

with System;

package Ada.Execution_Time.Timers is
   pragma Preelaborate;

   pragma Unimplemented_Unit;

   type Timer (T : not null access constant Ada.Task_Identification.Task_Id) is
      tagged limited private;

   type Timer_Handler is access protected procedure (TM : in out Timer);

   Min_Handler_Ceiling : constant System.Any_Priority := System.Priority'Last;

   procedure Set_Handler
     (TM      : in out Timer;
      In_Time : Ada.Real_Time.Time_Span;
      Handler : Timer_Handler);

   procedure Set_Handler
     (TM      : in out Timer;
      At_Time : CPU_Time;
      Handler : Timer_Handler);

   function Current_Handler (TM : Timer) return Timer_Handler;

   procedure Cancel_Handler
     (TM        : in out Timer;
      Cancelled : out Boolean);

   function Time_Remaining (TM : Timer) return Ada.Real_Time.Time_Span;

   Timer_Resource_Error : exception;

private
   type Timer (T : access Ada.Task_Identification.Task_Id) is
      tagged limited null record;
end Ada.Execution_Time.Timers;
