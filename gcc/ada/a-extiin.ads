------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . E X E C U T I O N _ T I M E . I N T E R R U P T S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Interrupts;
with Ada.Real_Time;

package Ada.Execution_Time.Interrupts with
  SPARK_Mode
is

   pragma Unimplemented_Unit;

   function Clock (Interrupt : Ada.Interrupts.Interrupt_ID) return CPU_Time
   with
     Volatile_Function,
     Global => Ada.Real_Time.Clock_Time,
     Pre    => Separate_Interrupt_Clocks_Supported;

   function Supported (Interrupt : Ada.Interrupts.Interrupt_ID) return Boolean
   with
     Global => null;

end Ada.Execution_Time.Interrupts;
