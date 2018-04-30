------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   A D A . E X E C U T I O N _ T I M E                    --
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

--  If a target environment provides appropriate support for this package
--  then the Unimplemented_Unit pragma should be removed from this spec and
--  an appropriate body provided.

with Ada.Task_Identification;
with Ada.Real_Time;

package Ada.Execution_Time with
  SPARK_Mode
is
   pragma Preelaborate;

   pragma Unimplemented_Unit;

   type CPU_Time is private;

   CPU_Time_First : constant CPU_Time;
   CPU_Time_Last  : constant CPU_Time;
   CPU_Time_Unit  : constant := 0.000001;
   CPU_Tick       : constant Ada.Real_Time.Time_Span;

   use type Ada.Task_Identification.Task_Id;

   function Clock
     (T : Ada.Task_Identification.Task_Id :=
        Ada.Task_Identification.Current_Task)
      return CPU_Time
   with
     Volatile_Function,
     Global => Ada.Real_Time.Clock_Time,
     Pre    => T /= Ada.Task_Identification.Null_Task_Id;

   function "+"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time
   with
     Global => null;

   function "+"
     (Left  : Ada.Real_Time.Time_Span;
      Right : CPU_Time) return CPU_Time
   with
     Global => null;

   function "-"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time
   with
     Global => null;

   function "-"
     (Left  : CPU_Time;
      Right : CPU_Time) return Ada.Real_Time.Time_Span
   with
     Global => null;

   function "<"  (Left, Right : CPU_Time) return Boolean with
     Global => null;
   function "<=" (Left, Right : CPU_Time) return Boolean with
     Global => null;
   function ">"  (Left, Right : CPU_Time) return Boolean with
     Global => null;
   function ">=" (Left, Right : CPU_Time) return Boolean with
     Global => null;

   procedure Split
     (T  : CPU_Time;
      SC : out Ada.Real_Time.Seconds_Count;
      TS : out Ada.Real_Time.Time_Span)
   with
     Global => null;

   function Time_Of
     (SC : Ada.Real_Time.Seconds_Count;
      TS : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero)
      return CPU_Time
   with
     Global => null;

   Interrupt_Clocks_Supported          : constant Boolean := False;
   Separate_Interrupt_Clocks_Supported : constant Boolean := False;

   pragma Warnings (Off, "check will fail at run time");
   function Clock_For_Interrupts return CPU_Time with
     Volatile_Function,
     Global => Ada.Real_Time.Clock_Time,
     Pre    => Interrupt_Clocks_Supported;
   pragma Warnings (On, "check will fail at run time");

private
   pragma SPARK_Mode (Off);

   type CPU_Time is new Ada.Real_Time.Time;

   CPU_Time_First : constant CPU_Time  := CPU_Time (Ada.Real_Time.Time_First);
   CPU_Time_Last  : constant CPU_Time  := CPU_Time (Ada.Real_Time.Time_Last);

   CPU_Tick : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Tick;

end Ada.Execution_Time;
