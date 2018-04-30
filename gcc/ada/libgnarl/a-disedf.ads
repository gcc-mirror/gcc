------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  A D A . D I S P A T C H I N G . E D F                   --
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

with Ada.Real_Time;
with Ada.Task_Identification;

package Ada.Dispatching.EDF is
   pragma Preelaborate;

   pragma Unimplemented_Unit;

   subtype Deadline is Ada.Real_Time.Time;

   Default_Deadline : constant Deadline := Ada.Real_Time.Time_Last;

   procedure Set_Deadline
      (D : Deadline;
       T : Ada.Task_Identification.Task_Id :=
             Ada.Task_Identification.Current_Task);

   procedure Delay_Until_And_Set_Deadline
      (Delay_Until_Time : Ada.Real_Time.Time;
       Deadline_Offset  : Ada.Real_Time.Time_Span);

   function Get_Deadline
      (T : Ada.Task_Identification.Task_Id :=
             Ada.Task_Identification.Current_Task)
       return Deadline
   with
     SPARK_Mode,
     Volatile_Function,
     Global => Ada.Task_Identification.Tasking_State;

end Ada.Dispatching.EDF;
