------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--      A D A . S Y N C H R O N O U S _ T A S K _ C O N T R O L . E D F     --
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

package Ada.Synchronous_Task_Control.EDF is

   pragma Unimplemented_Unit;

   procedure Suspend_Until_True_And_Set_Deadline
      (S  : in out Suspension_Object;
       TS : Ada.Real_Time.Time_Span);
end Ada.Synchronous_Task_Control.EDF;
