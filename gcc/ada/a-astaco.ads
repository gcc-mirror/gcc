------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--        A D A . A S Y N C H R O N O U S _ T A S K _ C O N T R O L         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

   --  This unit is not implemented in typical GNAT implementations that
   --  lie on top of operating systems, because it is infeasible to implement
   --  in such environments. The RM anticipates this situation (RM D.11(10)),
   --  and permits an implementation to leave this unimplemented even if the
   --  Real-Time Systems annex is fully supported.

   --  If a target environment provides appropriate support for this package,
   --  then the Unimplemented_Unit pragma should be removed from this spec,
   --  and an appropriate body provided. The framework for such a body is
   --  included in the distributed sources.

with Ada.Task_Identification;

package Ada.Asynchronous_Task_Control is

   pragma Unimplemented_Unit;

   procedure Hold (T : Ada.Task_Identification.Task_Id);

   procedure Continue (T : Ada.Task_Identification.Task_Id);

   function Is_Held (T : Ada.Task_Identification.Task_Id) return Boolean;

end Ada.Asynchronous_Task_Control;
