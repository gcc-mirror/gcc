------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . T A S K _ T E R M I N A T I O N                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Task_Identification;
with Ada.Exceptions;

package Ada.Task_Termination is
   pragma Preelaborate (Task_Termination);

   type Cause_Of_Termination is (Normal, Abnormal, Unhandled_Exception);

   type Termination_Handler is access protected procedure
     (Cause : Cause_Of_Termination;
      T     : Ada.Task_Identification.Task_Id;
      X     : Ada.Exceptions.Exception_Occurrence);

   procedure Set_Dependents_Fallback_Handler
     (Handler : Termination_Handler);
   function Current_Task_Fallback_Handler return Termination_Handler;

   procedure Set_Specific_Handler
     (T       : Ada.Task_Identification.Task_Id;
      Handler : Termination_Handler);
   function Specific_Handler
     (T : Ada.Task_Identification.Task_Id) return Termination_Handler;

end Ada.Task_Termination;
