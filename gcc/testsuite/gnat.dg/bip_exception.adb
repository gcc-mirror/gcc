--  { dg-do compile }
--  { dg-options "-gnatwa" }

package body BIP_Exception is
   package body Constructors is
      function Initialize return T_C4_Scheduler is
      begin
         return T_C4_Scheduler'(T_Super with null record);
      end Initialize;
   end Constructors;

   overriding procedure V_Run (This : in T_C4_Scheduler) is
      pragma Unreferenced (This);
   begin
      null;
   end V_Run;
end BIP_Exception;
