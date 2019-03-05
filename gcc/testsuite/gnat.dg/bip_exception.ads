pragma Restrictions (No_Exception_Propagation);
with BIP_Exception_Pkg;

package BIP_Exception is
   type T_C4_Scheduler is new BIP_Exception_Pkg.T_Process with private;
   type T_C4_Scheduler_Class_Access is access all T_C4_Scheduler'Class;

   package Constructors is
      function Initialize return T_C4_Scheduler;
   end Constructors;

   overriding procedure V_Run (This : in T_C4_Scheduler);
   pragma Suppress (Elaboration_Check, V_Run);

private
   package Super renames BIP_Exception_Pkg;
   subtype T_Super is Super.T_Process;

   type T_C4_Scheduler is new T_Super with null record;
end BIP_Exception;
