pragma Restrictions (No_Exception_Propagation);

package BIP_Exception_Pkg is
   type T_Process is abstract tagged limited private;
   type T_Process_Class_Access is access all T_Process'Class;

   procedure V_Run (This : in T_Process) is abstract;

private
   type T_Process is abstract tagged limited null record;
end BIP_Exception_Pkg;
