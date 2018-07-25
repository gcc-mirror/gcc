with Ada.Finalization; use Ada.Finalization;

package BIP_Case_Expr_Pkg is
   type Lim_Ctrl is new Limited_Controlled with null record;

   function Make_Lim_Ctrl return Lim_Ctrl;
end BIP_Case_Expr_Pkg;
