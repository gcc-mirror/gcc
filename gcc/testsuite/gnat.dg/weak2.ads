package Weak2 is

   Var : Integer;
   pragma Import (Ada, Var, "var_name");
   pragma Weak_External (Var);

   function F return Integer;

end Weak2;
