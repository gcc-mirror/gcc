-- { dg-do compile }

package Linker_Alias is

   Var : Integer;  -- { dg-error "aliased to undefined symbol" }
   pragma Export (C, Var, "my_var");
   pragma Linker_Alias (Var, "var2");

end Linker_Alias;
