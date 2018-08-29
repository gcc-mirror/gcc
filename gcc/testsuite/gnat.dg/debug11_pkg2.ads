package Debug11_Pkg2 is

   Foreign_Imported_Var : Integer;
   pragma Import (C, Foreign_Imported_Var, "imported_var");

   function Foreign_Imported_Func return Integer;
   pragma Import (C, Foreign_Imported_Func, "imported_func");

end Debug11_Pkg2;
