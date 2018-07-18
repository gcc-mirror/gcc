package Debug11_Pkg is

   Global_Imported_Var : Integer;
   pragma Import (C, Global_Imported_Var, "imported_var");

   function Global_Imported_Func return Integer;
   pragma Import (C, Global_Imported_Func, "imported_func");

   procedure Dummy;

end Debug11_Pkg;
