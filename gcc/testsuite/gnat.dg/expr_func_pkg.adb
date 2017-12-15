package body Expr_Func_Pkg is
   function Func (Val : Integer) return Boolean is
   begin
      Error;  --  { dg-error "\"Error\" is undefined" }
      return Val = 123;
   end Func;
end Expr_Func_Pkg;
