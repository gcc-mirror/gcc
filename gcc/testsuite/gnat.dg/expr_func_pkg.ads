package Expr_Func_Pkg is
   function Func (Val : Integer) return Boolean with Inline;

   function Expr_Func (Val : Integer) return Boolean;
   function Expr_Func (Val : Integer) return Boolean is (True);
end Expr_Func_Pkg;
