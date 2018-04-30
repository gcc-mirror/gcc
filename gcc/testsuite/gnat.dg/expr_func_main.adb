--  { dg-do compile }

with Expr_Func_Pkg; use Expr_Func_Pkg;

procedure Expr_Func_Main is
   Val : Boolean := Expr_Func (456);
begin
   null;
end Expr_Func_Main;
