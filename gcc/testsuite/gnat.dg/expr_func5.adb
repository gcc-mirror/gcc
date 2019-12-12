--  { dg-do compile }

procedure Expr_Func5 is
   type T is (B);
   function F return T is (B);
   type R (W : T := F) is null record;
   V : R;
begin
   null;
end Expr_Func5;
