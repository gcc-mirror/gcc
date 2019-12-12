--  { dg-do run }

with Predicate10_Pkg; use Predicate10_Pkg;

procedure Predicate10 is
   X : I_Pointer := new Integer'(0);
begin
   Foo (1, X);
end;
