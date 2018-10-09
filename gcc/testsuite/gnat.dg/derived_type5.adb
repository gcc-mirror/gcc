--  { dg-do compile }

with Derived_Type5_Pkg; use Derived_Type5_Pkg;

procedure Derived_Type5 is
  D : Derived;
begin
  Proc1 (Rec (D));
  Proc2 (Rec (D));
end;
