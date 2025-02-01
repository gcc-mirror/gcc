--  { dg-do compile }

with Warn33_Pkg; use Warn33_Pkg;

procedure Warn33 is
  Var : DerT;
begin
  Var := 1.0 - Var; -- { dg-warning "may be referenced before" }
end;
