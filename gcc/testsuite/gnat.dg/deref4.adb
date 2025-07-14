-- { dg-do compile }
-- { dg-options "-gnatX" }

with Deref4_Pkg; use Deref4_Pkg;

procedure Deref4 is
begin
  Obj.Proc (null);
end;
