-- { dg-do compile }
-- { dg-options "-O -gnatn" }

with Taft_Type4_Pkg; use Taft_Type4_Pkg;

procedure Taft_Type4 is
  Obj : T;
begin
  Proc (Obj);
end;
