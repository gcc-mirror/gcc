-- { dg-do run }
-- { dg-options "-flto" { target lto } }

with Lto20_Pkg;

procedure Lto20 is
begin
  Lto20_Pkg.Proc (Lto20_Pkg.Null_Arr);
end;
