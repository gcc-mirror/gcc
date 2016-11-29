-- { dg-do run }
-- { dg-options "-flto" { target lto } }
-- { dg-excess-errors "does not match original declaration" }

with Lto20_Pkg;

procedure Lto20 is
begin
  Lto20_Pkg.Proc (Lto20_Pkg.Null_Arr);
end;
