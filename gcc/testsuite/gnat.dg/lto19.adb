-- { dg-do run }
-- { dg-options "-flto" { target lto } }
-- { dg-excess-errors "does not match original declaration" }

with Lto19_Pkg1;

procedure Lto19 is
  R : Lto19_Pkg1.Rec := (I => 1, A => (others => 0));
begin
  Lto19_Pkg1.Proc (R);
end;
