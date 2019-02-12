-- { dg-do run }
-- { dg-options "-flto" { target lto } }

with Lto19_Pkg1;

procedure Lto19 is
  R : Lto19_Pkg1.Rec := (I => 1, A => (others => 0));
begin
  Lto19_Pkg1.Proc (R);
end;
