-- { dg-do run }
-- { dg-options "-gnata" }

with Ada.Numerics.Generic_Real_Arrays;

procedure Matrix1 is

  package GRA is new Ada.Numerics.Generic_Real_Arrays (real => float);
  use GRA;

  M : constant Real_Matrix (1..2, 1..2) := ((1.0, 0.0), (0.0, 2.0));
  E : constant Real_Vector := Eigenvalues (M);

begin
  null;
end;
