-- { dg-do compile }

with Ada.Containers.Vectors;

procedure Use_Type2 is

  package Vectors is new Ada.Containers.Vectors (Positive, Character);

  use all type Vectors.Vector;

  X : Vectors.Vector := To_Vector (0);

begin
  Append (X, 'A');
end;
