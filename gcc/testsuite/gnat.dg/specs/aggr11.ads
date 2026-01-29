-- PR ada/123861
-- { dg-do compile }
-- { dg-options "-gnat2022" }

with Ada.Containers.Vectors;

package Aggr11 is

  package Vectors is new Ada.Containers.Vectors (Positive, Integer);
  use Vectors;

  A : constant Vector  := [];
  B : constant Boolean := [] = A;        -- ICE
  C : constant Boolean := Vector'[] = A; -- Works
  D : constant Boolean := A = [];        -- Works

end Aggr11;
