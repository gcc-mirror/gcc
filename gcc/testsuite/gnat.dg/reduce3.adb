-- { dg-do run }
-- { dg-options "-gnat2022" }

with Ada.Containers.Vectors;

procedure Reduce3 is

  package Qs is new
    Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Positive);

  V   : Qs.Vector;
  Sum : Positive;

begin
  V.Append (1);
  Sum := V'Reduce ("+", 0);
end;
