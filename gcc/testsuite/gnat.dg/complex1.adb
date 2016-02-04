-- { dg-do compile }

with Ada.Numerics.Complex_types; use Ada.Numerics.Complex_types;
with Complex1_Pkg; use Complex1_Pkg;

procedure Complex1 is
  Z : Complex;
begin
  Coord (Z.Re, Z.Im);
end;
