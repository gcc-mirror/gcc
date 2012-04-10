package Vect7 is

  type v4sf is array (1 .. 4) of Float;
  for v4sf'Alignment use 16;
  pragma Machine_Attribute (v4sf, "vector_type");

  vzero  : constant v4sf := (0.0, 0.0, 0.0, 0.0);
  vconst : constant v4sf := (1.0, 2.0, 3.0, 4.0);
  vvar   : v4sf := vconst;

  F : Float := 5.0;

  procedure Assign;

end Vect7;
