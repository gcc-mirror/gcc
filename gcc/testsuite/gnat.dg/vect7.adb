-- { dg-do compile }

package body Vect7 is

  procedure Assign is
    v1 : constant v4sf := (-1.0, -2.0, -3.0, -4.0);
    v2 : v4sf := v1;
    v3 : v4sf;
  begin
    v3 := vzero;
    v3 := vconst;
    v3 := v1;
    v3 := v2;
    v3 := (1.0, -2.0, 3.0, -4.0);
    v3 := (1.0, -2.0, 3.0, F);

    v2 := vzero;
  end;

end Vect7;
