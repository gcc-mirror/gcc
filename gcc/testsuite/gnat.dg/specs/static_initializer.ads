-- { dg-do compile }

package static_initializer is

  type Vector is array (1 .. 3) of Float;
  type Arr is array (Integer range 1 .. 3) of Vector;

  Pos : constant Arr := ((0.0, 1.0, 2.0),
                         (0.5, 1.5, 2.5),
                         (1.0, 2.0, 4.0));

end;

-- { dg-final { scan-assembler-not "elabs" } }
