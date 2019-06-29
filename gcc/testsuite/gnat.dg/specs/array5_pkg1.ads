with Array5_Pkg2; use Array5_Pkg2;
with Array5_Pkg2.G;

package Array5_Pkg1 is

  type Derived is new Root with record
    N : Integer;
  end record;

  package My_G is new Array5_Pkg2.G (Derived);

  type Arr is array (1 .. My_G.Data.N) of Integer;

end Array5_Pkg1;
