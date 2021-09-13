-- { dg-do compile }
-- { dg-options "-O2 -gnata -gnatVa" }

with Opt5_Pkg;

package Opt5 is

  type Object is new Opt5_Pkg.Object with private;

  Undefined : constant Object;

  overriding function Is_Defined (Self : Object) return Boolean;

  function Create (Sloc : Opt5_Pkg.Object) return Integer is (0)
    with Pre  => Sloc.Is_Defined;

private

  type Object is new Opt5_Pkg.Object with null record;

  Undefined : constant Object := (Opt5_Pkg.Undefined with others => <>);

  overriding function Is_Defined (Self : Object) return Boolean is (Self /= Undefined);

end Opt5;
