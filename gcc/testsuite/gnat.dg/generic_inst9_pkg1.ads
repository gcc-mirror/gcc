
package Generic_Inst9_Pkg1 is

  type T is (None, Smaller, Equal, Larger);

  type R is record
    Val : Integer;
  end record;

  function Compare (Left, Right : R) return T;

end;
