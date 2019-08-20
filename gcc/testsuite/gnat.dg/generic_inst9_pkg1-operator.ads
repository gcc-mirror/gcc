generic
  type T is private;
  with function Compare
    (Left, Right: T) return Generic_Inst9_Pkg1.T is <>;
package Generic_Inst9_Pkg1.Operator is
  function Compare (Left, Right: Integer) return Generic_Inst9_Pkg1.T is
    (Equal);
  function "<"  (Left, Right: T) return Boolean is
    (Compare (Left, Right) = Smaller);
end Generic_Inst9_Pkg1.Operator;
