with Generic_Inst9_Pkg1;

generic

  type Item_T is private;
  with function Compare
    (Left, Right: Item_T) return Generic_Inst9_Pkg1.T is <>;

  type Bound_T is private;
  with function Compare
    (Left, Right : Bound_T) return Generic_Inst9_Pkg1.T is <>;

package Generic_Inst9_Pkg2 is

  procedure Dummy;

end Generic_Inst9_Pkg2;
