with Limited_With6_Pkg;

package Limited_With6 is
  type Sup_T is new Integer;
  procedure Doit (Obj : Limited_With6_Pkg.T);

  type Rec is record
    A : Limited_With6_Pkg.Taft_Ptr;
  end record;
end Limited_With6;
