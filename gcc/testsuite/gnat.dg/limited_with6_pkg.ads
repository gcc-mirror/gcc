limited with Limited_With6;

package Limited_With6_Pkg is
  type T is limited private;
  function Get_Expression_Support (Obj : T) return Limited_With6.Sup_T with Inline;

  type Taft_Ptr is private;

private
  type T is new Integer;

  type TT;
  type Taft_Ptr is access TT;
end Limited_With6_Pkg;
