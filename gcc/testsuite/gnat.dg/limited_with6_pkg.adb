with Limited_With6;

package body Limited_With6_Pkg is
  function Get_Expression_Support (Obj : T) return Limited_With6.Sup_T is
  begin
    return Limited_With6.Sup_T (Obj + 1);
  end Get_Expression_Support;

  type TT is access all Limited_With6.Rec;
end Limited_With6_Pkg;
