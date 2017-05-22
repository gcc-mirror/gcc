with Limited_With5;

package body Limited_With5_Pkg is
  function Get_Expression_Support (Obj : T) return Limited_With5.Sup_T is
  begin
    return Limited_With5.Sup_T (Obj + 1);
  end Get_Expression_Support;
end Limited_With5_Pkg;
