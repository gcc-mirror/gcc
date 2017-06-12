limited with Limited_With5;

package Limited_With5_Pkg is
  type T is limited private;
  function Get_Expression_Support (Obj : T) return Limited_With5.Sup_T with Inline;
private
  type T is new Integer;
end Limited_With5_Pkg;
