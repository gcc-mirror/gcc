generic
   type Value_Type is private;
package Predicate4_Pkg is
  type MT (Has : Boolean := False) is record
     case Has is
        when False =>
           null;
        when True =>
           MX : Value_Type;
     end case;
  end record;

  function Foo (M : MT) return Boolean is (not M.Has);
  subtype LT is MT with Dynamic_Predicate => not LT.Has;
  function Bar (M : MT) return Boolean is (Foo (M));
end;
