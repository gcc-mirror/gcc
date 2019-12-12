package Predicate3_Pkg is
 
   type Priv is private;
   C: constant Priv;
 
   function Test (X: Priv) return Boolean;
   subtype Subt is Priv with Dynamic_Predicate => (Test (Subt));
 
   function Wrong return Subt;
   function Good (X: Subt) return Boolean;
 
private
 
   type Priv is new Integer;
   C: constant Priv := -1;
 
   function Test (X: Priv) return Boolean is (X > 0);
 
   function Wrong return Subt is (-1);
   function Good (X: Subt) return Boolean is (True);
 
end Predicate3_Pkg;
