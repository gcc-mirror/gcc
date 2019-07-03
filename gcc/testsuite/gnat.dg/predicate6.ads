generic
package Predicate6 is
   type Price_Kind is (Infinitely_Small, Normal, Infinitely_Large);
   subtype Infinite_Kind is Price_Kind with Static_Predicate =>
     Infinite_Kind in Infinitely_Small | Infinitely_Large;
   function "not" (Kind : Infinite_Kind) return Infinite_Kind is
     (case Kind is when Infinitely_Small => Infinitely_Large,
        when Infinitely_Large => Infinitely_Small);
   procedure Foo;
end;
