with Predicate7_Pkg; use Predicate7_Pkg;

package Predicate7 is
   function Always_True (I : My_Int) return Boolean;

   function Identity (I : My_Int ) return Integer with Pre => Always_True (I);

   procedure Foo;

private
   function Identity (I : My_Int ) return Integer is (I);
   function Always_True (I : My_Int) return Boolean is (True);
end;
