// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept One = sizeof(T) >= 4;

template<typename T>
  concept Two = One<T> && sizeof(T) >= 8;

// Check that there is no ecsacpe hatch
template<Two T> struct S4 { };
template<One T> struct S4<T> { }; // { dg-error "does not specialize" }

struct one_type { char x[4]; };

// Constraints are checked even when decls are not instantiatied.
S4<one_type>* x4b; // { dg-error "constraint|invalid" }
