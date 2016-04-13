// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool One() { return sizeof(T) >= 4; }

template<typename T>
  concept bool Two() { return One<T>() && sizeof(T) >= 8; }

// Check that there is no ecsacpe hatch
template<Two T> struct S4 { };
template<One T> struct S4<T> { }; // { dg-error "does not specialize" }

struct one_type { char x[4]; };

// Constraints are checked even when decls are not instantiatied.
S4<one_type>* x4b; // { dg-error "constraint|invalid" }

int main() { }
