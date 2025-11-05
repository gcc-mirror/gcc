// PR c++/122192
// Verify name lookup within a base-specifier is type-only.

struct A {
  int B;
  struct B { };
};

struct S1 : A::B { }; // OK

template<class T> struct S2 : T::B { // OK, used to fail
  S2() : T::B() { } // Also OK
};
template struct S2<A>;
