// Test for P0195R2 multiple using.
// { dg-options "" }

namespace A {
  int i;
}

namespace A1 {
  using A::i, A::i;	 // OK: double declaration
  // { dg-warning "comma" "" { target c++14_down } .-1 }
}

struct B {
  int i;
};

struct X : B {
  using B::i, B::i; // { dg-error "redeclaration" }
  // { dg-warning "comma" "" { target c++14_down } .-1 }
};
