// P1094R2
// { dg-do compile { target c++20 } }

inline namespace A::B { // { dg-error "a nested namespace definition cannot be inline" }
  int i;
}

namespace inline C::D { // { dg-error "expected|does not name a type" }
  int i;
}

namespace E::F inline { // { dg-error "expected" }
  int i;
}

namespace inline G { // { dg-error "expected|does not name a type" }
  int i;
}

inline namespace inline H { // { dg-error "expected|does not name a type" }
  int i;
}

inline namespace inline I::J { // { dg-error "expected|does not name a type" }
  int i;
}
