// PR c++/46336
// { dg-options -std=c++11 }

extern "C" {
  enum A { };
  inline constexpr A
  f(A a, A b)			// { dg-error "previous declaration" }
  { return A(static_cast<int>(a) & static_cast<int>(b)); }
  enum B { };
  inline constexpr B
  f(B a, B b)			// { dg-error "C function" }
  { return B(static_cast<int>(a) & static_cast<int>(b)); }
}

