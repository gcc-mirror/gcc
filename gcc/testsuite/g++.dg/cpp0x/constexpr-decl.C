// PR c++/46930
// { dg-options -std=c++0x }

struct S {
  static constexpr int size;	// { dg-error "must have an initializer" }
  // { dg-error "previous declaration" "" { target *-*-* } 5 }
};

const int limit = 2 * S::size;
constexpr int S::size = 256;	// { dg-error "" }
