// PR c++/46930
// { dg-options -std=c++11 }

struct S {
  static constexpr int size;	// { dg-error "must have an initializer" "must have" }
  // { dg-error "previous declaration" "previous" { target *-*-* } 5 }
};

const int limit = 2 * S::size;
constexpr int S::size = 256;	// { dg-error "" }
