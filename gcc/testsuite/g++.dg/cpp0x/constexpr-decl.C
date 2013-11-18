// PR c++/46930
// { dg-options -std=c++11 }

struct S {
  static constexpr int size;	// { dg-error "must have an initializer" "must have" }
};

const int limit = 2 * S::size;
constexpr int S::size = 256;
