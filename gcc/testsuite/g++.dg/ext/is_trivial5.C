// CWG 1496
// PR c++/85723
// { dg-do compile { target c++11 } }

struct NonTrivial {
  NonTrivial() = delete;
};
static_assert(!__is_trivial (NonTrivial), "NonTrivial is trivial");
