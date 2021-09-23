// PR c++/99844
// { dg-do compile { target c++20 } }

template <bool... B>
struct S {
  constexpr explicit(B) S() {} // { dg-error "parameter packs not expanded" }
};

constexpr S<true> s;
