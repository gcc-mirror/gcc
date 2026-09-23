// PR c++/126483
// { dg-do compile { target c++17 } }

struct A {
  int m;
  constexpr A(int n) : m(__builtin_is_constant_evaluated() ? 42 : n) { }
};

constexpr auto v = [] {
  A a = 0;
  return a.m;
};
static_assert(v() == 42);

template<class>
constexpr auto vt = [] {
  A a = 0;
  return a.m;
};
static_assert(vt<void>() == 42);
