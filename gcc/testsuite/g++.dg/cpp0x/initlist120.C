// PR c++/93559 - ICE with CONSTRUCTOR flags verification.
// { dg-do compile { target c++11 } }

struct F { int d[10]; };
struct E { F f; };

struct S {
  constexpr int operator()(char) { return 42; }
};

template <typename> struct X {
  constexpr static E foo(S s) { return {{{s(1)}}}; }
};

S s;
static_assert((X<S>::foo(s), 1), "");
