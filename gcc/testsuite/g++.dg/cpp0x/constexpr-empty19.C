// PR c++/110197
// { dg-do compile { target c++11 } }

struct A {
  constexpr A() : A(__builtin_is_constant_evaluated()) { }
  constexpr A(int) { }
};
constexpr A a1[1] = {{}};
constexpr A a2[2] = {{}, {}};
constexpr A a3[3] = {{}, {}, {}};
constexpr A a4[4] = {};
constexpr A a5[5] = {};
