// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test member list initializers.

using info = decltype(^^int);

struct A {
  info i;
  consteval A() : i{^^void} {}
};

static_assert (A{}.i == ^^void);
constexpr A a;
static_assert (a.i == ^^void);
constexpr A a2;
static_assert (a2.i == ^^void);
