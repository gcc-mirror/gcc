// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::constant_of.

#include <meta>

using namespace std::meta;

constexpr int x = 0;
constexpr int y = 0;

// OK, x and y are different variables, so their reflections compare different
static_assert (^^x != ^^y);

// OK, both constant_of(^^x) and constant_of(^^y) represent the value 0
static_assert (constant_of (^^x) == constant_of (^^y));

// OK, likewise
static_assert (constant_of (^^x) == reflect_constant (0));

struct S { int m; };
constexpr S s {42};
static_assert (is_object (constant_of (^^s)) && is_object (reflect_object (s)));
// OK, template parameter object that is template-argument-equivalent to s is
// a different object than s
static_assert (constant_of (^^s) != reflect_object (s));
static_assert (constant_of (^^s) == constant_of (reflect_object (s)));
constexpr auto r1 = reflect_constant (s);
constexpr auto r2 = reflect_object (s);
static_assert (is_object (r1) && is_object (r2));
static_assert (r1 != r2);
static_assert (r1 == constant_of (r2));

consteval info fn() {
  constexpr int x = 42;
  return ^^x;
}

// error: x is outside its lifetime
constexpr info r = constant_of (fn ());
