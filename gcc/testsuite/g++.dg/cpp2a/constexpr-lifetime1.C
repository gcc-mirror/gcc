// { dg-do compile { target c++20 } }

#include "construct_at.h"

struct S { int x; };
constexpr int f() {
  S s;
  s.~S();
  std::construct_at(&s, 5);
  return s.x;
}
static_assert(f() == 5);

struct T { int x; constexpr ~T() {} };
constexpr int g() {
  T t;
  t.~T();
  std::construct_at(&t, 12);
  return t.x;
}
static_assert(g() == 12);
