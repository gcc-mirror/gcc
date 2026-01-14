// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test comparing COMPONENT_REFs.

#include <meta>

struct S { int x, y; };

consteval auto f(S &s) {
  return std::meta::reflect_object(s.x);
}

S s;
int& x = s.x;
static_assert(object_of(^^x) == f(s));
