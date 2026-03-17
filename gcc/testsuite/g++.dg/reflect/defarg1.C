// PR c++/123608
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

consteval bool f(int a, std::meta::info b = ^^a) { return is_variable(b); }
static_assert(f(42));
