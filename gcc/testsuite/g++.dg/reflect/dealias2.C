// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::dealias.

#include <meta>
using namespace std::meta;

constexpr int i = 42;
static_assert (dealias (reflect_object (i)) == reflect_object (i));
static_assert (dealias (constant_of (^^i)) == constant_of (^^i));
static_assert (dealias (reflect_constant (42)) == reflect_constant (42));
[[=1, =1, =2, =1.0f]] void fn ();
static_assert (dealias (annotations_of (^^fn)[0]) == annotations_of (^^fn)[0]);
static_assert (dealias (^^i) == ^^i);
