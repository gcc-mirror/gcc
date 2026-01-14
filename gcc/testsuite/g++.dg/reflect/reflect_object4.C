// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_object.

#include <meta>

using namespace std::meta;

void fn();
constexpr auto e = reflect_object (fn); // { dg-error "no matching function for call" }
