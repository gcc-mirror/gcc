// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::constant_of, CWG 3111.

#include <meta>
using namespace std::meta;

void f() {}
static_assert (constant_of (^^f) == reflect_function (f));
