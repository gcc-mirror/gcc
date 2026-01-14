// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant.

#include <meta>

using namespace std::meta;

int i;
int foo ();
struct S {};

constexpr auto r1 = reflect_constant (i); // { dg-error "not usable in a constant expression" }
constexpr auto r2 = reflect_constant (foo ()); // { dg-error "call to non-.constexpr." }
constexpr auto r3 = reflect_constant (S); // { dg-error "expected" }
