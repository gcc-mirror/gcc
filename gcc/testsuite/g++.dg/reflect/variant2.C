// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::variant_{size,alternative}.

#include <meta>
#include <variant>

using namespace std::meta;

constexpr auto s1 = variant_size (^^int); // { dg-error "couldn't instantiate 'std::variant_size<int>'" }
int x;
constexpr auto s2 = variant_size (^^x); // { dg-error "uncaught exception" }

constexpr auto r1 = variant_alternative (0, ^^std::variant<>); // { dg-error "uncaught exception" }
constexpr auto r2 = variant_alternative (0, ^^int); // { dg-error "uncaught exception" }
constexpr auto r3 = variant_alternative (0, ^^x); // { dg-error "uncaught exception" }

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
// { dg-error "pack index .0." "" { target *-*-* } 0 }
