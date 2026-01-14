// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::tuple_{size,element}.

#include <array>
#include <meta>

using namespace std::meta;

constexpr auto s1 = tuple_size (^^int); // { dg-error "couldn't instantiate 'std::tuple_size<int>'" }
int x;
constexpr auto s2 = tuple_size (^^x); // { dg-error "uncaught exception" }

constexpr auto r1 = tuple_element (666, ^^std::tuple<int>); // { dg-error "uncaught exception" }

using Arr0 = std::array<int, 0>;
constexpr auto r2 = tuple_element (1, ^^Arr0);

// { dg-error "tuple index must be in range" "" { target *-*-* } 0 }
// { dg-error "array index is in range" "" { target *-*-* } 0 }
// { dg-error "pack index .666." "" { target *-*-* } 0 }
