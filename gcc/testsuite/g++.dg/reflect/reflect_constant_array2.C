// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant_string.

#include <meta>
#include <ranges>
#include <span>

using namespace std::meta;

struct A { int a, b; mutable int c; };
constexpr auto a = reflect_constant_array (std::vector <A> { A { 1, 2, 3 } });
// { dg-error "'reflect_constant_array' argument with 'std::ranges::range_value_t<std::vector<A> >' \\\{aka 'A'\\\} which is not a structural type" "" { target *-*-* } .-1 }
struct B { constexpr B (int x, int y) : a (x), b (y) {} constexpr ~B () {} B (const B &) = delete; int a, b; };
constexpr B b[2] = { { 1, 2 }, { 2, 3 } };
constexpr auto c = reflect_constant_array (b);
// { dg-error "'reflect_constant_array' argument with 'std::ranges::range_value_t<const B \\\[2\\\]>' \\\{aka 'B'\\\} which is not copy constructible" "" { target *-*-* } .-1 }
