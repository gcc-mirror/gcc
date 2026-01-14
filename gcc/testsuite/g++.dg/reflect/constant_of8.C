// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::constant_of, CWG 3111.

#include <meta>
using namespace std::meta;

constexpr int is[] = {1, 2, 3};
constexpr info r = reflect_constant_array (is);
static_assert (constant_of (^^is) == r);

constexpr int mis[2][2] = {{1, 2}, {3, 4}};
// LWG4483 Multidimensional arrays
// constexpr info mr = reflect_constant_array (mis);
// static_assert (constant_of (^^mis) == mr);

