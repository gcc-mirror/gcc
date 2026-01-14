// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant_array.

#include <meta>
#include <ranges>
#include <span>

using namespace std::meta;

constexpr int arr[4]{1, 2, 3, 4};
constexpr info rarr = reflect_constant_array(std::span<const int, 4>(arr));
static_assert (constant_of(^^arr) == rarr);

constexpr int marr[2][2]{1, 2, 3, 4};
// LWG4483 multidimensional arrays
// constexpr info mrarr = reflect_constant_array(std::span<const int[2], 2>(marr));
// static_assert (constant_of(^^marr) == mrarr);
