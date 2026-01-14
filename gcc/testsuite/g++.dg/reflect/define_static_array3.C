// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant_array.

#include <meta>
#include <ranges>
#include <span>

constexpr int arr[4]{1, 2, 3, 4};
constexpr std::span<const int> spn = std::define_static_array(std::span<const int, 4>(arr));
static_assert ([: std::meta::constant_of(^^arr) :] == spn.data());

constexpr int marr[2][2]{1, 2, 3, 4};
// LWG4483 multidimensional arrays
// constexpr std::span<const int[2]> mspn = std::define_static_array(std::span<const int[2], 2>(marr));
// static_assert ([: std::meta::constant_of(^^marr) :] == mspn.data());
