// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant_array.

#include <meta>
#include <ranges>

constexpr int x[]{1,2,3,4,5};
constexpr int y[]{11,12,13,14,15};
constexpr auto as_pair = []<typename T1, typename T2>(const std::tuple<T1, T2>& t) static
{ return std::pair<T1, T2>(t); };

constexpr std::span spn = std::define_static_array(std::views::zip (x, y) | std::views::transform (as_pair));
// FIXME these should pass
// static_assert (^^decltype(spn) == ^^std::span<const std::pair<int, int>>);
// static_assert (spn[2].first == 3);
// static_assert (spn[2].second == 13);
