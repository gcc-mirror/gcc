// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant_array.

#include <meta>
#include <ranges>

using namespace std::meta;

constexpr int x[]{1,2,3,4,5};
constexpr int y[]{11,12,13,14,15};
constexpr auto as_pair = []<typename T1, typename T2>(const std::tuple<T1, T2>& t) static
{ return std::pair<T1, T2>(t); };

constexpr info r = reflect_constant_array(std::views::zip (x, y) | std::views::transform (as_pair));
// FIXME this should be pass
// static_assert (type_of (^^r) == ^^const std::pair<int, int>[5]);
// static_assert ([: r :][2].first == 3);
// static_assert ([: r :][2].second == 13);

