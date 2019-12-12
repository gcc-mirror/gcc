// { dg-do compile { target c++17 } }

#include <cassert>
#include <experimental/type_traits>

auto c = 'c';
auto u8c = u8'c';

#if __cpp_char8_t
static_assert(!std::experimental::is_same_v<decltype(u8c), decltype(c)>, "");
#else
static_assert(std::experimental::is_same_v<decltype(u8c), decltype(c)>, "");
#endif

auto u8s = u8"c";
auto x = u8s[0];

static_assert(std::experimental::is_same_v<decltype(u8c), decltype(x)>, "");
