// { dg-do compile }
// { dg-options "-std=c++17" }

#include <cassert>
#include <experimental/type_traits>

auto c = 'c';
auto u8c = u8'c';

static_assert(std::experimental::is_same_v<decltype(u8c), decltype(c)>, "");

auto u8s = u8"c";
auto x = u8s[0];

static_assert(std::experimental::is_same_v<decltype(u8c), decltype(x)>, "");
