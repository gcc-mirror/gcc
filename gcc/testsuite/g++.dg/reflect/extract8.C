// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::extract.

#include <meta>

using namespace std::meta;

template<auto E>
consteval bool
check_val (info R)
{
  return extract<decltype (E)>(R) == E; // { dg-error "accessing .x. outside its lifetime" }
}
constexpr auto r = (check_val<42>([]() {
  constexpr int x = 42;
  return ^^x;
}()));
