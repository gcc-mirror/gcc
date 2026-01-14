// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::parameters_of.

#include <meta>

using namespace std::meta;

template<class, class> struct same_type;
template<class T> struct same_type<T, T> {};

void fun (int, float, double, char);

consteval void
f ()
{
  auto parms = parameters_of (^^fun);

  same_type<decltype(parms), std::vector<info>>();

  constexpr auto p0 = parameters_of (^^fun)[0];
  static_assert (is_function_parameter (p0));
  constexpr auto p1 = parameters_of (^^fun)[1];
  static_assert (is_function_parameter (p1));
  constexpr auto p2 = parameters_of (^^fun)[2];
  static_assert (is_function_parameter (p2));
  constexpr auto p3 = parameters_of (^^fun)[3];
  static_assert (is_function_parameter (p3));
}

void
g ()
{
  f ();
}
