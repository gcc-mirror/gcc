// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

constexpr auto x = ^^std::meta::type_of;
constexpr auto m = parameters_of(x)[0];
static_assert (std::meta::type_of (m) == dealias (^^std::meta::info));
