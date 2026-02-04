// PR c++/123913
// PR c++/123964
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

using I = long;
void foo (int, I) {}
using bar = void (int, I);

constexpr auto a = std::meta::reflect_constant_array (parameters_of (^^foo));
constexpr auto b = std::meta::reflect_constant_array (parameters_of (^^bar));
static_assert (is_function_parameter (parameters_of (^^foo)[0]));
static_assert (is_function_parameter (parameters_of (^^foo)[1]));
static_assert (parameters_of (^^bar)[0] == ^^int);
static_assert (parameters_of (^^bar)[1] == ^^long);
