// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::variable_of.

#include <meta>

using namespace std::meta;

consteval int fn(int p) {
  constexpr auto rp = parameters_of(^^fn)[0];
  constexpr auto rv = variable_of(rp);
  static_assert(is_function_parameter(rp));
  static_assert(!is_function_parameter(rv));
  static_assert(!is_variable(rp));
  static_assert(is_variable(rv));
  static_assert(^^p == rv);

  return [:variable_of(parameters_of(^^fn)[0]):];
}

static_assert(fn(42) == 42);
