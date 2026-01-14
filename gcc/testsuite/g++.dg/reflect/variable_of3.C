// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::variable_of.

#include <meta>
#include <utility>

using namespace std::meta;

constexpr int fn(int, int) {
  return [: variable_of (parameters_of (^^fn)[0]) :]
       + [: variable_of (parameters_of (^^fn)[1]) :];
}

static_assert(fn(10, 32) == 42);

template<typename... Args>
constexpr int bar(Args...)
{
  constexpr int vt[] {__integer_pack (sizeof...(Args))...};
  constexpr auto [...ids] = vt;
  return (0 + ... + [: variable_of (parameters_of (^^bar<Args...>)[ids]) :]);
}

static_assert(bar(42) == 42);
static_assert(bar(10, 32) == 42);
static_assert(bar(10, 15, 17) == 42);
static_assert(bar(10, 15, 11, 6) == 42);
