// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::constant_of.

#include <meta>

using namespace std::meta;

const int K = 0;
consteval const int &fn() { return K; }

constexpr auto v = constant_of (reflect_constant (fn ()));
static_assert (is_value (v) && !is_object (v));
static_assert (type_of (v) == ^^int);
static_assert (!is_variable (v));
static_assert (v == reflect_constant (0));

template<int N>
constexpr int V = N;

struct W {
  static constexpr int I = 42;
};

// clang++ doesn't accept this.  That seems wrong.
constexpr auto v2 = constant_of (^^V<3>);
static_assert (is_value (v2) && !is_object (v2));
static_assert (type_of (v2) == ^^int);
static_assert (!is_variable (v2));
static_assert (v2 == reflect_constant (3));

constexpr auto v3 = constant_of (^^W::I);
static_assert (is_value (v3) && !is_object (v3));
static_assert (type_of (v3) == ^^int);
static_assert (!is_variable (v3));
static_assert (v3 == reflect_constant (42));

constexpr auto v4 = constant_of (reflect_constant (42));
static_assert (is_value (v4) && !is_object (v4));
static_assert (type_of (v4) == ^^int);
static_assert (!is_variable (v4));
static_assert (v4 == reflect_constant (42));

template<info R>
void
f ()
{
  constexpr auto v = constant_of (R);
  static_assert (is_value (v) && !is_object (v));
  static_assert (type_of (v) == ^^int);
  static_assert (!is_variable (v));
  static_assert (v == reflect_constant (42));
}

void
g ()
{
  constexpr int b = 42;
  f<^^b>();
}
