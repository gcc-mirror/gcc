// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant.

#include <meta>

using namespace std::meta;

struct S {};
constexpr auto r = reflect_constant (S{});
S s = [:r:];
static_assert (!is_value (r));
static_assert (is_object (r));

struct R { int i; };
constexpr auto rr = reflect_constant (R{42});
static_assert ([:rr:].i == 42);
static_assert (!is_value (rr));
static_assert (is_object (rr));

static_assert (is_value (reflect_constant (nullptr_t{})));
static_assert (!is_object (reflect_constant (nullptr_t{})));
static_assert (is_value (reflect_constant (auto(42))));
static_assert (!is_object (reflect_constant (auto(42))));
constexpr int foo () { return 42; }
static_assert (is_value (reflect_constant (foo ())));
static_assert (!is_object (reflect_constant (foo ())));
static constexpr int i = 0;
static_assert (is_value (reflect_constant (&i)));
static_assert (!is_object (reflect_constant (&i)));
