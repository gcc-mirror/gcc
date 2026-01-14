// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from P3491R3 3.3
// https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2025/p3491r3.html#return-type-and-layering

#include <meta>
#include <ranges>
#include <type_traits>

template <std::size_t N>
struct FixedString {
  char data[N] = {};

  constexpr FixedString (char const (&str) [N]) { std::ranges::copy (str, str + N, data); }
};

template <FixedString S>
struct Test { };

using A = Test <"foo">;
using E = Test <([: std::meta::reflect_constant_string ("foo") :])>;
using F = [: substitute (^^Test, { std::meta::reflect_constant_string ("foo") }) :];

static_assert (std::is_same_v <A, E>);
static_assert (std::is_same_v <A, F>);
