// PR c++/124425
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

constexpr auto &s = [: std::meta::reflect_constant_array (std::vector { true, false, true }) :];
static_assert (s[0] && !s[1] && s[2]);
