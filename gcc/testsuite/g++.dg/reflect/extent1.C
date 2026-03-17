// PR c++/124368
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

using T = int[extent(^^int[4])];
constexpr auto n = extent(^^int[4]);
static_assert (n == 4);
