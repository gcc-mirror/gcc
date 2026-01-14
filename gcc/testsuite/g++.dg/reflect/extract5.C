// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::extract.

#include <meta>

consteval int
foo ()
{
  int arg = 4;
  return std::meta::extract<int>(^^arg);
}

static_assert (foo () == 4);

consteval int
bar (int arg)
{
  return std::meta::extract<int>(^^arg);
}

static_assert (bar (4) == 4);

consteval int
baz ()
{
  constexpr int arg = 4;
  return std::meta::extract<int>(^^arg);
}

static_assert (baz () == 4);
