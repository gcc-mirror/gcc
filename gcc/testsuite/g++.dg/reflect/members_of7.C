// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::members_of.

#include <meta>

using namespace std::meta;

constexpr access_context uctx = access_context::unchecked ();

namespace N
{
}

consteval std::size_t
foo ()
{
  return members_of (^^N, uctx).size ();
}

namespace N
{
  static_assert (foo () == 0);
  int a;
  static_assert (foo () == 1);
  void bar (int);
  static_assert (foo () == 2);
  void baz (long);
  static_assert (foo () == 3);
  void baz (short, float);
  static_assert (foo () == 4);
}
