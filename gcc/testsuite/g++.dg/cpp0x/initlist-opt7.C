// PR c++/118285
// { dg-do compile { target c++20 } }

#include <initializer_list>

struct A {
  char *a;
  union { char b[8]; long c; };
  constexpr A (const char *x) : a(b)
  {
    for (int i = 0; i < 8; ++i)
      b[i] = 0;
  }
  constexpr ~A ()
  {
    if (!foo ())
      bar (c);
  }
  constexpr bool foo ()
  {
    char *x = a;
    if (x == b)
      return true;
    return false;
  }
  constexpr void bar (long) {}
};

constexpr void
baz (std::initializer_list<A>)
{
}

constexpr bool
qux ()
{
  baz ({""});
  return true;
}

static_assert (qux (), "");
