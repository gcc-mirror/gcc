// P3450R1 - Extend std::is_within_lifetime
// { dg-do compile { target c++20 } }

#include "../cpp2a/construct_at.h"

namespace std {
  template <class T>
  consteval bool
  is_within_lifetime (const T *p) noexcept
  {
    return __builtin_is_within_lifetime (p);
  }
}

consteval bool
foo (int &x, char &y)
{
  if (std::is_within_lifetime (&x) || std::is_within_lifetime (&y))
    return false;
  std::construct_at (&y, 42);
  if (std::is_within_lifetime (&x) || !std::is_within_lifetime (&y))
    return false;
  std::construct_at (&x, 41);
  if (!std::is_within_lifetime (&x) || std::is_within_lifetime (&y))
    return false;
  return true;
}

static_assert ([] { union { int a; char b; } u; return foo (u.a, u.b); } ());
static_assert ([] { union { int a; char b; }; return foo (a, b); } ());
static_assert ([] { struct { union { int a; char b; }; } s; return foo (s.a, s.b); } ());
static_assert ([] { struct { union { int a; char b; } u; } s; return foo (s.u.a, s.u.b); } ());

consteval bool
bar ()
{
  union { union { int a; long b; } c; short d; };
  if (std::is_within_lifetime (&d)
      || std::is_within_lifetime (&c)
      || std::is_within_lifetime (&c.a)
      || std::is_within_lifetime (&c.b))
    return false;
  std::construct_at (&d);
  if (!std::is_within_lifetime (&d)
      || std::is_within_lifetime (&c)
      || std::is_within_lifetime (&c.a)
      || std::is_within_lifetime (&c.b))
    return false;
  std::construct_at (&c);
  std::construct_at (&c.b);
  if (std::is_within_lifetime (&d)
      || !std::is_within_lifetime (&c)
      || std::is_within_lifetime (&c.a)
      || !std::is_within_lifetime (&c.b))
    return false;
  std::construct_at (&c.a);
  if (std::is_within_lifetime (&d)
      || !std::is_within_lifetime (&c)
      || !std::is_within_lifetime (&c.a)
      || std::is_within_lifetime (&c.b))
    return false;
  return true;
}

static_assert (bar ());
