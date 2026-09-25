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

  template <class T>
  constexpr void
  start_lifetime (T &r) noexcept
  {
    __builtin_start_lifetime (__builtin_addressof (r));
  }
}

struct A { int a, b; };
struct B { A c, d[2]; };
union C { B e; int f; };
struct D { C g; B h; };
struct E { D i[3]; };

consteval int
foo ()
{
  E j;
  if (!std::is_within_lifetime (&j.i[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.f))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e.d[1].b))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].h))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].h.d[0].a))
    return __LINE__;
  std::destroy_at (&j.i[1]);
  if (std::is_within_lifetime (&j.i[1]))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.f))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e.d[1].b))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].h))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].h.d[0].a))
    return __LINE__;
  std::start_lifetime (j.i[1]);
  if (!std::is_within_lifetime (&j.i[1]))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.f))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e.d[1].b))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].h))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].h.d[0].a))
    return __LINE__;
  std::start_lifetime (j.i[1].g);
  if (!std::is_within_lifetime (&j.i[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.f))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e.d[1].b))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].h))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].h.d[0].a))
    return __LINE__;
  std::start_lifetime (j.i[1].h);
  if (!std::is_within_lifetime (&j.i[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.f))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e.d[1].b))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].h))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].h.d[0].a))
    return __LINE__;
  std::start_lifetime (j.i[1].h.d);
  std::start_lifetime (j.i[1].h.d[0]);
  if (std::is_within_lifetime (&j.i[1].h.d[0].a))
    return __LINE__;
  j.i[1].h.d[0].a = 42;
  if (!std::is_within_lifetime (&j.i[1].h))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].h.d[0].a))
    return __LINE__;
  std::start_lifetime (j.i[1].g.e);
  std::start_lifetime (j.i[1].g.e.d);
  std::start_lifetime (j.i[1].g.e.d[1]);
  if (!std::is_within_lifetime (&j.i[1].g.e))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.f))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g.e.d[1]))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e.d[1].b))
    return __LINE__;
  j.i[1].g.e.d[1].b = 42;
  if (!std::is_within_lifetime (&j.i[1].g.e.d[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g.e.d[1].b))
    return __LINE__;
  std::start_lifetime (j.i[1].g);
  if (!std::is_within_lifetime (&j.i[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g.e))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.f))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g.e.d[1].b))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].h))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].h.d[0].a))
    return __LINE__;
  std::start_lifetime (j.i[1].g);
  j.i[1].g.f = 42;
  if (!std::is_within_lifetime (&j.i[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g.f))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e.d[1].b))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].h))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].h.d[0].a))
    return __LINE__;
  std::destroy_at (&j.i[1]);
  if (std::is_within_lifetime (&j.i[1]))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.f))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.e.d[1].b))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].h))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].h.d[0].a))
    return __LINE__;
  std::construct_at (&j.i[1]);
  if (!std::is_within_lifetime (&j.i[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g.e))
    return __LINE__;
  if (std::is_within_lifetime (&j.i[1].g.f))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].g.e.d[1].b))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].h))
    return __LINE__;
  if (!std::is_within_lifetime (&j.i[1].h.d[0].a))
    return __LINE__;
  return 0;
}

static_assert (foo () == 0);
