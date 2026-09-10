// P3450R1 - Extend std::is_within_lifetime
// { dg-do compile { target c++20 } }

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

struct A {};
struct B : A {};
struct C { B a; };
union D { A a[3]; B b[3]; C c[3]; };

consteval int
foo ()
{
  union D d;
  if (std::is_within_lifetime (&d.a[0]))
    return __LINE__;
  if (std::is_within_lifetime (&d.b[1]))
    return __LINE__;
  if (std::is_within_lifetime (&d.c[2]))
    return __LINE__;
  if (std::is_within_lifetime (&d.c[2].a))
    return __LINE__;
  std::start_lifetime (d.a);
  std::start_lifetime (d.a[0]);
  if (!std::is_within_lifetime (&d.a[0]))
    return __LINE__;
  if (std::is_within_lifetime (&d.b[1]))
    return __LINE__;
  if (std::is_within_lifetime (&d.c[2]))
    return __LINE__;
  if (std::is_within_lifetime (&d.c[2].a))
    return __LINE__;
  std::start_lifetime (d.b);
  std::start_lifetime (d.b[1]);
  if (std::is_within_lifetime (&d.a[0]))
    return __LINE__;
  if (!std::is_within_lifetime (&d.b[1]))
    return __LINE__;
  if (std::is_within_lifetime (&d.c[2]))
    return __LINE__;
  if (std::is_within_lifetime (&d.c[2].a))
    return __LINE__;
  std::start_lifetime (d.c);
  std::start_lifetime (d.c[2]);
  if (std::is_within_lifetime (&d.a[0]))
    return __LINE__;
  if (std::is_within_lifetime (&d.b[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&d.c[2]))
    return __LINE__;
  if (std::is_within_lifetime (&d.c[2].a))
    return __LINE__;
  std::start_lifetime (d.c[2].a);
  if (std::is_within_lifetime (&d.a[0]))
    return __LINE__;
  if (std::is_within_lifetime (&d.b[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&d.c[2]))
    return __LINE__;
  if (!std::is_within_lifetime (&d.c[2].a))
    return __LINE__;
  if (std::is_within_lifetime (&d.c[1]))
    return __LINE__;
  if (std::is_within_lifetime (&d.c[1].a))
    return __LINE__;
  std::start_lifetime (d.c[1]);
  if (!std::is_within_lifetime (&d.c[1]))
    return __LINE__;
  if (std::is_within_lifetime (&d.c[1].a))
    return __LINE__;
  return 0;
}

static_assert (foo () == 0);

consteval int
bar ()
{
  union D d;
  std::start_lifetime (d.c);
  std::start_lifetime (d.c[1]);
  d.c[1].a = {};
  if (!std::is_within_lifetime (&d.c[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&d.c[1].a))
    return __LINE__;
  return 0;
}

static_assert (bar () == 0);		// { dg-bogus "static assertion failed" "" { xfail *-*-* } }
