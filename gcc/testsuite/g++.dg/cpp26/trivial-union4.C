// P3074R7 - trivial unions (was std::uninitialized<T>)
// P3726R2 - Adjustments to Union Lifetime Rules
// { dg-do run { target c++26 } }

namespace std {
  template <class T>
  constexpr void
  start_lifetime (T &r) noexcept
  {
    __builtin_start_lifetime (__builtin_addressof (r));
  }
}

struct S { int a; };
union U { int a; S b[3]; };
union V { int a; U b[2]; };
struct W { S a[2]; };

consteval
{
  S s;
  s.a = 42;
  std::start_lifetime (s);
  if (s.a != 42)
    throw 1;
  U u;
  u.b[0].a = 42;
  std::start_lifetime (u.b);
  std::start_lifetime (u.b[0]);
  std::start_lifetime (u.b[2]);
  u.a = 42;
  std::start_lifetime (u.b);
  std::start_lifetime (u.b[0]);
  std::start_lifetime (u.b[1]);
  V v;
  v.a = 42;
  std::start_lifetime (v.b);
  std::start_lifetime (v.b[1]);
  v.b[1].a = 42;
  std::start_lifetime (v.b[1].b);
  std::start_lifetime (v.b[1].b[1]);
  v.b[1].b[1].a = 43;
  std::start_lifetime (v.b);
  std::start_lifetime (v.b[1].b[1]);
  if (v.b[1].b[1].a != 43)
    throw 2;
  const S cs = { 42 };
  std::start_lifetime (cs);
  if (cs.a != 42)
    throw 3;
  const U cu = { .b = { { 41 }, { 42 }, { 43 } } };
  std::start_lifetime (cu.b);
  std::start_lifetime (cu.b[0]);
  std::start_lifetime (cu.b[2]);
  if (cu.b[2].a != 43)
    throw 4;
  const V cv = { .b = { { .b = { { 44 } } } } };
  std::start_lifetime (cv.b);
  std::start_lifetime (cv.b[0]);
  std::start_lifetime (cv.b[0].b);
  std::start_lifetime (cv.b[0].b[0]);
  if (cv.b[0].b[0].a != 44)
    throw 5;
}

int
main ()
{
  W w;
  int i = 0;
  __builtin_start_lifetime (&w.a[++i]);
  if (i != 1)
    __builtin_abort ();
}
