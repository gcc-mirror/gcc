// P3450R1 - Extend std::is_within_lifetime
// { dg-do compile { target c++20 } }

#if __has_builtin(__builtin_is_within_lifetime)
namespace std {
  template <class T>
  consteval bool
  is_within_lifetime (const T *p) noexcept
  {
    return __builtin_is_within_lifetime (p);
  }
}
#endif

namespace std {
  template <typename T, typename F>
  constexpr T
  bit_cast (const F &f) noexcept
  {
    return __builtin_bit_cast (T, f);
  }
}

constexpr char d = 0;
struct E { char a; union F { int b; long c; short d; struct G { int e; } f[2]; } g; };
constexpr E e = {};
constexpr E f = { .a = 1, .g = { .f = {} } };
constexpr int s = 42;
constexpr int t[2] = {};

consteval int
foo ()
{
  char a = 0;
  struct B { int b; } b = {};
  struct C : B { int c[2]; union D { int d; long e; } f; } c = {};
  union H { union I { int a; union J { int b; long c; } d; } e; long f; } g = {};
  H h;
  C i;
  char j;
  bool k = true;
  if (!std::is_within_lifetime (&a))
    return __LINE__;
  if (!std::is_within_lifetime (&b))
    return __LINE__;
  if (!std::is_within_lifetime (&b.b))
    return __LINE__;
  if (!std::is_within_lifetime (&c))
    return __LINE__;
  if (!std::is_within_lifetime (&c.b))
    return __LINE__;
  if (!std::is_within_lifetime (&c.c))
    return __LINE__;
  if (!std::is_within_lifetime (&c.c[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&c.f))
    return __LINE__;
  if (!std::is_within_lifetime (&c.f.d))
    return __LINE__;
  if (std::is_within_lifetime (&c.f.e))
    return __LINE__;
  if (!std::is_within_lifetime (&d))
    return __LINE__;
  if (!std::is_within_lifetime (&e))
    return __LINE__;
  if (!std::is_within_lifetime (&e.a))
    return __LINE__;
  if (!std::is_within_lifetime (&e.g))
    return __LINE__;
  if (!std::is_within_lifetime (&e.g.b))
    return __LINE__;
  if (std::is_within_lifetime (&e.g.c))
    return __LINE__;
  if (std::is_within_lifetime (&e.g.d))
    return __LINE__;
  if (std::is_within_lifetime (&e.g.f))
    return __LINE__;
  if (!std::is_within_lifetime (&f))
    return __LINE__;
  if (!std::is_within_lifetime (&f.a))
    return __LINE__;
  if (!std::is_within_lifetime (&f.g))
    return __LINE__;
  if (std::is_within_lifetime (&f.g.b))
    return __LINE__;
  if (std::is_within_lifetime (&f.g.c))
    return __LINE__;
  if (std::is_within_lifetime (&f.g.d))
    return __LINE__;
  if (!std::is_within_lifetime (&f.g.f))
    return __LINE__;
  if (!std::is_within_lifetime (&f.g.f[0]))
    return __LINE__;
  if (!std::is_within_lifetime (&f.g.f[0].e))
    return __LINE__;
  if (!std::is_within_lifetime (&f.g.f[1].e))
    return __LINE__;
  if (!std::is_within_lifetime (&g))
    return __LINE__;
  if (!std::is_within_lifetime (&g.e))
    return __LINE__;
  if (std::is_within_lifetime (&g.f))
    return __LINE__;
  if (!std::is_within_lifetime (&g.e.a))
    return __LINE__;
  if (std::is_within_lifetime (&g.e.d))
    return __LINE__;
  g.e.d.c = 1;
  if (!std::is_within_lifetime (&g))
    return __LINE__;
  if (!std::is_within_lifetime (&g.e))
    return __LINE__;
  if (std::is_within_lifetime (&g.f))
    return __LINE__;
  if (std::is_within_lifetime (&g.e.a))
    return __LINE__;
  if (!std::is_within_lifetime (&g.e.d))
    return __LINE__;
  if (std::is_within_lifetime (&g.e.d.b))
    return __LINE__;
  if (!std::is_within_lifetime (&g.e.d.c))
    return __LINE__;
  g.f = 42;
  if (!std::is_within_lifetime (&g))
    return __LINE__;
  if (std::is_within_lifetime (&g.e))
    return __LINE__;
  if (!std::is_within_lifetime (&g.f))
    return __LINE__;
  if (!std::is_within_lifetime (&h))
    return __LINE__;
  if (std::is_within_lifetime (&h.e))
    return __LINE__;
  if (std::is_within_lifetime (&h.f))
    return __LINE__;
  if (!std::is_within_lifetime (&i))
    return __LINE__;
  if (!std::is_within_lifetime (&i.b))
    return __LINE__;
  if (!std::is_within_lifetime (&i.c[1]))
    return __LINE__;
  if (!std::is_within_lifetime (&i.f))
    return __LINE__;
  if (std::is_within_lifetime (&i.f.d))
    return __LINE__;
  if (std::is_within_lifetime (&i.f.e))
    return __LINE__;
  if (!std::is_within_lifetime (&j))
    return __LINE__;
  if (!std::is_within_lifetime (&k))
    return __LINE__;
  unsigned char l = std::bit_cast <unsigned char> (k);
  if (!std::is_within_lifetime (&l))
    return __LINE__;
  struct {} m;
  if (!std::is_within_lifetime (&m))
    return __LINE__;
  unsigned char n = std::bit_cast <unsigned char> (m);
  if (!std::is_within_lifetime (&n))
    return __LINE__;
  int o;
  if (!std::is_within_lifetime (static_cast <volatile int *> (&o)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&o)))
    return __LINE__;
  volatile int p;
  if (!std::is_within_lifetime (const_cast <int *> (&p)))
    return __LINE__;
  struct { union { union { struct { int a; long b; } c; int d; } e; int f; } g; int h; } q;
  if (!std::is_within_lifetime (&q))
    return __LINE__;
  if (!std::is_within_lifetime (&q.g))
    return __LINE__;
  if (!std::is_within_lifetime (&q.h))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.f))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.c))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.d))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.c.a))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.c.b))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.g)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.h)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.f)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.c)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.d)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.c.a)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.c.b)))
    return __LINE__;
  q.g.f = 0;
  if (!std::is_within_lifetime (&q))
    return __LINE__;
  if (!std::is_within_lifetime (&q.g))
    return __LINE__;
  if (!std::is_within_lifetime (&q.h))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e))
    return __LINE__;
  if (!std::is_within_lifetime (&q.g.f))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.c))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.d))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.c.a))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.c.b))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.g)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.h)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.g.f)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.c)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.d)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.c.a)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.c.b)))
    return __LINE__;
  q.g.e.d = 0;
  if (!std::is_within_lifetime (&q))
    return __LINE__;
  if (!std::is_within_lifetime (&q.g))
    return __LINE__;
  if (!std::is_within_lifetime (&q.h))
    return __LINE__;
  if (!std::is_within_lifetime (&q.g.e))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.f))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.c))
    return __LINE__;
  if (!std::is_within_lifetime (&q.g.e.d))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.c.a))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.c.b))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.g)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.h)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.g.e)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.f)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.c)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.g.e.d)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.c.a)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.c.b)))
    return __LINE__;
  q.g.e.c.a = 0;
  if (!std::is_within_lifetime (&q))
    return __LINE__;
  if (!std::is_within_lifetime (&q.g))
    return __LINE__;
  if (!std::is_within_lifetime (&q.h))
    return __LINE__;
  if (!std::is_within_lifetime (&q.g.e))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.f))
    return __LINE__;
  if (!std::is_within_lifetime (&q.g.e.c))
    return __LINE__;
  if (std::is_within_lifetime (&q.g.e.d))
    return __LINE__;
  if (!std::is_within_lifetime (&q.g.e.c.a))
    return __LINE__;
  if (!std::is_within_lifetime (&q.g.e.c.b))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.g)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.h)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.g.e)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.f)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.g.e.c)))
    return __LINE__;
  if (std::is_within_lifetime (static_cast <void *> (&q.g.e.d)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.g.e.c.a)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <void *> (&q.g.e.c.b)))
    return __LINE__;
  struct { union { int a; short b; }; mutable int c; } r = { .b = 42 };
  if (!std::is_within_lifetime (&r))
    return __LINE__;
  if (std::is_within_lifetime (&r.a))
    return __LINE__;
  if (!std::is_within_lifetime (&r.b))
    return __LINE__;
  if (!std::is_within_lifetime (&r.c))
    return __LINE__;
  if (!std::is_within_lifetime (&s))
    return __LINE__;
  if (!std::is_within_lifetime (const_cast <int *> (&s)))
    return __LINE__;
  if (!std::is_within_lifetime (const_cast <volatile int *> (&s)))
    return __LINE__;
  if (!std::is_within_lifetime (static_cast <const void *> (&s)))
    return __LINE__;
  if (!std::is_within_lifetime (t))
    return __LINE__;
  if (!std::is_within_lifetime (t + 0))
    return __LINE__;
  if (!std::is_within_lifetime (t + 1))
    return __LINE__;
  int u[4];
  if (!std::is_within_lifetime (&u))
    return __LINE__;
  if (!std::is_within_lifetime (&u[2]))
    return __LINE__;
  u[1] = 42;
  if (!std::is_within_lifetime (&u))
    return __LINE__;
  if (!std::is_within_lifetime (&u[2]))
    return __LINE__;
  return 0;
}

static_assert (foo () == 0);
