/* PR target/92904 */

#include <stdarg.h>

struct S { long long a, b; };
struct __attribute__((aligned (16))) T { long long a, b; };
struct U { double a, b, c, d; };
struct __attribute__((aligned (32))) V { double a, b, c, d; };
struct W { double a; long long b; };
struct __attribute__((aligned (16))) X { double a; long long b; };
#if __SIZEOF_INT128__ == 2 * __SIZEOF_LONG_LONG__
__int128 b;
#endif
struct S c;
struct T d;
struct U e;
struct V f;
struct W g;
struct X h;

#if __SIZEOF_INT128__ == 2 * __SIZEOF_LONG_LONG__
__attribute__((noipa)) __int128
f1 (int x, ...)
{
  __int128 r;
  va_list ap;
  va_start (ap, x);
  while (x--)
    va_arg (ap, int);
  r = va_arg (ap, __int128);
  va_end (ap);
  return r;
}
#endif

__attribute__((noipa)) struct S
f2 (int x, ...)
{
  struct S r;
  va_list ap;
  va_start (ap, x);
  while (x--)
    va_arg (ap, int);
  r = va_arg (ap, struct S);
  va_end (ap);
  return r;
}

__attribute__((noipa)) struct T
f3 (int x, ...)
{
  struct T r;
  va_list ap;
  va_start (ap, x);
  while (x--)
    va_arg (ap, int);
  r = va_arg (ap, struct T);
  va_end (ap);
  return r;
}

#if __SIZEOF_INT128__ == 2 * __SIZEOF_LONG_LONG__
__attribute__((noipa)) void
f4 (int x, ...)
{
  va_list ap;
  va_start (ap, x);
  while (x--)
    va_arg (ap, int);
  b = va_arg (ap, __int128);
  va_end (ap);
}
#endif

__attribute__((noipa)) void
f5 (int x, ...)
{
  va_list ap;
  va_start (ap, x);
  while (x--)
    va_arg (ap, int);
  c = va_arg (ap, struct S);
  va_end (ap);
}

__attribute__((noipa)) void
f6 (int x, ...)
{
  va_list ap;
  va_start (ap, x);
  while (x--)
    va_arg (ap, int);
  d = va_arg (ap, struct T);
  va_end (ap);
}

__attribute__((noipa)) struct U
f7 (int x, ...)
{
  struct U r;
  va_list ap;
  va_start (ap, x);
  while (x--)
    va_arg (ap, double);
  r = va_arg (ap, struct U);
  va_end (ap);
  return r;
}

__attribute__((noipa)) struct V
f8 (int x, ...)
{
  struct V r;
  va_list ap;
  va_start (ap, x);
  while (x--)
    va_arg (ap, double);
  r = va_arg (ap, struct V);
  va_end (ap);
  return r;
}

__attribute__((noipa)) void
f9 (int x, ...)
{
  va_list ap;
  va_start (ap, x);
  while (x--)
    va_arg (ap, double);
  e = va_arg (ap, struct U);
  va_end (ap);
}

__attribute__((noipa)) void
f10 (int x, ...)
{
  va_list ap;
  va_start (ap, x);
  while (x--)
    va_arg (ap, double);
  f = va_arg (ap, struct V);
  va_end (ap);
}

__attribute__((noipa)) struct W
f11 (int x, ...)
{
  struct W r;
  va_list ap;
  va_start (ap, x);
  while (x--)
    {
      va_arg (ap, int);
      va_arg (ap, double);
    }
  r = va_arg (ap, struct W);
  va_end (ap);
  return r;
}

__attribute__((noipa)) struct X
f12 (int x, ...)
{
  struct X r;
  va_list ap;
  va_start (ap, x);
  while (x--)
    {
      va_arg (ap, int);
      va_arg (ap, double);
    }
  r = va_arg (ap, struct X);
  va_end (ap);
  return r;
}

__attribute__((noipa)) void
f13 (int x, ...)
{
  va_list ap;
  va_start (ap, x);
  while (x--)
    {
      va_arg (ap, int);
      va_arg (ap, double);
    }
  g = va_arg (ap, struct W);
  va_end (ap);
}

__attribute__((noipa)) void
f14 (int x, ...)
{
  va_list ap;
  va_start (ap, x);
  while (x--)
    {
      va_arg (ap, int);
      va_arg (ap, double);
    }
  h = va_arg (ap, struct X);
  va_end (ap);
}

int
main ()
{
  union Y {
#if __SIZEOF_INT128__ == 2 * __SIZEOF_LONG_LONG__
    __int128 b;
#endif
    struct S c;
    struct T d;
    struct U e;
    struct V f;
    struct W g;
    struct X h;
  } u, v;
  u.c.a = 0x5555555555555555ULL;
  u.c.b = 0xaaaaaaaaaaaaaaaaULL;
#define C(x) \
  do {								\
    if (u.c.a != x.c.a || u.c.b != x.c.b) __builtin_abort ();	\
    u.c.a++;							\
    u.c.b--;							\
  } while (0)
#if __SIZEOF_INT128__ == 2 * __SIZEOF_LONG_LONG__
  v.b = f1 (0, u.b); C (v);
  v.b = f1 (1, 0, u.b); C (v);
  v.b = f1 (2, 0, 0, u.b); C (v);
  v.b = f1 (3, 0, 0, 0, u.b); C (v);
  v.b = f1 (4, 0, 0, 0, 0, u.b); C (v);
  v.b = f1 (5, 0, 0, 0, 0, 0, u.b); C (v);
  v.b = f1 (6, 0, 0, 0, 0, 0, 0, u.b); C (v);
  v.b = f1 (7, 0, 0, 0, 0, 0, 0, 0, u.b); C (v);
  v.b = f1 (8, 0, 0, 0, 0, 0, 0, 0, 0, u.b); C (v);
  v.b = f1 (9, 0, 0, 0, 0, 0, 0, 0, 0, 0, u.b); C (v);
#endif
  v.c = f2 (0, u.c); C (v);
  v.c = f2 (1, 0, u.c); C (v);
  v.c = f2 (2, 0, 0, u.c); C (v);
  v.c = f2 (3, 0, 0, 0, u.c); C (v);
  v.c = f2 (4, 0, 0, 0, 0, u.c); C (v);
  v.c = f2 (5, 0, 0, 0, 0, 0, u.c); C (v);
  v.c = f2 (6, 0, 0, 0, 0, 0, 0, u.c); C (v);
  v.c = f2 (7, 0, 0, 0, 0, 0, 0, 0, u.c); C (v);
  v.c = f2 (8, 0, 0, 0, 0, 0, 0, 0, 0, u.c); C (v);
  v.c = f2 (9, 0, 0, 0, 0, 0, 0, 0, 0, 0, u.c); C (v);
  v.d = f3 (0, u.d); C (v);
  v.d = f3 (1, 0, u.d); C (v);
  v.d = f3 (2, 0, 0, u.d); C (v);
  v.d = f3 (3, 0, 0, 0, u.d); C (v);
  v.d = f3 (4, 0, 0, 0, 0, u.d); C (v);
  v.d = f3 (5, 0, 0, 0, 0, 0, u.d); C (v);
  v.d = f3 (6, 0, 0, 0, 0, 0, 0, u.d); C (v);
  v.d = f3 (7, 0, 0, 0, 0, 0, 0, 0, u.d); C (v);
  v.d = f3 (8, 0, 0, 0, 0, 0, 0, 0, 0, u.d); C (v);
  v.d = f3 (9, 0, 0, 0, 0, 0, 0, 0, 0, 0, u.d); C (v);
#if __SIZEOF_INT128__ == 2 * __SIZEOF_LONG_LONG__
  f4 (0, u.b); v.b = b; C (v);
  f4 (1, 0, u.b); v.b = b; C (v);
  f4 (2, 0, 0, u.b); v.b = b; C (v);
  f4 (3, 0, 0, 0, u.b); v.b = b; C (v);
  f4 (4, 0, 0, 0, 0, u.b); v.b = b; C (v);
  f4 (5, 0, 0, 0, 0, 0, u.b); v.b = b; C (v);
  f4 (6, 0, 0, 0, 0, 0, 0, u.b); v.b = b; C (v);
  f4 (7, 0, 0, 0, 0, 0, 0, 0, u.b); v.b = b; C (v);
  f4 (8, 0, 0, 0, 0, 0, 0, 0, 0, u.b); v.b = b; C (v);
  f4 (9, 0, 0, 0, 0, 0, 0, 0, 0, 0, u.b); v.b = b; C (v);
#endif
  f5 (0, u.c); v.c = c; C (v);
  f5 (1, 0, u.c); v.c = c; C (v);
  f5 (2, 0, 0, u.c); v.c = c; C (v);
  f5 (3, 0, 0, 0, u.c); v.c = c; C (v);
  f5 (4, 0, 0, 0, 0, u.c); v.c = c; C (v);
  f5 (5, 0, 0, 0, 0, 0, u.c); v.c = c; C (v);
  f5 (6, 0, 0, 0, 0, 0, 0, u.c); v.c = c; C (v);
  f5 (7, 0, 0, 0, 0, 0, 0, 0, u.c); v.c = c; C (v);
  f5 (8, 0, 0, 0, 0, 0, 0, 0, 0, u.c); v.c = c; C (v);
  f5 (9, 0, 0, 0, 0, 0, 0, 0, 0, 0, u.c); v.c = c; C (v);
  f6 (0, u.d); v.d = d; C (v);
  f6 (1, 0, u.d); v.d = d; C (v);
  f6 (2, 0, 0, u.d); v.d = d; C (v);
  f6 (3, 0, 0, 0, u.d); v.d = d; C (v);
  f6 (4, 0, 0, 0, 0, u.d); v.d = d; C (v);
  f6 (5, 0, 0, 0, 0, 0, u.d); v.d = d; C (v);
  f6 (6, 0, 0, 0, 0, 0, 0, u.d); v.d = d; C (v);
  f6 (7, 0, 0, 0, 0, 0, 0, 0, u.d); v.d = d; C (v);
  f6 (8, 0, 0, 0, 0, 0, 0, 0, 0, u.d); v.d = d; C (v);
  f6 (9, 0, 0, 0, 0, 0, 0, 0, 0, 0, u.d); v.d = d; C (v);
  u.e.a = 1.25;
  u.e.b = 2.75;
  u.e.c = -3.5;
  u.e.d = -2.0;
#undef C
#define C(x) \
  do {								\
    if (u.e.a != x.e.a || u.e.b != x.e.b			\
	|| u.e.c != x.e.c || u.e.d != x.e.d) __builtin_abort ();\
    u.e.a++;							\
    u.e.b--;							\
    u.e.c++;							\
    u.e.d--;							\
  } while (0)
  v.e = f7 (0, u.e); C (v);
  v.e = f7 (1, 0.0, u.e); C (v);
  v.e = f7 (2, 0.0, 0.0, u.e); C (v);
  v.e = f7 (3, 0.0, 0.0, 0.0, u.e); C (v);
  v.e = f7 (4, 0.0, 0.0, 0.0, 0.0, u.e); C (v);
  v.e = f7 (5, 0.0, 0.0, 0.0, 0.0, 0.0, u.e); C (v);
  v.e = f7 (6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.e); C (v);
  v.e = f7 (7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.e); C (v);
  v.e = f7 (8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.e); C (v);
  v.e = f7 (9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.e); C (v);
  v.f = f8 (0, u.f); C (v);
  v.f = f8 (1, 0.0, u.f); C (v);
  v.f = f8 (2, 0.0, 0.0, u.f); C (v);
  v.f = f8 (3, 0.0, 0.0, 0.0, u.f); C (v);
  v.f = f8 (4, 0.0, 0.0, 0.0, 0.0, u.f); C (v);
  v.f = f8 (5, 0.0, 0.0, 0.0, 0.0, 0.0, u.f); C (v);
  v.f = f8 (6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.f); C (v);
  v.f = f8 (7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.f); C (v);
  v.f = f8 (8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.f); C (v);
  v.f = f8 (9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.f); C (v);
  f9 (0, u.e); v.e = e; C (v);
  f9 (1, 0.0, u.e); v.e = e; C (v);
  f9 (2, 0.0, 0.0, u.e); v.e = e; C (v);
  f9 (3, 0.0, 0.0, 0.0, u.e); v.e = e; C (v);
  f9 (4, 0.0, 0.0, 0.0, 0.0, u.e); v.e = e; C (v);
  f9 (5, 0.0, 0.0, 0.0, 0.0, 0.0, u.e); v.e = e; C (v);
  f9 (6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.e); v.e = e; C (v);
  f9 (7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.e); v.e = e; C (v);
  f9 (8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.e); v.e = e; C (v);
  f9 (9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.e); v.e = e; C (v);
  f10 (0, u.f); v.f = f; C (v);
  f10 (1, 0.0, u.f); v.f = f; C (v);
  f10 (2, 0.0, 0.0, u.f); v.f = f; C (v);
  f10 (3, 0.0, 0.0, 0.0, u.f); v.f = f; C (v);
  f10 (4, 0.0, 0.0, 0.0, 0.0, u.f); v.f = f; C (v);
  f10 (5, 0.0, 0.0, 0.0, 0.0, 0.0, u.f); v.f = f; C (v);
  f10 (6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.f); v.f = f; C (v);
  f10 (7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.f); v.f = f; C (v);
  f10 (8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.f); v.f = f; C (v);
  f10 (9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u.f); v.f = f; C (v);
  u.g.a = 9.5;
  u.g.b = 0x5555555555555555ULL;
#undef C
#define C(x) \
  do {								\
    if (u.e.a != x.e.a || u.e.b != x.e.b) __builtin_abort ();	\
    u.e.a++;							\
    u.e.b--;							\
  } while (0)
  v.g = f11 (0, u.g); C (v);
  v.g = f11 (1, 0, 0.0, u.g); C (v);
  v.g = f11 (2, 0, 0.0, 0, 0.0, u.g); C (v);
  v.g = f11 (3, 0, 0.0, 0, 0.0, 0, 0.0, u.g); C (v);
  v.g = f11 (4, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); C (v);
  v.g = f11 (5, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); C (v);
  v.g = f11 (6, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); C (v);
  v.g = f11 (7, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); C (v);
  v.g = f11 (8, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); C (v);
  v.g = f11 (9, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); C (v);
  v.h = f12 (0, u.h); C (v);
  v.h = f12 (1, 0, 0.0, u.h); C (v);
  v.h = f12 (2, 0, 0.0, 0, 0.0, u.h); C (v);
  v.h = f12 (3, 0, 0.0, 0, 0.0, 0, 0.0, u.h); C (v);
  v.h = f12 (4, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); C (v);
  v.h = f12 (5, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); C (v);
  v.h = f12 (6, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); C (v);
  v.h = f12 (7, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); C (v);
  v.h = f12 (8, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); C (v);
  v.h = f12 (9, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); C (v);
  f13 (0, u.g); v.g = g; C (v);
  f13 (1, 0, 0.0, u.g); v.g = g; C (v);
  f13 (2, 0, 0.0, 0, 0.0, u.g); v.g = g; C (v);
  f13 (3, 0, 0.0, 0, 0.0, 0, 0.0, u.g); v.g = g; C (v);
  f13 (4, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); v.g = g; C (v);
  f13 (5, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); v.g = g; C (v);
  f13 (6, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); v.g = g; C (v);
  f13 (7, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); v.g = g; C (v);
  f13 (8, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); v.g = g; C (v);
  f13 (9, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.g); v.g = g; C (v);
  f14 (0, u.h); v.h = h; C (v);
  f14 (1, 0, 0.0, u.h); v.h = h; C (v);
  f14 (2, 0, 0.0, 0, 0.0, u.h); v.h = h; C (v);
  f14 (3, 0, 0.0, 0, 0.0, 0, 0.0, u.h); v.h = h; C (v);
  f14 (4, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); v.h = h; C (v);
  f14 (5, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); v.h = h; C (v);
  f14 (6, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); v.h = h; C (v);
  f14 (7, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); v.h = h; C (v);
  f14 (8, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); v.h = h; C (v);
  f14 (9, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, u.h); v.h = h; C (v);
  return 0;
}
