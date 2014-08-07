/* PR tree-optimization/57233 */
/* { dg-do run { target { ilp32 || lp64 } } } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

typedef unsigned V4 __attribute__((vector_size(4 * sizeof (int))));
typedef unsigned V8 __attribute__((vector_size(8 * sizeof (int))));
typedef unsigned V16 __attribute__((vector_size(16 * sizeof (int))));
V4 a, b, g;
V8 c, d, h;
V16 e, f, j;

__attribute__((noinline)) void
f1 (void)
{
  a = (a << 2) | (a >> 30);
}

__attribute__((noinline)) void
f2 (void)
{
  a = (a << 30) | (a >> 2);
}

__attribute__((noinline)) void
f3 (void)
{
  a = (a << b) | (a >> (32 - b));
}

__attribute__((noinline, noclone)) void
f4 (int x)
{
  a = (a << x) | (a >> (32 - x));
}

__attribute__((noinline)) void
f5 (void)
{
  c = (c << 2) | (c >> 30);
}

__attribute__((noinline)) void
f6 (void)
{
  c = (c << 30) | (c >> 2);
}

__attribute__((noinline)) void
f7 (void)
{
  c = (c << d) | (c >> (32 - d));
}

__attribute__((noinline, noclone)) void
f8 (int x)
{
  c = (c << x) | (c >> (32 - x));
}

__attribute__((noinline)) void
f9 (void)
{
  e = (e << 2) | (e >> 30);
}

__attribute__((noinline)) void
f10 (void)
{
  e = (e << 30) | (e >> 2);
}

__attribute__((noinline)) void
f11 (void)
{
  e = (e << f) | (e >> (32 - f));
}

__attribute__((noinline, noclone)) void
f12 (int x)
{
  e = (e << x) | (e >> (32 - x));
}

unsigned
r (void)
{
  static unsigned x = 0xdeadbeefU;
  static unsigned y = 0x12347654U;
  static unsigned z = 0x1a2b3c4dU;
  static unsigned w = 0x87654321U;
  unsigned t = x ^ (x << 11);
  x = y;
  y = z;
  z = w;
  w = w ^ (w >> 19) ^ t ^ (t >> 8);
  return w;
}

void
init (unsigned int *p, int count, int mod)
{
  int i;
  for (i = 0; i < count; i++)
    {
      unsigned int v = r ();
      if (mod)
	v = (v % 31) + 1;
      p[i] = v;
    }
}

void
check (unsigned int *p, unsigned int *q, int count, unsigned int *s, int ss)
{
  int i;
  for (i = 0; i < count; i++)
    {
      if (s)
	ss = s[i];
      if (p[i] != ((q[i] << ss) | (q[i] >> (32 - ss))))
	__builtin_abort ();
    }
}

int
main ()
{
  init ((unsigned int *) &a, 4, 0);
  init ((unsigned int *) &b, 4, 1);
  init ((unsigned int *) &c, 8, 0);
  init ((unsigned int *) &d, 8, 1);
  init ((unsigned int *) &e, 16, 0);
  init ((unsigned int *) &f, 16, 1);
  g = a;
  h = c;
  j = e;
  f1 ();
  f5 ();
  f9 ();
  check ((unsigned int *) &a, (unsigned int *) &g, 4, 0, 2);
  check ((unsigned int *) &c, (unsigned int *) &h, 8, 0, 2);
  check ((unsigned int *) &e, (unsigned int *) &j, 16, 0, 2);
  g = a;
  h = c;
  j = e;
  f2 ();
  f6 ();
  f10 ();
  check ((unsigned int *) &a, (unsigned int *) &g, 4, 0, 30);
  check ((unsigned int *) &c, (unsigned int *) &h, 8, 0, 30);
  check ((unsigned int *) &e, (unsigned int *) &j, 16, 0, 30);
  g = a;
  h = c;
  j = e;
  f3 ();
  f7 ();
  f11 ();
  check ((unsigned int *) &a, (unsigned int *) &g, 4, (unsigned int *) &b, 0);
  check ((unsigned int *) &c, (unsigned int *) &h, 8, (unsigned int *) &d, 0);
  check ((unsigned int *) &e, (unsigned int *) &j, 16, (unsigned int *) &f, 0);
  g = a;
  h = c;
  j = e;
  f4 (5);
  f8 (5);
  f12 (5);
  check ((unsigned int *) &a, (unsigned int *) &g, 4, 0, 5);
  check ((unsigned int *) &c, (unsigned int *) &h, 8, 0, 5);
  check ((unsigned int *) &e, (unsigned int *) &j, 16, 0, 5);
  return 0;
}
