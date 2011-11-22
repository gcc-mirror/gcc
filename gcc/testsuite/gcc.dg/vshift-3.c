/* { dg-do run } */
/* { dg-options "-O3" } */

#include <stdlib.h>

#define N 64

#ifndef TYPE1
#define TYPE1 int
#define TYPE2 long long
#endif

signed TYPE1 a[N], b, g[N];
unsigned TYPE1 c[N], h[N];
signed TYPE2 d[N], e, j[N];
unsigned TYPE2 f[N], k[N];

#ifndef S
#define S(x) x
#endif

__attribute__((noinline)) void
f1 (void)
{
  int i;
  for (i = 0; i < N; i++)
    g[i] = a[i] << S (b);
}

__attribute__((noinline)) void
f2 (void)
{
  int i;
  for (i = 0; i < N; i++)
    g[i] = a[i] >> S (b);
}

__attribute__((noinline)) void
f3 (void)
{
  int i;
  for (i = 0; i < N; i++)
    h[i] = c[i] >> S (b);
}

__attribute__((noinline)) void
f4 (void)
{
  int i;
  for (i = 0; i < N; i++)
    j[i] = d[i] << S (e);
}

__attribute__((noinline)) void
f5 (void)
{
  int i;
  for (i = 0; i < N; i++)
    j[i] = d[i] >> S (e);
}

__attribute__((noinline)) void
f6 (void)
{
  int i;
  for (i = 0; i < N; i++)
    k[i] = f[i] >> S (e);
}

__attribute__((noinline)) void
f7 (void)
{
  int i;
  for (i = 0; i < N; i++)
    j[i] = d[i] << S (b);
}

__attribute__((noinline)) void
f8 (void)
{
  int i;
  for (i = 0; i < N; i++)
    j[i] = d[i] >> S (b);
}

__attribute__((noinline)) void
f9 (void)
{
  int i;
  for (i = 0; i < N; i++)
    k[i] = f[i] >> S (b);
}

int
main ()
{
  int i;
  b = 7;
  e = 12;
  for (i = 0; i < N; i++)
    {
      asm ("");
      c[i] = (rand () << 1) | (rand () & 1);
      a[i] = c[i];
      d[i] = (rand () << 1) | (rand () & 1);
      d[i] |= (unsigned long long) c[i] << 32;
      f[i] = d[i];
    }
  f1 ();
  f3 ();
  f4 ();
  f6 ();
  for (i = 0; i < N; i++)
    if (g[i] != (signed TYPE1) (a[i] << S (b))
	|| h[i] != (unsigned TYPE1) (c[i] >> S (b))
	|| j[i] != (signed TYPE2) (d[i] << S (e))
	|| k[i] != (unsigned TYPE2) (f[i] >> S (e)))
      abort ();
  f2 ();
  f5 ();
  f9 ();
  for (i = 0; i < N; i++)
    if (g[i] != (signed TYPE1) (a[i] >> S (b))
	|| j[i] != (signed TYPE2) (d[i] >> S (e))
	|| k[i] != (unsigned TYPE2) (f[i] >> S (b)))
      abort ();
  f7 ();
  for (i = 0; i < N; i++)
    if (j[i] != (signed TYPE2) (d[i] << S (b)))
      abort ();
  f8 ();
  for (i = 0; i < N; i++)
    if (j[i] != (signed TYPE2) (d[i] >> S (b)))
      abort ();
  return 0;
}
