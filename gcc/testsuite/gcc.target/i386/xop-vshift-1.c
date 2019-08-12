/* { dg-do run } */
/* { dg-options "-O3 -mxop" } */
/* { dg-require-effective-target xop } */

#ifndef CHECK_H
#define CHECK_H "xop-check.h"
#endif

#ifndef TEST
#define TEST xop_test
#endif

#include CHECK_H

#define N 64

#ifndef TYPE1
#define TYPE1 int
#define TYPE2 long long
#endif

signed TYPE1 a[N], b[N], g[N];
unsigned TYPE1 c[N], h[N];
signed TYPE2 d[N], e[N], j[N];
unsigned TYPE2 f[N], k[N];

__attribute__((noinline)) void
f1 (void)
{
  int i;
  for (i = 0; i < N; i++)
    g[i] = a[i] << b[i];
}

__attribute__((noinline)) void
f2 (void)
{
  int i;
  for (i = 0; i < N; i++)
    g[i] = a[i] >> b[i];
}

__attribute__((noinline)) void
f3 (void)
{
  int i;
  for (i = 0; i < N; i++)
    h[i] = c[i] >> b[i];
}

__attribute__((noinline)) void
f4 (void)
{
  int i;
  for (i = 0; i < N; i++)
    j[i] = d[i] << e[i];
}

__attribute__((noinline)) void
f5 (void)
{
  int i;
  for (i = 0; i < N; i++)
    j[i] = d[i] >> e[i];
}

__attribute__((noinline)) void
f6 (void)
{
  int i;
  for (i = 0; i < N; i++)
    k[i] = f[i] >> e[i];
}

__attribute__((noinline)) void
f7 (void)
{
  int i;
  for (i = 0; i < N; i++)
    j[i] = d[i] << b[i];
}

__attribute__((noinline)) void
f8 (void)
{
  int i;
  for (i = 0; i < N; i++)
    j[i] = d[i] >> b[i];
}

__attribute__((noinline)) void
f9 (void)
{
  int i;
  for (i = 0; i < N; i++)
    k[i] = f[i] >> b[i];
}

static void
TEST ()
{
  int i;
  for (i = 0; i < N; i++)
    {
      asm ("");
      c[i] = (rand () << 1) | (rand () & 1);
      b[i] = (i * 85) & (sizeof (TYPE1) * __CHAR_BIT__ - 1);
      a[i] = c[i];
      d[i] = (rand () << 1) | (rand () & 1);
      d[i] |= (unsigned long long) c[i] << 32;
      e[i] = (i * 85) & (sizeof (TYPE2) * __CHAR_BIT__ - 1);
      f[i] = d[i];
    }
  f1 ();
  f3 ();
  f4 ();
  f6 ();
  for (i = 0; i < N; i++)
    if (g[i] != (signed TYPE1) (a[i] << b[i])
	|| h[i] != (unsigned TYPE1) (c[i] >> b[i])
	|| j[i] != (signed TYPE2) (d[i] << e[i])
	|| k[i] != (unsigned TYPE2) (f[i] >> e[i]))
      abort ();
  f2 ();
  f5 ();
  f9 ();
  for (i = 0; i < N; i++)
    if (g[i] != (signed TYPE1) (a[i] >> b[i])
	|| j[i] != (signed TYPE2) (d[i] >> e[i])
	|| k[i] != (unsigned TYPE2) (f[i] >> b[i]))
      abort ();
  f7 ();
  for (i = 0; i < N; i++)
    if (j[i] != (signed TYPE2) (d[i] << b[i]))
      abort ();
  f8 ();
  for (i = 0; i < N; i++)
    if (j[i] != (signed TYPE2) (d[i] >> b[i]))
      abort ();
}
