/* { dg-do run } */
/* { dg-require-effective-target sse2 } */
/* { dg-options "-O3 -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include <stdlib.h>

#define N 512
static short a1[N], a2[N], a3[N];
static unsigned short b1[N], b2[N], b3[N];
static int c1[N], c2[N], c3[N];
static unsigned int d1[N], d2[N], d3[N];
static long long e1[N], e2[N], e3[N];
static unsigned long long g1[N], g2[N], g3[N];

__attribute__((noinline, noclone)) void
f1 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    a1[i] = a2[i] * a3[i];
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    b1[i] = b2[i] * b3[i];
}

__attribute__((noinline, noclone)) void
f3 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    c1[i] = c2[i] * c3[i];
}

__attribute__((noinline, noclone)) void
f4 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    d1[i] = d2[i] * d3[i];
}

__attribute__((noinline, noclone)) void
f5 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    e1[i] = e2[i] * e3[i];
}

__attribute__((noinline, noclone)) void
f6 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    g1[i] = g2[i] * g3[i];
}

__attribute__((noinline, noclone)) void
f7 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    c1[i] = a2[i] * a3[i];
}

__attribute__((noinline, noclone)) void
f8 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    d1[i] = (unsigned int) b2[i] * b3[i];
}

__attribute__((noinline, noclone)) void
f9 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    e1[i] = (long long) c2[i] * (long long) c3[i];
}

__attribute__((noinline, noclone)) void
f10 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    g1[i] = (unsigned long long) d2[i] * (unsigned long long) d3[i];
}

__attribute__((noinline, noclone)) int
f11 (void)
{
  int i, r = 0;
  for (i = 0; i < N; ++i)
    r += a2[i] * a3[i];
  return r;
}

__attribute__((noinline, noclone)) unsigned int
f12 (void)
{
  int i;
  unsigned r = 0;
  for (i = 0; i < N; ++i)
    r += (unsigned int) b2[i] * b3[i];
  return r;
}

__attribute__((noinline, noclone)) long long
f13 (void)
{
  int i;
  long long r = 0;
  for (i = 0; i < N; ++i)
    r += (long long) c2[i] * (long long) c3[i];
  return r;
}

__attribute__((noinline, noclone)) unsigned long long
f14 (void)
{
  int i;
  unsigned long long r = 0;
  for (i = 0; i < N; ++i)
    r += (unsigned long long) d2[i] * (unsigned long long) d3[i];
  return r;
}

static void
TEST (void)
{
  int i;
  int s1 = 0;
  unsigned int s2 = 0;
  long long s3 = 0;
  unsigned long long s4 = 0;
  for (i = 0; i < N; ++i)
    {
      asm volatile ("" : : "r" (&s1) : "memory");
      asm volatile ("" : : "r" (&s2) : "memory");
      asm volatile ("" : : "r" (&s3) : "memory");
      asm volatile ("" : : "r" (&s4) : "memory");
      b2[i] = (int) random ();
      b3[i] = (int) random ();
      a2[i] = b2[i];
      a3[i] = b3[i];
      d2[i] = (((int) random ()) << 16) | b2[i];
      d3[i] = (((int) random ()) << 16) | b3[i];
      c2[i] = d2[i];
      c3[i] = d3[i];
      s1 += a2[i] * a3[i];
      s2 += (unsigned int) b2[i] * b3[i];
      s3 += (long long) c2[i] * (long long) c3[i];
      s4 += (unsigned long long) d2[i] * (unsigned long long) d3[i];
    }
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  f5 ();
  f6 ();
  for (i = 0; i < N; ++i)
    {
      if (a1[i] != (short) (a2[i] * a3[i]))
	abort ();
      if (b1[i] != (unsigned short) (b2[i] * b3[i]))
	abort ();
      if (c1[i] != c2[i] * c3[i])
	abort ();
      if (d1[i] != d2[i] * d3[i])
	abort ();
      if (e1[i] != e2[i] * e3[i])
	abort ();
      if (g1[i] != g2[i] * g3[i])
	abort ();
    }
  f7 ();
  f8 ();
  f9 ();
  f10 ();
  for (i = 0; i < N; ++i)
    {
      if (c1[i] != a2[i] * a3[i])
	abort ();
      if (d1[i] != b2[i] * b3[i])
	abort ();
      if (e1[i] != (long long) c2[i] * (long long) c3[i])
	abort ();
      if (g1[i] != (unsigned long long) d2[i] * (unsigned long long) d3[i])
	abort ();
    }
  if (f11 () != s1 || f12 () != s2 || f13 () != s3 || f14 () != s4)
    abort ();
}
