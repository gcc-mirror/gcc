/* { dg-do run } */
/* { dg-options "-O3 -msse2 -mno-avx" } */
/* { dg-require-effective-target sse2 } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#define N 16
float f[N];
double d[N];
int n[N];

__attribute__((noinline)) void
f1 (void)
{
  int i;
  for (i = 0; i < N; i++)
    n[i] = d[i];
}

__attribute__((noinline)) void
f2 (void)
{
  int i;
  for (i = 0; i < N; i++)
    f[i] = n[i];
}

__attribute__((noinline)) void
f3 (void)
{
  int i;
  for (i = 0; i < N; i++)
    d[i] = f[i];
}

__attribute__((noinline)) void
f4 (void)
{
  int i;
  for (i = 0; i < N; i++)
    n[i] = f[i];
}

__attribute__((noinline)) void
f5 (void)
{
  int i;
  for (i = 0; i < N; i++)
    d[i] = n[i];
}

__attribute__((noinline)) void
f6 (void)
{
  int i;
  for (i = 0; i < N; i++)
    f[i] = d[i];
}

static void
TEST ()
{
  int i;
  for (i = 0; i < N; i++)
    {
      asm ("");
      d[i] = i + 2.5;
    }
  f1 ();
  for (i = 0; i < N; i++)
    if (n[i] != i + 2)
      abort ();
    else
      n[i] = i + 7;
  f2 ();
  for (i = 0; i < N; i++)
    if (f[i] != i + 7)
      abort ();
    else
      f[i] = i - 2.25f;
  f3 ();
  for (i = 0; i < N; i++)
    if (d[i] != i - 2.25)
      abort ();
    else
      f[i] = i + 3.5;
  f4 ();
  for (i = 0; i < N; i++)
    if (n[i] != i + 3)
      abort ();
    else
      n[i] = i + 9;
  f5 ();
  for (i = 0; i < N; i++)
    if (d[i] != i + 9)
      abort ();
    else
      d[i] = i - 7.25;
  f6 ();
  for (i = 0; i < N; i++)
    if (f[i] != i - 7.25)
      abort ();
}
