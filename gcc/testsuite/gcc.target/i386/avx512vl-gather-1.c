/* { dg-do run } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-O3 -mavx512vl" } */

#include "avx512vl-check.h"

#define N 12
float vf1[N+4], vf2[N];
double vd1[N+4], vd2[N];
int vi1[N+4], vi2[N], k[N];
long long vl1[N+4], vl2[N];
long l[N];

__attribute__((noinline, noclone)) void
f1 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vf2[i] = vf1[k[i]];
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vi2[i] = vi1[k[i]];
}

__attribute__((noinline, noclone)) void
f3 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    vf2[i] = vf1[k[i] + x];
}

__attribute__((noinline, noclone)) void
f4 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    vi2[i] = vi1[k[i] + x];
}

__attribute__((noinline, noclone)) void
f5 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vd2[i] = vd1[k[i]];
}

__attribute__((noinline, noclone)) void
f6 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vl2[i] = vl1[k[i]];
}

__attribute__((noinline, noclone)) void
f7 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    vd2[i] = vd1[k[i] + x];
}

__attribute__((noinline, noclone)) void
f8 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    vl2[i] = vl1[k[i] + x];
}

__attribute__((noinline, noclone)) void
f9 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vf2[i] = vf1[l[i]];
}

__attribute__((noinline, noclone)) void
f10 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vi2[i] = vi1[l[i]];
}

__attribute__((noinline, noclone)) void
f11 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    vf2[i] = vf1[l[i] + x];
}

__attribute__((noinline, noclone)) void
f12 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    vi2[i] = vi1[l[i] + x];
}

__attribute__((noinline, noclone)) void
f13 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vd2[i] = vd1[l[i]];
}

__attribute__((noinline, noclone)) void
f14 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vl2[i] = vl1[l[i]];
}

__attribute__((noinline, noclone)) void
f15 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    vd2[i] = vd1[l[i] + x];
}

__attribute__((noinline, noclone)) void
f16 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    vl2[i] = vl1[l[i] + x];
}

static void
avx512vl_test (void)
{
  int i;

  for (i = 0; i < N + 4; i++)
    {
      asm ("");
      vf1[i] = 17.0f + i;
      vd1[i] = 19.0 + i;
      vi1[i] = 21 + i;
      vl1[i] = 23L + i;
    }
  for (i = 0; i < N; i++)
    {
      asm ("");
      k[i] = (i * 731) & (N - 1);
      l[i] = (i * 657) & (N - 1);
    }

  f1 ();
  f2 ();
  for (i = 0; i < N; i++)
    if (vf2[i] != ((i * 731) & (N - 1)) + 17
	|| vi2[i] != ((i * 731) & (N - 1)) + 21)
      abort ();

  f3 (1);
  f4 (2);
  for (i = 0; i < N; i++)
    if (vf2[i] != ((i * 731) & (N - 1)) + 17 + 1
	|| vi2[i] != ((i * 731) & (N - 1)) + 21 + 2)
      abort ();

  f5 ();
  f6 ();
  for (i = 0; i < N; i++)
    if (vd2[i] != ((i * 731) & (N - 1)) + 19
	|| vl2[i] != ((i * 731) & (N - 1)) + 23)
      abort ();

  f7 (3);
  f8 (2);
  for (i = 0; i < N; i++)
    if (vd2[i] != ((i * 731) & (N - 1)) + 19 + 3
	|| vl2[i] != ((i * 731) & (N - 1)) + 23 + 2)
      abort ();

  f9 ();
  f10 ();
  for (i = 0; i < N; i++)
    if (vf2[i] != ((i * 657) & (N - 1)) + 17
	|| vi2[i] != ((i * 657) & (N - 1)) + 21)
      abort ();

  f11 (4);
  f12 (1);
  for (i = 0; i < N; i++)
    if (vf2[i] != ((i * 657) & (N - 1)) + 17 + 4
	|| vi2[i] != ((i * 657) & (N - 1)) + 21 + 1)
      abort ();

  f13 ();
  f14 ();
  for (i = 0; i < N; i++)
    if (vd2[i] != ((i * 657) & (N - 1)) + 19
	|| vl2[i] != ((i * 657) & (N - 1)) + 23)
      abort ();

  f15 (2);
  f16 (4);
  for (i = 0; i < N; i++)
    if (vd2[i] != ((i * 657) & (N - 1)) + 19 + 2
	|| vl2[i] != ((i * 657) & (N - 1)) + 23 + 4)
      abort ();
}
