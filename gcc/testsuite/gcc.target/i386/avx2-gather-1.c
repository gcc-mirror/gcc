/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O3 -mavx2 -mtune=skylake" } */

#include "avx2-check.h"

#define N 1024
float vf1[N+16], vf2[N];
double vd1[N+16], vd2[N];
int k[N];
long l[N];
short n[N];

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
    n[i] = (int) vf1[k[i]];
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
    n[i] = (int) vf1[k[i] + x];
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
    n[i] = (int) vd1[k[i]];
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
    n[i] = (int) vd1[k[i] + x];
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
    n[i] = (int) vf1[l[i]];
}

__attribute__((noinline, noclone)) void
f11 (long x)
{
  int i;
  for (i = 0; i < N; i++)
    vf2[i] = vf1[l[i] + x];
}

__attribute__((noinline, noclone)) void
f12 (long x)
{
  int i;
  for (i = 0; i < N; i++)
    n[i] = (int) vf1[l[i] + x];
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
    n[i] = (int) vd1[l[i]];
}

__attribute__((noinline, noclone)) void
f15 (long x)
{
  int i;
  for (i = 0; i < N; i++)
    vd2[i] = vd1[l[i] + x];
}

__attribute__((noinline, noclone)) void
f16 (long x)
{
  int i;
  for (i = 0; i < N; i++)
    n[i] = (int) vd1[l[i] + x];
}

static void
avx2_test (void)
{
  int i;

  for (i = 0; i < N + 16; i++)
    {
      asm ("");
      vf1[i] = 17.0f + i;
      vd1[i] = 19.0 + i;
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
	|| n[i] != ((i * 731) & (N - 1)) + 17)
      abort ();

  f3 (12);
  f4 (14);
  for (i = 0; i < N; i++)
    if (vf2[i] != ((i * 731) & (N - 1)) + 17 + 12
	|| n[i] != ((i * 731) & (N - 1)) + 17 + 14)
      abort ();

  f5 ();
  f6 ();
  for (i = 0; i < N; i++)
    if (vd2[i] != ((i * 731) & (N - 1)) + 19
	|| n[i] != ((i * 731) & (N - 1)) + 19)
      abort ();

  f7 (7);
  f8 (9);
  for (i = 0; i < N; i++)
    if (vd2[i] != ((i * 731) & (N - 1)) + 19 + 7
	|| n[i] != ((i * 731) & (N - 1)) + 19 + 9)
      abort ();

  f9 ();
  f10 ();
  for (i = 0; i < N; i++)
    if (vf2[i] != ((i * 657) & (N - 1)) + 17
	|| n[i] != ((i * 657) & (N - 1)) + 17)
      abort ();

  f11 (2);
  f12 (4);
  for (i = 0; i < N; i++)
    if (vf2[i] != ((i * 657) & (N - 1)) + 17 + 2
	|| n[i] != ((i * 657) & (N - 1)) + 17 + 4)
      abort ();

  f13 ();
  f14 ();
  for (i = 0; i < N; i++)
    if (vd2[i] != ((i * 657) & (N - 1)) + 19
	|| n[i] != ((i * 657) & (N - 1)) + 19)
      abort ();

  f15 (13);
  f16 (15);
  for (i = 0; i < N; i++)
    if (vd2[i] != ((i * 657) & (N - 1)) + 19 + 13
	|| n[i] != ((i * 657) & (N - 1)) + 19 + 15)
      abort ();
}
