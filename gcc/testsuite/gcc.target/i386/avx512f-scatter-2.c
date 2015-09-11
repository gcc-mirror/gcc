/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O3 -mavx512f" } */

#define AVX512F

#include "avx512f-check.h"

#define N 1024
float vf1[N], vf2[2*N+16];
double vd1[N], vd2[2*N+16];
int k[N];
long l[N];
short n[2*N+16];

__attribute__((noinline, noclone)) void
f1 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vf2[k[i]] = vf1[i];
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  int i;
  for (i = 0; i < N; i++)
    n[k[i]] = (int) vf1[i];
}

__attribute__((noinline, noclone)) void
f3 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    vf2[k[i] + x] = vf1[i];
}

__attribute__((noinline, noclone)) void
f4 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    n[k[i] + x] = (int) vf1[i];
}

__attribute__((noinline, noclone)) void
f5 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vd2[k[i]] = vd1[i];
}

__attribute__((noinline, noclone)) void
f6 (void)
{
  int i;
  for (i = 0; i < N; i++)
    n[k[i]] = (int) vd1[i];
}

__attribute__((noinline, noclone)) void
f7 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    vd2[k[i] + x] = vd1[i];
}

__attribute__((noinline, noclone)) void
f8 (int x)
{
  int i;
  for (i = 0; i < N; i++)
    n[k[i] + x] = vd1[i];
}

__attribute__((noinline, noclone)) void
f9 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vf2[l[i]] = vf1[i];
}

__attribute__((noinline, noclone)) void
f10 (void)
{
  int i;
  for (i = 0; i < N; i++)
    n[l[i]] = (int) vf1[i];
}

__attribute__((noinline, noclone)) void
f11 (long x)
{
  int i;
  for (i = 0; i < N; i++)
    vf2[l[i] + x] = vf1[i];
}

__attribute__((noinline, noclone)) void
f12 (long x)
{
  int i;
  for (i = 0; i < N; i++)
    n[l[i] + x] = (int) vf1[i];
}

__attribute__((noinline, noclone)) void
f13 (void)
{
  int i;
  for (i = 0; i < N; i++)
    vd2[l[i]] = vd1[i];
}

__attribute__((noinline, noclone)) void
f14 (void)
{
  int i;
  for (i = 0; i < N; i++)
    n[l[i]] = (int) vd1[i];
}

__attribute__((noinline, noclone)) void
f15 (long x)
{
  int i;
  for (i = 0; i < N; i++)
    vd2[l[i] + x] = vd1[i];
}

__attribute__((noinline, noclone)) void
f16 (long x)
{
  int i;
  for (i = 0; i < N; i++)
    n[l[i] + x] = (int) vd1[i];
}

static void
avx512f_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    {
      asm ("");
      vf1[i] = 17.0f + i;
      vd1[i] = 19.0 + i;
    }
  for (i = 0; i < N; i++)
    {
      asm ("");
      k[i] = (i % 2) ? (N / 2 + i) : (N / 2 - i / 2);
      l[i] = 2 * i + i % 2;
    }

  f1 ();
  f2 ();
  for (i = 0; i < N; i++)
    if (vf2[(i % 2) ? (N / 2 + i) : (N / 2 - i / 2)] != i + 17
	|| n[(i % 2) ? (N / 2 + i) : (N / 2 - i / 2)] != i + 17)
      abort ();

  f3 (12);
  f4 (14);
  for (i = 0; i < N; i++)
    if (vf2[((i % 2) ? (N / 2 + i) : (N / 2 - i / 2)) + 12] != i + 17
	|| n[((i % 2) ? (N / 2 + i) : (N / 2 - i / 2)) + 14] != i + 17)
      abort ();

  f5 ();
  f6 ();
  for (i = 0; i < N; i++)
    if (vd2[(i % 2) ? (N / 2 + i) : (N / 2 - i / 2)] != i + 19
	|| n[(i % 2) ? (N / 2 + i) : (N / 2 - i / 2)] != i + 19)
      abort ();

  f7 (7);
  f8 (9);
  for (i = 0; i < N; i++)
    if (vd2[((i % 2) ? (N / 2 + i) : (N / 2 - i / 2)) + 7] != i + 19
	|| n[((i % 2) ? (N / 2 + i) : (N / 2 - i / 2)) + 9] != i + 19)
      abort ();

  f9 ();
  f10 ();
  for (i = 0; i < N; i++)
    if (vf2[2 * i + i % 2] != i + 17
	|| n[2 * i + i % 2] != i + 17)
      abort ();

  f11 (2);
  f12 (4);
  for (i = 0; i < N; i++)
    if (vf2[2 * i + i % 2 + 2] != i + 17
	|| n[2 * i + i % 2 + 4] != i + 17)
      abort ();

  f13 ();
  f14 ();
  for (i = 0; i < N; i++)
    if (vd2[2 * i + i % 2] != i + 19
	|| n[2 * i + i % 2] != i + 19)
      abort ();

  f15 (13);
  f16 (15);
  for (i = 0; i < N; i++)
    if (vd2[2 * i + i % 2 + 13] != i + 19
	|| n[2 * i + i % 2 + 15] != i + 19)
      abort ();
}
