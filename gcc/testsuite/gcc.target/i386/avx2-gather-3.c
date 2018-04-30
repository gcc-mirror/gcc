/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O3 -mavx2 -ffast-math -mtune=skylake" } */

#include "avx2-check.h"

#define N 1024
float f[N];
double d[N];
int k[N];
float *l[N];
double *n[N];
int **m[N];
long **o[N];
long q[N];
long *r[N];
int *s[N];

__attribute__((noinline, noclone)) float
f1 (void)
{
  int i;
  float g = 0.0;
  for (i = 0; i < N / 2; i++)
    g += f[k[i]];
  return g;
}

__attribute__((noinline, noclone)) float
f2 (float *p)
{
  int i;
  float g = 0.0;
  for (i = 0; i < N / 2; i++)
    g += p[k[i]];
  return g;
}

__attribute__((noinline, noclone)) float
f3 (void)
{
  int i;
  float g = 0.0;
  for (i = 0; i < N / 2; i++)
    g += *l[i];
  return g;
}

__attribute__((noinline, noclone)) int
f4 (void)
{
  int i;
  int g = 0;
  for (i = 0; i < N / 2; i++)
    g += **m[i];
  return g;
}

__attribute__((noinline, noclone)) double
f5 (void)
{
  int i;
  double g = 0.0;
  for (i = 0; i < N / 2; i++)
    g += d[k[i]];
  return g;
}

__attribute__((noinline, noclone)) double
f6 (double *p)
{
  int i;
  double g = 0.0;
  for (i = 0; i < N / 2; i++)
    g += p[k[i]];
  return g;
}

__attribute__((noinline, noclone)) double
f7 (void)
{
  int i;
  double g = 0.0;
  for (i = 0; i < N / 2; i++)
    g += *n[i];
  return g;
}

__attribute__((noinline, noclone)) int
f8 (void)
{
  int i;
  int g = 0;
  for (i = 0; i < N / 2; i++)
    g += **o[i];
  return g;
}

__attribute__((noinline, noclone)) float
f9 (void)
{
  int i;
  float g = 0.0;
  for (i = 0; i < N / 2; i++)
    g += f[q[i]];
  return g;
}

__attribute__((noinline, noclone)) float
f10 (float *p)
{
  int i;
  float g = 0.0;
  for (i = 0; i < N / 2; i++)
    g += p[q[i]];
  return g;
}

__attribute__((noinline, noclone)) double
f11 (void)
{
  int i;
  double g = 0.0;
  for (i = 0; i < N / 2; i++)
    g += d[q[i]];
  return g;
}

__attribute__((noinline, noclone)) double
f12 (double *p)
{
  int i;
  double g = 0.0;
  for (i = 0; i < N / 2; i++)
    g += p[q[i]];
  return g;
}

static void
avx2_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    {
      asm ("");
      f[i] = -256.0f + i;
      d[i] = -258.0 + i;
      k[i] = (i * 731) & (N - 1);
      q[i] = (i * 657) & (N - 1);
      l[i] = &f[(i * 239) & (N - 1)];
      n[i] = &d[(i * 271) & (N - 1)];
      r[i] = &q[(i * 323) & (N - 1)];
      s[i] = &k[(i * 565) & (N - 1)];
      m[i] = &s[(i * 13) & (N - 1)];
      o[i] = &r[(i * 19) & (N - 1)];
    }

  if (f1 () != 136448.0f || f2 (f) != 136448.0f || f3 () != 130304.0)
    abort ();
  if (f4 () != 261376 || f5 () != 135424.0 || f6 (d) != 135424.0)
    abort ();
  if (f7 () != 129280.0 || f8 () != 259840L || f9 () != 130816.0f)
    abort ();
  if (f10 (f) != 130816.0f || f11 () != 129792.0 || f12 (d) != 129792.0)
    abort ();
}
