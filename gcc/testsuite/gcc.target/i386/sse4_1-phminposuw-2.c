/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O3 -msse4.1 -mno-avx2" } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

extern void abort (void);

#define N 1024
short a[N], c, e;
unsigned short b[N], d, f;

__attribute__((noinline)) short
vecsmax (void)
{
  int i;
  short r = -32768;
  for (i = 0; i < N; ++i)
    if (r < a[i]) r = a[i];
  return r;
}

__attribute__((noinline)) unsigned short
vecumax (void)
{
  int i;
  unsigned short r = 0;
  for (i = 0; i < N; ++i)
    if (r < b[i]) r = b[i];
  return r;
}

__attribute__((noinline)) short
vecsmin (void)
{
  int i;
  short r = 32767;
  for (i = 0; i < N; ++i)
    if (r > a[i]) r = a[i];
  return r;
}

__attribute__((noinline)) unsigned short
vecumin (void)
{
  int i;
  unsigned short r = 65535;
  for (i = 0; i < N; ++i)
    if (r > b[i]) r = b[i];
  return r;
}

static void
TEST (void)
{
  int i;
  for (i = 0; i < N; ++i)
    {
      a[i] = i - N / 2;
      b[i] = i + 32768 - N / 2;
    }
  a[N / 3] = N;
  a[2 * N / 3] = -N;
  b[N / 5] = 32768 + N;
  b[4 * N / 5] = 32768 - N;
  if (vecsmax () != N || vecsmin () != -N)
    abort ();
  if (vecumax () != 32768 + N || vecumin () != 32768 - N)
    abort ();
}
