/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse2" } */
/* { dg-require-effective-target sse2 } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

extern long lrint (double);

#define N 32

double a[N] = {0.4,3.5,6.6,9.4,12.5,15.6,18.4,21.5,24.6,27.4,30.5,33.6,36.4,39.5,42.6,45.4,0.5,3.6,6.4,9.5,12.6,15.4,18.5,21.6,24.4,27.5,30.6,33.4,36.5,39.6,42.4,45.5};
long r[N];

static void
TEST (void)
{
  int i;

  for (i = 0; i < N; i++)
    {
      r[i] = lrint (a[i]);
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (r[i] != lrint (a[i]))
	abort();
    }   
}
