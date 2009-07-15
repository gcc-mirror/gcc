/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse2" } */

#include "sse2-check.h"

extern long lrintf (float);

#define N 32

float a[N] = {0.4,3.5,6.6,9.4,12.5,15.6,18.4,21.5,24.6,27.4,30.5,33.6,36.4,39.5,42.6,45.4,0.5,3.6,6.4,9.5,12.6,15.4,18.5,21.6,24.4,27.5,30.6,33.4,36.5,39.6,42.4,45.5};
long r[N];

static void
sse2_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    {
      r[i] = lrintf (a[i]);
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (r[i] != lrintf (a[i]))
	abort();
    }   
}
