/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -msse -mfpmath=sse -mrecip" } */
/* { dg-require-effective-target sse } */

#include "sse-check.h"

extern float sqrtf (float);

#define N 8

static void
sse_test (void)
{
  float a[N] = { 0.f, 18.f, 108.f, 324.f, 720.f, 1944.f, 3087.f, 5832.f };
  float b[N] = { 1.f, 2.f, 3.f, 4.f, 5.f, 6.f, 7.f, 8.f };
  float r[N];

  float rc[N] = { 0.f, 3.f, 6.f, 9.f, 12.f, 18.f, 21.f, 27.f };

  int i;

  for (i = 0; i < N; i++)
    {
      r[i] = sqrtf (a[i] / b[i]);
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (r[i] != rc[i])
	abort();
    }   
}
