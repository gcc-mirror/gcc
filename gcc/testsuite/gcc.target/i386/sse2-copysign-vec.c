/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */
/* { dg-require-effective-target sse2 } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

extern double copysign (double, double);

#define N 16

double a[N] = {-0.1,-3.2,-6.3,-9.4,-12.5,-15.6,-18.7,-21.8,24.9,27.1,30.2,33.3,36.4,39.5,42.6,45.7};
double b[N] = {-1.2,3.4,-5.6,7.8,-9.0,1.0,-2.0,3.0,-4.0,-5.0,6.0,7.0,-8.0,-9.0,10.0,11.0};
double r[N];

static void
TEST (void)
{  
  int i;

  for (i = 0; i < N; i++)
    r[i] = copysign (a[i], b[i]);

  /* check results:  */
  for (i = 0; i < N; i++)
    if (r[i] != copysign (a[i], b[i]))
      abort ();
}

