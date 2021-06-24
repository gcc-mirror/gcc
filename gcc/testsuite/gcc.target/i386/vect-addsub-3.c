/* { dg-do run } */
/* { dg-require-effective-target sse3 } */
/* { dg-options "-O3 -msse3" } */

#ifndef CHECK_H
#define CHECK_H "sse3-check.h"
#endif

#ifndef TEST
#define TEST sse3_test
#endif

#include CHECK_H

double a[2], b[2], c[2];

void __attribute__((noipa))
foo ()
{
  /* When we want to use addsubpd we have to keep permuting both
     loads, if instead we blend the result of an add and a sub we
     can combine the blend with the permute.  Both are similar in cost,
     verify we did not wrongly apply both.  */
  double tem0 = a[1] - b[1];
  double tem1 = a[0] + b[0];
  c[0] = tem0;
  c[1] = tem1;
}

static void
TEST (void)
{
  a[0] = 1.; a[1] = 2.;
  b[0] = 2.; b[1] = 4.;
  foo ();
  if (c[0] != -2. || c[1] != 3.)
    __builtin_abort ();
}
