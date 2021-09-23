/* { dg-do run } */
/* { dg-require-effective-target sse3 } */
/* { dg-options "-O3 -msse3 -fdump-tree-slp2" } */

#ifndef CHECK_H
#define CHECK_H "sse3-check.h"
#endif

#ifndef TEST
#define TEST sse3_test
#endif

#include CHECK_H

double x[2], y[2], z[2];
void __attribute__((noipa)) foo ()
{
  x[0] = y[0] - z[0];
  x[1] = y[1] + z[1];
}
void __attribute__((noipa)) bar ()
{
  x[0] = y[0] + z[0];
  x[1] = y[1] - z[1];
}
static void
TEST (void)
{
  for (int i = 0; i < 2; ++i)
    {
      y[i] = i + 1;
      z[i] = 2 * i + 1;
    }
  foo ();
  if (x[0] != 0 || x[1] != 5)
    __builtin_abort ();
  bar ();
  if (x[0] != 2 || x[1] != -1)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-times "ADDSUB" 1 "slp2" } } */
