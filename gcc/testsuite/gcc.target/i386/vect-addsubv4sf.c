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

float x[4], y[4], z[4];
void __attribute__((noipa)) foo ()
{
  x[0] = y[0] - z[0];
  x[1] = y[1] + z[1];
  x[2] = y[2] - z[2];
  x[3] = y[3] + z[3];
}
void __attribute__((noipa)) bar ()
{
  x[0] = y[0] + z[0];
  x[1] = y[1] - z[1];
  x[2] = y[2] + z[2];
  x[3] = y[3] - z[3];
}
static void
TEST (void)
{
  for (int i = 0; i < 4; ++i)
    {
      y[i] = i + 1;
      z[i] = 2 * i + 1;
    }
  foo ();
  if (x[0] != 0 || x[1] != 5 || x[2] != -2 || x[3] != 11)
    __builtin_abort ();
  bar ();
  if (x[0] != 2 || x[1] != -1 || x[2] != 8 || x[3] != -3)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-times "ADDSUB" 1 "slp2" } } */
