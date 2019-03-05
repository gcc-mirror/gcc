/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1 -mfpmath=sse" } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

__attribute__((noipa)) double
f1 (double a, double b)
{
  return a < 0 ? a : b;
}

__attribute__((noipa)) float
f2 (float a, float b)
{
  return a < 0 ? a : b;
}

static void
TEST (void)
{
  if (f1 (5.0, 7.0) != 7.0
      || f1 (-2.0, 7.0) != -2.0
      || f2 (1.0f, 2.0f) != 2.0f
      || f2 (-1.0f, -3.0f) != -1.0f)
    abort ();
}
