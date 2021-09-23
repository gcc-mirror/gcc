/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -msse2" } */
/* { dg-require-effective-target sse2 } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

float
__attribute__((noipa, optimize("tree-vectorize")))
foo (float* p)
{
  float sum = 0.f;
  for (int i = 0; i != 4; i++)
    sum += p[i];
  return sum;
}

static void
TEST (void)
{
  float p[4] = {1.0f, 2.0f, 3.0f, 4.0f};
  float res = foo (p);
  if (res != 10.0f)
    abort();
}
