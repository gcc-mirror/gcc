/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include <emmintrin.h>

static int
__attribute__((noinline, unused))
test (__m128d p)
{
  return _mm_movemask_pd (p); 
}

static void
TEST (void)
{
  double source[2] = {1.234, -2234.23};
  union128d s1;
  int d;
  int e;

  s1.x = _mm_loadu_pd (source);

  d = test (s1.x);

  e = 0;
  if (source[0] < 0)
    e |= 1;
  
  if (source[1] < 0)
    e |= 1 << 1;

  if (checkVi (&d, &e, 1))
    abort ();  
}
