/* { dg-do run } */
/* { dg-options "-O2 -mfpmath=sse -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include <emmintrin.h>

static void
__attribute__((noinline, unused))
test (double *p, __m128d a)
{
  return _mm_storeh_pd (p, a); 
}

static void
TEST (void)
{
  union128d s;
  double d[1];
  double e[1];
   
  s.x = _mm_set_pd (2134.3343,1234.635654);
  test (d, s.x);

  e[0] = s.a[1];

  if (e[0] != d[0])
    abort ();
}
