/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

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
test (__m128d s1, __m128d s2)
{
  return _mm_ucomile_sd (s1, s2); 
}

static void
TEST (void)
{
  union128d s1, s2;
  int d[1] = {0};
  int e[1] = {0};
 
  s1.x = _mm_set_pd (2134.3343,12344.2354);
  s2.x = _mm_set_pd (41124.234,2344.2354);
  d[0] = test (s1.x, s2.x); 
  e[0] = s1.a[0] <= s2.a[0];

  if (checkVi (d, e, 1))
    abort ();
}
