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

static __m128
__attribute__((noinline, unused))
test (__m128d p)
{
  return _mm_cvtpd_ps (p); 
}

static void
TEST (void)
{
  union128 u;
  union128d s;
  float e[4] = {0};

  s.x = _mm_set_pd (123.321, 456.987);

  u.x = test (s.x);

  e[0] = (float)s.a[0]; 
  e[1] = (float)s.a[1]; 

  if (check_union128 (u, e))
    abort ();
}
