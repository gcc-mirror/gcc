/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_movlpd_1
#endif

#include <emmintrin.h>

static __m128d
__attribute__((noinline, unused))
test (__m128d a, double *e)
{
  __asm("" : "+v"(a), "+b"(e));
  return _mm_loadl_pd (a, e); 
}

static void
TEST (void)
{
  union128d u, s1;
  double d[2] = {2134.3343,1234.635654};
  double e[2];

  s1.x = _mm_set_pd (41124.234,2344.2354);
  u.x = _mm_loadu_pd (d);

  u.x = test (s1.x, d);  

  e[0] = d[0];
  e[1] = s1.a[1];

  if (check_union128d (u, e))
    abort ();
}
