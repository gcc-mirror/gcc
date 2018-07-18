/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_movlpd_2
#endif

#include <emmintrin.h>

static void
__attribute__((noinline, unused))
test (double *e, __m128d a)
{
  __asm("" : "+v"(a), "+b"(e));
  return _mm_storel_pd (e, a); 
}

static void
TEST (void)
{
  union128d u;
  double e[2];

  u.x = _mm_set_pd (41124.234,2344.2354);

  test (e, u.x);  

  e[1] = u.a[1];
  
  if (check_union128d (u, e))
    abort ();
}
