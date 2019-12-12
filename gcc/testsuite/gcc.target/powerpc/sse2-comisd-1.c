/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_comi_sd_1
#endif

#include <emmintrin.h>

static int 
__attribute__((noinline, unused))
test (__m128d s1, __m128d s2)
{
  __asm("" : "+v"(s1), "+v"(s2));
  return _mm_comieq_sd (s1, s2); 
}

static void
TEST (void)
{
  union128d s1, s2;
  int d[1];
  int e[1];
 
  s1.x = _mm_set_pd (2134.3343,2344.2354);
  s2.x = _mm_set_pd (41124.234,2344.2354);
  d[0] = test (s1.x, s2.x); 
  e[0] = s1.a[0] == s2.a[0];

  if (checkVi (d, e, 1))
    abort ();
}
