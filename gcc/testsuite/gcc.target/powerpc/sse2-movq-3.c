/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_movq_3
#endif

#include <emmintrin.h>

static long long
__attribute__((noinline, unused))
test (__m128i b)
{
  __asm("" : "+v"(b));
  return _mm_cvtsi128_si64 (b); 
}

static void
TEST (void)
{
  union128i_q u;
  long long e;

  u.x = _mm_set_epi64x (4294967295133LL, 3844294967295133LL);
  e = test (u.x);
  if (e != u.a[0])
    abort ();
}
