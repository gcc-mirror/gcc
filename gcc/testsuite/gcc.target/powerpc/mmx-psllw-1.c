/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector" } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1
#ifndef CHECK_H
#define CHECK_H "mmx-check.h"
#endif

#ifndef TEST
#define TEST mmx_test
#endif

#define N 0xb

#include CHECK_H

#include <mmintrin.h>

static __m64
__attribute__((noinline, unused))
test (__m64 s1)
{
  return _mm_sll_pi16 (s1, N);
}

static void
TEST (void)
{
  __m64_union u, s1;
  __m64_union e;
  int i;

  s1.as_m64 = _mm_setr_pi16 (1, 2, 0x7000, 0x9000);
  u.as_m64 = test (s1.as_m64);


  if (N < 16)
    for (i = 0; i < 4; i++)
      e.as_short[i] = s1.as_short[i] << N;

  if (u.as_m64 != e.as_m64)
    abort ();
}
