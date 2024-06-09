/* { dg-do run } */
/* { dg-options "-O3 -mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
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
  return _mm_sll_pi32 (s1, N);
}

static void
TEST (void)
{
  __m64_union u, s1;
  __m64_union e;
  int i;

  s1.as_m64 = _mm_setr_pi32 (1, -2);
  u.as_m64 = test (s1.as_m64);


  if (N < 16)
    for (i = 0; i < 2; i++)
      e.as_int[i] = s1.as_int[i] << N;

  if (u.as_m64 != e.as_m64)
    abort ();
}
