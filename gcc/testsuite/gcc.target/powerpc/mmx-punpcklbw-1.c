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

#include CHECK_H

#include <mmintrin.h>

static __m64
__attribute__((noinline, unused))
test (__m64 s1, __m64 s2)
{
  return _mm_unpacklo_pi8 (s1, s2);
}

static void
TEST (void)
{
  __m64_union u, s1, s2, e;
  int i;
   
  s1.as_m64 = 0x0706050403020100UL;
  s2.as_m64 = 0x1716151413121110UL;
  u.as_m64 = test (s1.as_m64, s2.as_m64);
   
  for (i = 0; i < 4; i++)
    {
      e.as_char[2*i] = s1.as_char[i];
      e.as_char[2*i + 1] = s2.as_char[i];
    }

  if (u.as_m64 != e.as_m64)
    abort ();
}
