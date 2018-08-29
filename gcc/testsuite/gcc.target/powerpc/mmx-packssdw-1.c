/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector" } */
/* { dg-require-effective-target lp64 } */
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
  return _mm_packs_pi32 (s1, s2);
}

static void
TEST (void)
{
  __m64_union s1, s2;
  __m64_union u;
  __m64_union e;
  int i;

  s1.as_m64 = _mm_set_pi32 (2134, -128);
  s2.as_m64 = _mm_set_pi32 (41124, 234);
  u.as_m64 = test (s1.as_m64, s2.as_m64);

  for (i = 0; i < 2; i++)
    {
      if (s1.as_int[i] > 32767)
        e.as_short[i] = 32767;
      else if (s1.as_int[i] < -32768)
        e.as_short[i] = -32768;
      else
        e.as_short[i] = s1.as_int[i];
    }
  
  for (i = 0; i < 2; i++)
   {
      if (s2.as_int[i] > 32767)
        e.as_short[i+2] = 32767;
      else if (s2.as_int[i] < -32768)
        e.as_short[i+2] = -32768;
      else
        e.as_short[i+2] = s2.as_int[i];
    }

  if (u.as_m64 != e.as_m64)
    abort ();
}
