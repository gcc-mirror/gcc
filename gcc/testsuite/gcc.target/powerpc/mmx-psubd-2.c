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
  return _mm_sub_pi32 (s1, s2);
}

static __m64
__attribute__((noinline, unused))
test_alias (__m64 s1, __m64 s2)
{
  return _m_psubd (s1, s2);
}

static void
TEST (void)
{
  __m64_union u, s1, s2;
  __m64_union e, v;
  int i;
   
  s1.as_m64 = _mm_setr_pi32 (30, 90);
  s2.as_m64 = _mm_setr_pi32 (76, -100);
  u.as_m64 = test (s1.as_m64, s2.as_m64);
  v.as_m64 = test_alias (s1.as_m64, s2.as_m64);

  for (i = 0; i < 2; i++)
     e.as_int[i] = s1.as_int[i] - s2.as_int[i];

  if (u.as_m64 != e.as_m64 || u.as_m64 != v.as_m64)
    abort ();
}
