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

#include CHECK_H

#include <mmintrin.h>

static __m64
__attribute__((noinline, unused))
test (__m64 s1, __m64 s2)
{
  return _mm_packs_pi32 (s1, s2);
}

static short
saturate (int val)
{
  if (val > 32767)
    return 32767;
  else if (val < -32768)
    return -32768;
  else
    return val;
}

static inline int
l_mm_extract_pi32 (__m64 b, int imm8)
{
  unsigned int shift = imm8 & 0x1;
#ifdef __BIG_ENDIAN__
  shift = 1 - shift;
#endif
  return ((long long)b >> (shift * 32)) & 0xffffffff;
}

static void
TEST (void)
{
  __m64_union s1, s2;
  __m64_union u;
  __m64_union e;
  int start, end, inc;

  s1.as_m64 = _mm_set_pi32 (2134, -128);
  s2.as_m64 = _mm_set_pi32 (41124, 234);
  u.as_m64 = test (s1.as_m64, s2.as_m64);

#ifdef __LITTLE_ENDIAN__
  e.as_m64 = _mm_set_pi16 (saturate (l_mm_extract_pi32 (s2.as_m64, 1)),
                           saturate (l_mm_extract_pi32 (s2.as_m64, 0)),
                           saturate (l_mm_extract_pi32 (s1.as_m64, 1)),
                           saturate (l_mm_extract_pi32 (s1.as_m64, 0)));
#else
  e.as_m64 = _mm_set_pi16 (saturate (l_mm_extract_pi32 (s1.as_m64, 1)),
                           saturate (l_mm_extract_pi32 (s1.as_m64, 0)),
                           saturate (l_mm_extract_pi32 (s2.as_m64, 1)),
                           saturate (l_mm_extract_pi32 (s2.as_m64, 0)));
#endif

  if (u.as_m64 != e.as_m64)
    abort ();
}
