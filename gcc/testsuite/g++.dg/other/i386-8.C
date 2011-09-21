// PR rtl-optimization/45400
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-O2 -msse2" }
// { dg-options "-O2 -msse2 -fpic" { target fpic } }

#include <xmmintrin.h>

static inline unsigned short
bar (unsigned short x)
{
  return ((x << 8) | (x >> 8));
}

unsigned int
foo (float *x, short *y)
{
  __m128 a = _mm_set_ps1 (32767.5f);
  __m128 b = _mm_mul_ps (_mm_load_ps (x), a);
  __m64 c = _mm_cvtps_pi16 (b);
  __builtin_memcpy (y, &c, sizeof (short) * 4);
  y[0] = bar (y[0]);
}
