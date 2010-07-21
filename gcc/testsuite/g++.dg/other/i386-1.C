/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-msse2" } */
/* { dg-require-effective-target sse2 } */
/* { dg-require-effective-target sse2_runtime } */

#include <xmmintrin.h>

static void
sse2_test (void)
{
  float a = 1.0f;
  float b = 2.0f;
  float c = 3.0f;
  float r;

  __m128 v = _mm_set_ps(a, b, c, 0);
  
  v = (__m128)_mm_srli_si128((__m128i)v, 4);
  _mm_store_ss(&r, v);
  if (r != 3.0f)
    abort ();
}

int
main ()
{
  sse2_test ();
  return 0;
}
