/* PR rtl-optimization/64286 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx2" } */
/* { dg-require-effective-target avx2 } */

#include <string.h>
#include <stdlib.h>
#include <x86intrin.h>
#include "avx2-check.h"

__m128i v;
__m256i w;

__attribute__((noinline, noclone)) void
foo (__m128i *p, __m128i *q)
{
  __m128i a = _mm_loadu_si128 (p);
  __m128i b = _mm_xor_si128 (a, v);
  w = _mm256_cvtepu8_epi16 (a);
  *q = b;
}

static void
avx2_test (void)
{
  v = _mm_set1_epi8 (0x40);
  __m128i c = _mm_set_epi8 (16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1);
  __m128i d;
  foo (&c, &d);
  __m128i e = _mm_set_epi8 (0x50, 0x4f, 0x4e, 0x4d, 0x4c, 0x4b, 0x4a, 0x49,
			    0x48, 0x47, 0x46, 0x45, 0x44, 0x43, 0x42, 0x41);
  __m256i f = _mm256_set_epi16 (16, 15, 14, 13, 12, 11, 10, 9,
				8, 7, 6, 5, 4, 3, 2, 1);
  if (memcmp (&w, &f, sizeof (w)) != 0
      || memcmp (&d, &e, sizeof (d)) != 0)
    abort ();
}
