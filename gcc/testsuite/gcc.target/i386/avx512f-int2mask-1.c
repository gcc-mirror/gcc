/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static void
avx512f_test (void)
{
  __m512i a = _mm512_set_epi32 (1, 17, 2, 12, 4, 14, 6, 16,
				8, 11, 10, 20, 12, 22, 14, 24);
  __m512i b = _mm512_set_epi32 (0, 1, 11, 3, 13, 5, 15, 7,
				17, 9, 19, 11, 21, 13, 23, 16);
  __mmask16 c = _mm512_kmov (_mm512_int2mask (2 | 8));
  __m512i d = _mm512_mask_mov_epi32 (a, c, b);
  __m512i e = _mm512_set_epi32 (1, 17, 2, 12, 4, 14, 6, 16,
				8, 11, 10, 20, 21, 22, 23, 24);
  if (_mm512_mask2int (_mm512_cmpeq_epi32_mask (d, e)) != 0xffff)
    __builtin_abort ();
}
