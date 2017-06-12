/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static void
avx512f_test (void)
{
  __m512i a
    = _mm512_set_epi32 (1, 17, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16);
  __m512i b
    = _mm512_set_epi32 (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16);
  __mmask16 c = _mm512_cmpgt_epu32_mask (a, b);
  if (_mm512_mask2int (c) != 0xc000)
    __builtin_abort ();
}
