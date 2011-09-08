/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpmovmskb\[ \\t\]+\[^\n\]*%ymm\[0-9\]" } } */

#include "avx2-check.h"

static void
avx2_test (void)
{
  union256i_b s;
  int res, res_ref;
  int i, e = 0;

  s.x = _mm256_set_epi8 (1, 2, 3, 4, 10, 20, 30, 90, -80, -40, -100,
			 15, 98, 25, 98, 7, 1, 2, 3, 4, 10, 20, 30, 90,
			 -80, -40, -100, -15, 98, 25, 98, 7);

  res = _mm256_movemask_epi8 (s.x);

  for (i = 0; i < 32; i++)
    if (s.a[i] & (1 << 7))
      res_ref = res_ref | (1 << i);

  if (res != res_ref)
    abort ();
}
