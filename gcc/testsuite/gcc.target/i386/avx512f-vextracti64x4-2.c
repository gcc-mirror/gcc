/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#include <string.h>
#include "avx512f-check.h"
#include "avx512f-helper.h"

void static
avx512f_test (void)
{
  union512i_q s1;
  union256i_q res1, res2, res3;
  __mmask8 mask = 0xBA;
  long long int res_ref[4];
  int j;

  for (j = 0; j < 8; j++)
    s1.a[j] = j * j;

  for (j = 0; j < 4; j++)
    {
      res1.a[j] = DEFAULT_VALUE;
      res2.a[j] = DEFAULT_VALUE;
      res3.a[j] = DEFAULT_VALUE;
    }
  res1.x = _mm512_extracti64x4_epi64 (s1.x, 0);
  res2.x = _mm512_mask_extracti64x4_epi64 (res2.x, mask, s1.x, 0);
  res3.x = _mm512_maskz_extracti64x4_epi64 (mask, s1.x, 0);

  memset (res_ref, 0, 32);
  memcpy (res_ref, s1.a, 32);

  if (check_union256i_q (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, 4);
  if (check_union256i_q (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, 4);
  if (check_union256i_q (res3, res_ref))
    abort ();

  res1.x = _mm512_extracti64x4_epi64 (s1.x, 1);
  res2.x = _mm512_mask_extracti64x4_epi64 (res2.x, mask, s1.x, 1);
  res3.x = _mm512_maskz_extracti64x4_epi64 (mask, s1.x, 1);

  memset (res_ref, 0, 32);
  memcpy (res_ref, s1.a + 4, 32);

  if (check_union256i_q (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, 4);
  if (check_union256i_q (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, 4);
  if (check_union256i_q (res3, res_ref))
    abort ();
}
