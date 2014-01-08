/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#define SIZE (512 / 64)
#include "avx512f-mask-type.h"
#include <string.h>
#include "avx512f-check.h"
#include "avx512f-mask-type.h"
#include "avx512f-helper.h"

void static
avx512f_test (void)
{
  union512d s1, res, res2, res3;
  union256d s2;
  double res_ref[8];
  MASK_TYPE mask = MASK_VALUE;
  int j;

  for (j = 0; j < 8; j++)
    {
      s1.a[j] = j * j + 1.6;
      res2.a[j] = DEFAULT_VALUE;
    }

  for (j = 0; j < 4; j++)
    s2.a[j] = j * j * j / 2.7;

  res.x = _mm512_insertf64x4 (s1.x, s2.x, 0);
  res2.x = _mm512_mask_insertf64x4 (res2.x, mask, s1.x, s2.x, 0);
  res3.x = _mm512_maskz_insertf64x4 (mask, s1.x, s2.x, 0);

  memcpy (res_ref, s1.a, 64);
  memcpy (res_ref, s2.a, 32);

  if (check_union512d (res, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (check_union512d (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (check_union512d (res3, res_ref))
    abort ();

  res.x = _mm512_insertf64x4 (s1.x, s2.x, 1);
  res2.x = _mm512_mask_insertf64x4 (res2.x, mask, s1.x, s2.x, 1);
  res3.x = _mm512_maskz_insertf64x4 (mask, s1.x, s2.x, 1);

  memcpy (res_ref, s1.a, 64);
  memcpy (res_ref + 4, s2.a, 32);

  if (check_union512d (res, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (check_union512d (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (check_union512d (res3, res_ref))
    abort ();
}
