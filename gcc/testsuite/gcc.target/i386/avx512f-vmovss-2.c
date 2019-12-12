/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#include "avx512f-helper.h"

#define SIZE (128 / 32)
#include "avx512f-mask-type.h"

void
avx512f_test (void)
{
  int i, sign;
  union128 res1, res2, res3, res4, src1, src2, src3;
  volatile __mmask8 mask = 5;
  float val[2] = { 35.5f, 0.0f };
  float *volatile p = &val[0];
  float res_ref[SIZE];
  float zero[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 1.5f + i;
      src2.a[i] = 7.5f + i;
      src3.a[i] = 4.5f + i;
      zero[i] = 0.0f;
    }

  res1.x = _mm_mask_load_ss (src1.x, mask, p);
  res2.x = _mm_maskz_load_ss (mask, p);

  __builtin_memcpy (res_ref, zero, sizeof (zero));
  res_ref[0] = val[0];
  if (check_union128 (res1, res_ref))
    abort ();

  if (check_union128 (res2, res_ref))
    abort ();

  res3.x = _mm_mask_move_ss (src1.x, mask, src2.x, src3.x);
  res4.x = _mm_maskz_move_ss (mask, src2.x, src3.x);

  __builtin_memcpy (res_ref, src2.a, sizeof (src2.a));
  res_ref[0] = src3.a[0];
  if (check_union128 (res3, res_ref))
    abort ();

  if (check_union128 (res4, res_ref))
    abort ();

  _mm_mask_store_ss (p + 1, mask, src1.x);
  if (val[1] != src1.a[0])
    abort ();

  mask ^= 1;

  res1.x = _mm_mask_load_ss (src1.x, mask, p);
  res2.x = _mm_maskz_load_ss (mask, p);

  __builtin_memcpy (res_ref, zero, sizeof (zero));
  res_ref[0] = src1.a[0];
  if (check_union128 (res1, res_ref))
    abort ();

  res_ref[0] = zero[0];
  if (check_union128 (res2, res_ref))
    abort ();

  res3.x = _mm_mask_move_ss (src1.x, mask, src2.x, src3.x);
  res4.x = _mm_maskz_move_ss (mask, src2.x, src3.x);

  __builtin_memcpy (res_ref, src2.a, sizeof (src2.a));
  res_ref[0] = src1.a[0];
  if (check_union128 (res3, res_ref))
    abort ();

  res_ref[0] = zero[0];
  if (check_union128 (res4, res_ref))
    abort ();

  val[1] = 42.0f;
  _mm_mask_store_ss (p + 1, mask, src1.x);
  if (val[1] != 42.0f)
    abort ();
}
