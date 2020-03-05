/* { dg-do run } */
/* { dg-options "-O2 -mavx512bitalg -mavx512bw" } */
/* { dg-require-effective-target avx512bitalg } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BITALG
#define SIZE (AVX512F_LEN / 16)

#include "avx512f-helper.h"
#include "avx512f-mask-type.h"

#define TYPE short

int
CALC (TYPE v)
{
  int ret;
  int i;

 ret = 0;
 for (i = 0; i < sizeof(v) * 8; i++)
   if ((v & ((TYPE)1 << (TYPE) i)))
     ret++;

 return ret;
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_w) res1, res2, res3, src, src0;
  MASK_TYPE mask = MASK_VALUE;
  TYPE res_ref[SIZE];
  src.x = INTRINSIC (_set1_epi8) (0x3D);
  int i;

  for (i = 0; i < SIZE; i++)
  {
    res_ref[i] = CALC (src.a[i]);
    src0.a[i] = DEFAULT_VALUE;
  }

  res1.x = INTRINSIC (_popcnt_epi16)       (src.x);
  res2.x = INTRINSIC (_mask_popcnt_epi16)  (src0.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_popcnt_epi16) (mask, src.x);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
