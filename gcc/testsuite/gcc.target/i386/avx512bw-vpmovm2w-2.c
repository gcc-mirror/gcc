/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (short *r, MASK_TYPE s)
{
  int i;
  short all_ones = 0xffff;

  for (i = 0; i < SIZE; i++)
    r[i] = ((s >> i) & 1) ? all_ones : 0;
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_w) res, res_ref;
  MASK_TYPE src = (MASK_TYPE) 0x1111abc2;

  res.x = INTRINSIC (_movm_epi16) (src);

  CALC (res_ref.a, src);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res, res_ref.a))
    abort ();
}
