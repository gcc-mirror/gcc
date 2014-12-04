/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

void
CALC (int *r, MASK_TYPE s)
{
  int i;
  int all_ones = 0xffffffff;

  for (i = 0; i < SIZE; i++)
    r[i] = ((s >> i) & 1) ? all_ones : 0;
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_d) res, res_ref;
  MASK_TYPE src = (MASK_TYPE) 0x1111;

  res.x = INTRINSIC (_movm_epi32) (src);

  CALC (res_ref.a, src);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res, res_ref.a))
    abort ();
}
