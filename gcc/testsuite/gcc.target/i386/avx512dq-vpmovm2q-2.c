/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

void
CALC (long long *r, MASK_TYPE s)
{
  int i;
  long long all_ones = 0xffffffffffffffff;

  for (i = 0; i < SIZE; i++)
    r[i] = ((s >> i) & 1) ? all_ones : 0;
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_q) res, res_ref;
  MASK_TYPE src = (MASK_TYPE) 0xff;

  res.x = INTRINSIC (_movm_epi64) (src);

  CALC (res_ref.a, src);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res, res_ref.a))
    abort ();
}
