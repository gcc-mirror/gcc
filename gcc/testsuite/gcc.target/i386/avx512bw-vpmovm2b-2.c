/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 8)
#include "avx512f-mask-type.h"

void
CALC (char *r, MASK_TYPE s)
{
  int i;
  char all_ones = 0xff;

  for (i = 0; i < SIZE; i++)
    r[i] = ((s >> i) & 1) ? all_ones : 0;
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_b) res, res_ref;
  MASK_TYPE src = (MASK_TYPE) 0x1111abeffeec1234;

  res.x = INTRINSIC (_movm_epi8) (src);

  CALC (res_ref.a, src);

  if (UNION_CHECK (AVX512F_LEN, i_b) (res, res_ref.a))
    abort ();
}
