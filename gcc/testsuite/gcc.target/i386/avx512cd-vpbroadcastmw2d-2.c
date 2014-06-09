/* { dg-do run } */
/* { dg-options "-O2 -mavx512cd" } */
/* { dg-require-effective-target avx512cd } */

#define HAVE_512
#define AVX512CD

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)

static void
CALC (int *res, __mmask16 src)
{
  int i;

  for (i = 0; i < SIZE; i++)
    res[i] = src;
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_d) res;
  int res_ref[SIZE];
  __mmask16 src = 0;

  for (i = 0; i < SIZE; i++)
    {
      res.a[i] = -1;
    }

  res.x = INTRINSIC (_broadcastmw_epi32) (src);

  CALC (res_ref, src);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res, res_ref))
    abort ();
}
