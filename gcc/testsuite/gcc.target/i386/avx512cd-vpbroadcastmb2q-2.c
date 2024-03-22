/* { dg-do run } */
/* { dg-options "-O2 -mavx512cd" } */
/* { dg-require-effective-target avx512cd } */
/* { dg-skip-if "PR target/114150" { *-*-solaris2.* && { ! gas } } } */

#define HAVE_512
#define AVX512CD

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)

static void
CALC (long long *res, __mmask8 src)
{
  int i;

  for (i = 0; i < SIZE; i++)
    res[i] = src;
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_q) res;
  long long res_ref[SIZE];
  __mmask8 src = 0;

  for (i = 0; i < SIZE; i++)
    {
      res.a[i] = -1;
    }

  res.x = INTRINSIC (_broadcastmb_epi64) (src);

  CALC (res_ref, src);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res, res_ref))
    abort ();
}
