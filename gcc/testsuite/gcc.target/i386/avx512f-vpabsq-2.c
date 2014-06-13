/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (long long *i1, long long *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    if (i1[i] < 0)
      r[i] = -i1[i];
    else
      r[i] = i1[i];
}

void
TEST (void)
{
  long long ck[SIZE];
  int i;
  UNION_TYPE (AVX512F_LEN, i_q) s, d, dm, dz;
  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = i * 7 + (i << 15) + 356;
      d.a[i] = DEFAULT_VALUE;
      dm.a[i] = DEFAULT_VALUE;
      dz.a[i] = DEFAULT_VALUE;
    }

  CALC (s.a, ck);

  d.x = INTRINSIC (_abs_epi64) (s.x);
  dz.x = INTRINSIC (_maskz_abs_epi64) (mask, s.x);
  dm.x = INTRINSIC (_mask_abs_epi64) (dm.x, mask, s.x);

  if (UNION_CHECK (AVX512F_LEN, i_q) (d, ck))
    abort ();

  MASK_MERGE (i_q) (ck, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (dm, ck))
    abort ();

  MASK_ZERO (i_q) (ck, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (dz, ck))
    abort ();
}
