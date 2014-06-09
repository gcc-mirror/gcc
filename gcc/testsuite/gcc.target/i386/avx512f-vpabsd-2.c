/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (int *i1, int *r)
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
  int ck[SIZE];
  int i;
  UNION_TYPE (AVX512F_LEN, i_d) s, d, dm, dz;
  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = i * 7 + (i << 15) + 356;
      d.a[i] = DEFAULT_VALUE;
      dm.a[i] = DEFAULT_VALUE;
      dz.a[i] = DEFAULT_VALUE;
    }

  CALC (s.a, ck);

  d.x = INTRINSIC (_abs_epi32) (s.x);
  dz.x = INTRINSIC (_maskz_abs_epi32) (mask, s.x);
  dm.x = INTRINSIC (_mask_abs_epi32) (dm.x, mask, s.x);

  if (UNION_CHECK (AVX512F_LEN, i_d) (d, ck))
    abort ();

  MASK_MERGE (i_d) (ck, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (dm, ck))
    abort ();

  MASK_ZERO (i_d) (ck, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (dz, ck))
    abort ();
}
