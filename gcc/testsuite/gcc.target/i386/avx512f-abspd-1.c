/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (float *i1, float *r)
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
  float ck[SIZE];
  int i;
  UNION_TYPE (AVX512F_LEN, ) s, d, dm;
  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = i * ((i & 1) ? 3.5f : -7.5f);
      d.a[i] = DEFAULT_VALUE;
      dm.a[i] = DEFAULT_VALUE;
    }

  CALC (s.a, ck);

  d.x = INTRINSIC (_abs_ps) (s.x);
  dm.x = INTRINSIC (_mask_abs_ps) (dm.x, mask, s.x);

  if (UNION_CHECK (AVX512F_LEN, ) (d, ck))
    abort ();

  MASK_MERGE () (ck, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (dm, ck))
    abort ();
}
