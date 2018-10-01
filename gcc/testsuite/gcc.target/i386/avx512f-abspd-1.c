/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (double *i1, double *r)
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
  double ck[SIZE];
  int i;
  UNION_TYPE (AVX512F_LEN, d) s, d, dm;
  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = i * ((i & 1) ? 3.5 : -7.5);
      d.a[i] = DEFAULT_VALUE;
      dm.a[i] = DEFAULT_VALUE;
    }

  CALC (s.a, ck);

  d.x = INTRINSIC (_abs_pd) (s.x);
  dm.x = INTRINSIC (_mask_abs_pd) (dm.x, mask, s.x);

  if (UNION_CHECK (AVX512F_LEN, d) (d, ck))
    abort ();

  MASK_MERGE (d) (ck, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (dm, ck))
    abort ();
}
