/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

#ifndef CTRL
#define CTRL 129
#endif

#ifndef SELECT4_DEFINED
#define SELECT4_DEFINED
static int
select4 (int i, unsigned ctrl)
{
  int res;
  switch (i % 4)
    {
    case 0:
      res = (CTRL & 0x03);
      break;
    case 1:
      res = ((CTRL & 0x0c) >> 2);
      break;
    case 2:
      res = ((CTRL & 0x30) >> 4);
      break;
    case 3:
      res = ((CTRL & 0xc0) >> 6);
      break;
    }
  return res;
}
#endif

static void
CALC (float *s, float *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      r[i] = s[(4 * (i / 4)) + select4 (i, CTRL)];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN,) s1, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i + 10.;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_permute_ps) (s1.x, CTRL);
  res2.x = INTRINSIC (_mask_permute_ps) (res2.x, mask, s1.x, CTRL);
  res3.x = INTRINSIC (_maskz_permute_ps) (mask, s1.x, CTRL);

  CALC (s1.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN,) (res1, res_ref))
    abort ();

  MASK_MERGE ()(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (res2, res_ref))
    abort ();

  MASK_ZERO ()(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (res3, res_ref))
    abort ();
}
