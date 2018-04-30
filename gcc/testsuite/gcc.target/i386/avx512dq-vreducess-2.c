/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"
#include <string.h>

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

#define IMM 0x23

void
CALC (float *r, float *s)
{
  int i;

  memcpy (&r[1], &s[1], 2 * sizeof(float));

  for (i = 0; i < 2; i++)
    {
      float tmp = (int) (4 * s[i]) / 4.0;
      r[i] = s[i] - tmp;
    }
}

void
TEST (void)
{
  printf("\nsize = %d\n\n", SIZE);

  union128 res1, res2, res3;
  union128 s1, s2, src;
  float res_ref[4];
  MASK_TYPE mask = MASK_VALUE;
  int j;

  for (j = 0; j < 4; j++)
    {
      s1.a[j] = j / 123.456;
      s2.a[j] = j / 123.456;
      res_ref[j] = j / 123.456;
      res1.a[j] = DEFAULT_VALUE;
      res2.a[j] = DEFAULT_VALUE;
      res3.a[j] = DEFAULT_VALUE;
    }

  res1.x = _mm_reduce_ss (s1.x, s2.x, IMM);
  res2.x = _mm_mask_reduce_ss (s1.x, mask, s1.x, s2.x, IMM);
  res3.x = _mm_maskz_reduce_ss (mask, s1.x, s2.x, IMM);

  CALC (res_ref, s2.a);

  if (check_union128 (res1, res_ref))
    abort ();
 
  MASK_MERGE () (res_ref, mask, 1);

  if (check_union128 (res2, res_ref))
    abort ();

  MASK_ZERO () (res_ref, mask, 1);

  if (check_union128 (res3, res_ref))
    abort ();

}
