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
CALC (double *r, double *s)
{
  int i;

  memcpy (&r[1], &s[1], sizeof(double));

  for (i = 0; i < 1; i++)
    {
      double tmp = (int) (4 * s[i]) / 4.0;
      r[i] = s[i] - tmp;
    }
}

void
TEST (void)
{
  union128d res1, res2, res3, res4, res5, res6;
  union128d s1, s2, src;
  double res_ref[2];
  MASK_TYPE mask = MASK_VALUE;
  int j;

  for (j = 0; j < 2; j++)
    {
      s1.a[j] = j / 123.456;
      s2.a[j] = j / 123.456;
      res_ref[j] = j / 123.456;
      res1.a[j] = DEFAULT_VALUE;
      res2.a[j] = DEFAULT_VALUE;
      res3.a[j] = DEFAULT_VALUE;
      res4.a[j] = DEFAULT_VALUE;
      res5.a[j] = DEFAULT_VALUE;
      res6.a[j] = DEFAULT_VALUE;
    }

  res1.x = _mm_reduce_sd (s1.x, s2.x, IMM);
  res2.x = _mm_mask_reduce_sd (s1.x, mask, s1.x, s2.x, IMM);
  res3.x = _mm_maskz_reduce_sd (mask, s1.x, s2.x, IMM);
  res4.x = _mm_reduce_round_sd (s1.x, s2.x, IMM,_MM_FROUND_TO_NEAREST_INT
				| _MM_FROUND_NO_EXC);
  res5.x = _mm_mask_reduce_round_sd (s1.x, mask, s1.x, s2.x, IMM,
				     _MM_FROUND_TO_NEAREST_INT
				     | _MM_FROUND_NO_EXC);
  res6.x = _mm_maskz_reduce_round_sd (mask, s1.x, s2.x, IMM,
				      _MM_FROUND_TO_NEAREST_INT
				      | _MM_FROUND_NO_EXC);

  CALC (res_ref, s2.a);

  if (check_union128d (res1, res_ref))
    abort ();
 
  if (check_union128d (res4, res_ref))
    abort ();
 
  MASK_MERGE (d) (res_ref, mask, 1);

  if (check_union128d (res2, res_ref))
    abort ();

  if (check_union128d (res5, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, 1);

  if (check_union128d (res3, res_ref))
    abort ();

  if (check_union128d (res6, res_ref))
    abort ();

}
