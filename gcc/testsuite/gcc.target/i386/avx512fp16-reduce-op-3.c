/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16" } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-require-effective-target avx512vl } */

#define AVX512FP16
#define AVX512VL

#include "avx512f-helper.h"

#include "avx512fp16-reduce-op-2.c"

void
test_256 (void)
{
  _Float16 a[32];
  int sign = 1;
  _Float16 res1 = 0, exp1;
  _Float16 res2 = 0, exp2;
  _Float16 res3 = 0, exp3;

  for (int i = 0; i != 32; i++)
    {
      a[i] = sign * (4.0 * i);
      sign *= -1;
      if (i < 8)
	res1 += a[i];
      if (i < 16)
	res2 += a[i];
      res3 += a[i];
    }

  exp1 = reduc_add_128 (a);
  exp2 = reduc_add_256 (a);
  exp3 = reduc_add_512 (a);
  if (exp1 != res1 || exp2 != res2 || exp3 != res3)
    abort();
}

#define MAX(A, B) ((A) > (B) ? (A) : (B))
#define MIN(A, B) ((A) < (B) ? (A) : (B))

void
test_128 ()
{
  _Float16 a[32];
  int sign = 1;
  _Float16 min_res1, min_exp1, max_res1, max_exp1;
  _Float16 min_res2, min_exp2, max_res2, max_exp2;
  _Float16 min_res3, min_exp3, max_res3, max_exp3;

  for (int i = 0; i != 32; i++)
    {
      a[i] = sign * (4.9 * i * i - 8.3 * i + 14.8);
      sign *= -1;
    }

  min_res1 = max_res1 = a[0];
  for (int i = 0 ; i != 8; i++)
    {
      min_res1 = MIN (min_res1, a[i]);
      max_res1 = MAX (max_res1, a[i]);
    }

  min_res2 = min_res1;
  max_res2 = max_res1;
  for (int i = 8 ; i != 16; i++)
    {
      min_res2 = MIN (min_res2, a[i]);
      max_res2 = MAX (max_res2, a[i]);
    }

  min_res3 = min_res2;
  max_res3 = max_res2;
  for (int i = 16 ; i != 32; i++)
    {
      min_res3 = MIN (min_res3, a[i]);
      max_res3 = MAX (max_res3, a[i]);
    }

  min_exp1 = reduc_min_128 (a);
  min_exp2 = reduc_min_256 (a);
  min_exp3 = reduc_min_512 (a);
  max_exp1 = reduc_max_128 (a);
  max_exp2 = reduc_max_256 (a);
  max_exp3 = reduc_max_512 (a);

  if (min_exp1 != min_res1 || min_exp2 != min_res2 || min_exp3 != min_res3
      || max_exp1 != max_res1 || max_exp2 != max_res2 || max_exp3 != max_res3)
    abort();
}
