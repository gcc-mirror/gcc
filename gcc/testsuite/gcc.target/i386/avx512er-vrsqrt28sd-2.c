/* { dg-do run } */
/* { dg-require-effective-target avx512er } */
/* { dg-options "-O2 -mavx512er" } */

#include "avx512er-check.h"
#include "avx512f-mask-type.h"
#include "avx512f-helper.h"
#include <math.h>

void static
avx512er_test (void)
{
  union128d src1, src2, res;
  double res_ref[2];
  int i;
  
  for (i = 0; i < 2; i++)
    {
      src1.a[i] = 179.345 - 6.5645 * i;
      src2.a[i] = 45 - 6.5645 * i;
      res_ref[i] = src1.a[i];
    }

  res_ref[0] = 1.0 / sqrt (src2.a[0]);

  res.x = _mm_rsqrt28_round_sd (src1.x, src2.x, _MM_FROUND_NO_EXC);

  if (checkVd (res.a, res_ref, 2))
    abort ();
}
