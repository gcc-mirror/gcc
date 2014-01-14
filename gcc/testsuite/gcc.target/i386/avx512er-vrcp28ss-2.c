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
  union128 src, res;
  float res_ref[4];
  int i;
  
  for (i = 0; i < 4; i++)
    {
      src.a[i] = 179.345 - 6.5645 * i;
      res_ref[i] = src.a[i];
    }

  res_ref[0] = 1.0 / src.a[0];

  res.x = _mm_rsqrt28_round_ss (src.x, src.x, _MM_FROUND_NO_EXC);

  if (checkVf (res.a, res_ref, 4))
    abort ();
}
