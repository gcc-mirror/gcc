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
  union128 src1, src2, res;
  float res_ref[4];
  int i;
  
  for (i = 0; i < 4; i++)
    {
      src1.a[i] = 179.345 - 6.5645 * i;
      src2.a[i] = 179345.006 + 6.5645 * i;
      res_ref[i] = src1.a[i];
    }

  res_ref[0] = 1.0 / src2.a[0];

  res.x = _mm_rcp28_round_ss (src1.x, src2.x, _MM_FROUND_NO_EXC);

  if (checkVf (res.a, res_ref, 4))
    abort ();
}
