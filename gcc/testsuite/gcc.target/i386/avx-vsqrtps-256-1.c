/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  union256 u, s1;
  float e[8] = {0x1.7edeccp+10, 0x1.e3fa46p+8, 0x1.dabbcep+7, 0x1.d93e0cp+9,\
                0x1.d3881cp+7, 0x1.54abbp+4, 0x1.19195cp+5, 0x1.719742p+5};

  s1.x = _mm256_set_ps (2134.3343,1234.635654,453.345635,54646.464356, \
                        895833.346347,56343,234234.34563,2345434.67832);
  u.x = _mm256_sqrt_ps (s1.x);

  if (check_union256 (u, e))
    abort ();
}
