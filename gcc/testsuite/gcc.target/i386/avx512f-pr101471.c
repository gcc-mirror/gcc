/* { dg-do run } */
/* { dg-options "-mavx512dq -O0" } */
/* { dg-require-effective-target avx512dq } */

#include "avx512f-check.h"

static void
avx512f_test (void)
{
  __m512 x = {
      1, 1, 1, 1,
      1, 1, 1, 1,
      0, 0, 0, 0,
      0, 0, 0, 0,  };
  int ret = _mm512_fpclass_ps_mask(x, 0x26);
  if (ret != 65280)
    __builtin_abort();
}
