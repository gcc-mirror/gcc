/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

void
avx512bw_test ()
{
  __mmask64 k1, k2;
  unsigned char r1, r2;

  k1 = _cvtu64_mask64(0);
  k2 = _cvtu64_mask64(-1);

  r1 = _kortest_mask64_u8(k1, k2, &r2);

  if (r1 != 0 || r2 != 1)
    abort ();
}
