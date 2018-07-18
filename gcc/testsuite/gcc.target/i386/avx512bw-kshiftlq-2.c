/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

void
avx512bw_test ()
{
  __mmask64 k1 = 1;
  unsigned int i = 53;

  volatile __mmask64 r = _kshiftli_mask64 (k1, i);
  if (r != 1ULL << i)
    abort ();
}
