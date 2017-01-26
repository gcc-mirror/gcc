
/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

void
avx512bw_test ()
{
  unsigned int i = 53;
  __mmask64 k1 = 1ULL << i;

  volatile __mmask64 r = _kshiftri_mask64 (k1, i);
  if (r != 1)
    abort ();
}
