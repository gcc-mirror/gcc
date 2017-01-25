/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

void
avx512bw_test ()
{
  unsigned int i = 25;
  __mmask32 k1 = 1 << i;

  volatile __mmask32 r = _kshiftri_mask32 (k1, i);
  if (r != 1)
    abort ();
}
