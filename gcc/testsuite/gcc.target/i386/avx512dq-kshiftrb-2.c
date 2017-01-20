/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#include "avx512dq-check.h"

void
avx512dq_test ()
{
  unsigned int i = 5;
  __mmask8 k1 = 1 << i;

  volatile __mmask8 r = _kshiftri_mask8 (k1, i);
  if (r != 1)
    abort ();
}
