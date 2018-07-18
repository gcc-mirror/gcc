/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#include "avx512dq-check.h"

void
avx512dq_test ()
{
  __mmask8 k1 = 1;
  unsigned int i = 5;

  volatile __mmask8 r = _kshiftli_mask8 (k1, i);
  if (r != 1 << i)
    abort ();
}
