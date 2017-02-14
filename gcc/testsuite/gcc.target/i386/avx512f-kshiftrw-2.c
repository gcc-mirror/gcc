/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void
avx512f_test ()
{
  unsigned int i = 10;
  __mmask16 k1 = 1 << i;

  volatile __mmask16 r = _kshiftri_mask16 (k1, i);
  if (r != 1)
    abort ();
}
