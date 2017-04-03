/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#include "avx512dq-check.h"

void
avx512dq_test ()
{
  volatile __mmask16 k1, k2;
  unsigned char r1, r2;

  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (0) );
  __asm__( "kmovw %1, %0" : "=k" (k2) : "r" (-1) );

  r1 = _ktest_mask16_u8(k1, k2, &r2);

  if (r1 != 1 || r2 != 0)
    abort ();
}
