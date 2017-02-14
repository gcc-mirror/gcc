/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

void
avx512bw_test ()
{
  volatile __mmask32 k1, k2;
  unsigned char r1, r2;

  __asm__( "kmovd %1, %0" : "=k" (k1) : "r" (0) );
  __asm__( "kmovd %1, %0" : "=k" (k2) : "r" (-1) );

  r1 = _ktest_mask32_u8(k1, k2, &r2);

  if (r1 != 1 || r2 != 0)
    abort ();
}
