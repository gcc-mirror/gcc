/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void
avx512f_test () {
  volatile __mmask16 k1;
  __mmask16 k2;
  volatile short r = 0;
  volatile unsigned char r1 = 0;
  unsigned char r2;

  /* Test kortestc.  */
  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (0) );
  __asm__( "kmovw %1, %0" : "=k" (k2) : "r" (45) );

  r += _mm512_kortestc (k1, k2);

  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (0) );
  __asm__( "kmovw %1, %0" : "=k" (k2) : "r" (0) );

  r += _mm512_kortestc (k1, k2);
  if (r)
    abort ();

  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (-1) );
  __asm__( "kmovw %1, %0" : "=k" (k2) : "r" (0) );

  r += _mm512_kortestc (k1, k2);
  if (!r)
    abort ();

  r = 0;
  /* Test kortestz.  */
  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (0) );
  __asm__( "kmovw %1, %0" : "=k" (k2) : "r" (45) );

  r += _mm512_kortestz (k1, k2);

  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (-1) );
  __asm__( "kmovw %1, %0" : "=k" (k2) : "r" (0) );

  r += _mm512_kortestz (k1, k2);
  if (r)
    abort ();

  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (0) );
  __asm__( "kmovw %1, %0" : "=k" (k2) : "r" (0) );

  r += _mm512_kortestz (k1, k2);
  if (!r)
    abort ();

  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (0) );
  __asm__( "kmovw %1, %0" : "=k" (k2) : "r" (-1) );

  r1 = _kortest_mask16_u8 (k1, k2, &r2);
  if (r1 != 0 || r2 != 1)
    abort ();
}
