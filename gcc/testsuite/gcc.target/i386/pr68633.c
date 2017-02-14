/* { dg-do run } */
/* { dg-options "-Ofast -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include <immintrin.h>

#define AVX512F

#include "avx512f-helper.h"

void abort ();

void
TEST ()
{
  __mmask16 k1, k2, k3;

  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (1) );
  __asm__( "kmovw %1, %0" : "=k" (k2) : "r" (2) );

  k3 = _mm512_kunpackb (k1, k2);
  if (k3 != 0x102)
    abort ();
}
