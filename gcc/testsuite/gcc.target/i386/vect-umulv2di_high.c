/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-avx" } */

#include <stdint.h>

void mulu64_high (uint64_t * __restrict r, uint64_t *a, uint64_t *b)
{
  for (int i = 0; i < 128; ++i)
    r[i] = ((unsigned __int128)a[i] * (unsigned __int128)b[i]) >> 64;
}

/* Vectorizing two 64bit unsigned highpart multiplies with 4 pmuludq plus
   more is slower than two iterations with a mulq each.  */
/* { dg-final { scan-assembler-times "pmuludq" 0 } } */
