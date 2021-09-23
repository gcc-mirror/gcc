/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mno-avx512f -mtune=generic -dp" } */

#include <immintrin.h>

extern __m256 x, y;
extern void (*bar) (void);

void
foo ()
{
  x = y;
  bar ();
}

/* { dg-final { scan-assembler-times "avx_vzeroupper" 1 } } */
