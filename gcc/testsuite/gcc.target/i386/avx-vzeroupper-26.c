/* { dg-do compile } */
/* { dg-options "-Os -mavx -mtune=generic -dp" } */

#include <immintrin.h>

extern __m256 x, y;
extern void (*bar) (void);

void
foo ()
{
  x = y;
  bar ();
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
