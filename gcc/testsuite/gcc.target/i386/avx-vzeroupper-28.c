/* PR target/101495  */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -mtune=generic -dp" } */

#include <immintrin.h>

extern __m256 x, y;
extern __m256 bar (void);

__m256
foo ()
{
  x = y;
  return bar ();
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
