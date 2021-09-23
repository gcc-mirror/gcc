/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake" } */

#include <x86intrin.h>

extern __m256 x1;
extern __m256d x2;
extern __m256i x3;

extern __m256 bar (void);

void
foo1 (void)
{
  bar ();
  x1 = _mm256_setzero_ps ();
}

void
foo2 (void)
{
  bar ();
  x2 = _mm256_setzero_pd ();
}

void
foo3 (void)
{
  bar ();
  x3 = _mm256_setzero_si256 ();
}

/* { dg-final { scan-assembler-times "vzeroupper" 3 } } */
