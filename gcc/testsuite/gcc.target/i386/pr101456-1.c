/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake" } */

#include <x86intrin.h>

extern __m256 x1;
extern __m256d x2;
extern __m256i x3;

extern void bar (void);

void
foo1 (void)
{
  x1 = _mm256_setzero_ps ();
  bar ();
}

void
foo2 (void)
{
  x2 = _mm256_setzero_pd ();
  bar ();
}

void
foo3 (void)
{
  x3 = _mm256_setzero_si256 ();
  bar ();
}

/* { dg-final { scan-assembler-not "vzeroupper" } } */
