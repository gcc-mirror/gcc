/* PR target/65671 */
/* { dg-do assemble } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-O2 -mavx512vl -ffixed-ymm16" } */

#include <x86intrin.h>

register __m256d a asm ("ymm16");
__m128d b;

void
foo ()
{
  b = _mm256_extractf128_pd (a, 1);
}
