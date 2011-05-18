/* PR target/49002 */
/* { dg-do compile } */
/* { dg-options "-O -mavx" } */

#include <immintrin.h>

void foo(const __m128d *from, __m256d *to, int s)
{
  __m256d var = _mm256_castpd128_pd256(from[0]);
  var = _mm256_insertf128_pd(var, from[s], 1);
  to[0] = var;
}

/* Ensure we load into xmm, not ymm.  */
/* { dg-final { scan-assembler-not "vmovapd\[\t \]*\[^,\]*,\[\t \]*%ymm" } } */
/* { dg-final { scan-assembler "vmovapd\[\t \]*\[^,\]*,\[\t \]*%xmm" } } */
