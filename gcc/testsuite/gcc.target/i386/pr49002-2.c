/* PR target/49002 */
/* { dg-do compile } */
/* { dg-options "-O -mavx" } */

#include <immintrin.h>

void foo(const __m128d from, __m256d *to)
{
  *to = _mm256_castpd128_pd256(from);
}

/* Ensure we store ymm, not xmm.  */
/* { dg-final { scan-assembler-not "vmovapd\[\t \]*%xmm\[0-9\]\+,\[^,\]*" } } */
/* { dg-final { scan-assembler-not "vmovaps\[\t \]*%xmm\[0-9\]\+,\[^,\]*" } } */
/* { dg-final { scan-assembler "vmovap\[sd\]\[\t \]*%ymm\[0-9\]\+,\[^,\]*" } } */
