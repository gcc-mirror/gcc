/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler-times "vbroadcasti128" 1 } } */
/* { dg-final { scan-assembler-not {(?n)vlddqu.*xmm} } } */

#include <immintrin.h>
__m256i foo(void *data) {
    __m128i X1 = _mm_lddqu_si128((__m128i*)data);
    __m256i V1 = _mm256_broadcastsi128_si256 (X1);
    return V1;
}
