/* PR rtl-optimization/17853 */
/* Contributed by Stuart Hastings <stuart@apple.com> */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -mmmx" } */
#include <mmintrin.h>
#include <stdlib.h>

__m64 global_mask;

int main()
{
    __m64 zero = _mm_setzero_si64();
    __m64 mask = _mm_cmpeq_pi8( zero, zero );
    mask = _mm_unpacklo_pi8( mask, zero );
    global_mask = mask;
    return 0;
}

