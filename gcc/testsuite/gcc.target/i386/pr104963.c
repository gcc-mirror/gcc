/* PR target/104963 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=sapphirerapids" } */

#include<immintrin.h>

__m512i
foo (__m512i a, __m512i b)
{
    return _mm512_permutexvar_epi8(a, b);
}

