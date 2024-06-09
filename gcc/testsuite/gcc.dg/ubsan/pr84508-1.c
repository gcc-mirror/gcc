/* { dg-do run { target { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } */
/* { dg-options "-fsanitize=undefined" } */

#include <emmintrin.h>

int main()
{
    unsigned char t[16+1];
    __m128d x = _mm_load_sd((const double *)(t+1));
    _mm_store_sd((double*)(t+1), x);
    return 0;
}
