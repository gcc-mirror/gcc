/* { dg-do run { target { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } */
/* { dg-options "-fsanitize=undefined" } */

#include <emmintrin.h>

int main()
{
    unsigned char t[8+1];
    __m128 x = _mm_load_ss((const float *)(t));
    _mm_store_ss((float*)(t), x);
    return 0;
}
