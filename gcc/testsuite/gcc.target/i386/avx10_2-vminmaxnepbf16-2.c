/* { dg-do run } */
/* { dg-options "-fsignaling-nans -mfpmath=sse -O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#define AVX10_2
#define AVX512VL
#define AVX512F_LEN 256
#include "avx10_2-512-vminmaxnepbf16-2.c"

#undef AVX512F_LEN

#define AVX512F_LEN 128
#include "avx10_2-512-vminmaxnepbf16-2.c"
