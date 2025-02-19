/* { dg-do run } */
/* { dg-options "-fsignaling-nans -mfpmath=sse -O2 -march=x86-64-v3 -mavx10.2-256" } */
/* { dg-require-effective-target avx10_2_256 } */

#define AVX10_2
#define AVX512VL
#define AVX512F_LEN 256
typedef _Float16 __m256h __attribute__ ((__vector_size__ (32), __may_alias__));
#include "avx10_2-512-vminmaxph-2.c"

#undef AVX512F_LEN

#define AVX512F_LEN 128
typedef _Float16 __m128h __attribute__ ((__vector_size__ (16), __may_alias__));
#include "avx10_2-512-vminmaxph-2.c"
