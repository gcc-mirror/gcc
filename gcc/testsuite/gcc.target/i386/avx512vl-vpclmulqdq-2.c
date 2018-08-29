/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw -mavx512vl -mvpclmulqdq" } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target vpclmulqdq } */

#define AVX512VL
#define AVX512F_LEN 256
#define AVX512F_LEN_HALF 128
#include "avx512f-vpclmulqdq-2.c"

#undef AVX512F_LEN
#undef AVX512F_LEN_HALF

void
test_128 () {}
