/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-require-effective-target avx512vl } */

#define AVX512VL
#define AVX512F_LEN 256
#define AVX512F_LEN_HALF 128
#include "avx512f-vinsertf32x4-2.c"

void
test_128 () {}
