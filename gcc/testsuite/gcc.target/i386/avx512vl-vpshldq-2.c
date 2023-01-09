/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mavx512vbmi2" } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512vbmi2 } */

#define AVX512VL
#define AVX512F_LEN 256
#define AVX512F_LEN_HALF 128
#include "avx512f-vpshldq-2.c"

#undef AVX512F_LEN
#undef AVX512F_LEN_HALF

#define AVX512F_LEN 128
#define AVX512F_LEN_HALF 128
#include "avx512f-vpshldq-2.c"
