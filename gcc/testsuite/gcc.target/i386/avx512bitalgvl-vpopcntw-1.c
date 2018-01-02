/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mavx512bitalg" } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512bitalg } */

#define AVX512VL
#define AVX512F_LEN 256
#define AVX512F_LEN_HALF 128
#include "avx512bitalg-vpopcntw-1.c"

#undef AVX512F_LEN
#undef AVX512F_LEN_HALF

#define AVX512F_LEN 128
#define AVX512F_LEN_HALF 128
#include "avx512bitalg-vpopcntw-1.c"
