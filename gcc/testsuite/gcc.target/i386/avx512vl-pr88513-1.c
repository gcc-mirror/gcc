/* PR target/88513 */
/* { dg-do run } */
/* { dg-options "-O2 -fopenmp-simd -mavx512vl -mtune=intel -mprefer-vector-width=128 -fno-vect-cost-model" } */
/* { dg-require-effective-target avx512vl } */

#define AVX512VL
#define AVX512F_LEN 512
#define AVX512F_LEN_HALF 256
#define CHECK_H "avx512f-check.h"

#include "../../gcc.dg/vect/pr59591-1.c"

#include CHECK_H

static void
test_256 (void)
{
  bar ();
}

static void
test_128 (void)
{
}
