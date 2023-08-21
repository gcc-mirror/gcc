/* PR tree-optimization/88464 */
/* { dg-do run { target { avx512vl } } } */
/* { dg-options "-O3 -mavx512vl -mprefer-vector-width=128 -mtune=skylake-avx512 -mgather" } */

#define AVX512VL
#define AVX512F_LEN 512
#define AVX512F_LEN_HALF 256

#include "avx512f-pr88464-2.c"

static void
test_256 (void)
{
  avx512f_test ();
}

static void
test_128 (void)
{
}
