/* { dg-do run } */
/* { dg-options "-mavx512vl -O2 -std=gnu99" } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target c99_runtime } */

#define AVX512VL
#define AVX512F_LEN 512
#define AVX512F_LEN_HALF 256
#include "avx512f-vfixupimmsd-2.c"

static void
test_256 (void)
{
  test_512 ();
}

static void
test_128 (void)
{
}
