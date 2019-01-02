/* { dg-do run } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512dq } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -mavx512dq" } */

#define AVX512VL
#define AVX512BW
#define AVX512DQ

#include "avx512f-pr88547-2.c"

static void
test_256 (void)
{
  test_512 ();
}

static void
test_128 (void)
{
}
