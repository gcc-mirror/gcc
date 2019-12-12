/* { dg-do run } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512dq } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -mavx512dq" } */

#define AVX512VL
#define AVX512BW
#define AVX512DQ
#define CHECK "avx512-check.h"
#define TEST test_512

#include "avx2-pr88547-2.c"

static void
test_256 (void)
{
  return test_512 ();
}

static void
test_128 (void)
{
}
