/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

static void
do_test (void)
{
  union128i_w u;
  short b = 128;
  short e[8] = {0,0,0,0,0,0,0,0};

  u.x = _mm_cvtsi16_si128 (b);

  e[0] = b;

  if (check_union128i_w (u, e))
    abort ();
  u.a[0] = 123;
  b = _mm_cvtsi128_si16 (u.x);
  if (u.a[0] != b)
    abort();  
}
