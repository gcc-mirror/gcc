/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512fp16-1a.c"

__m128h
__attribute__ ((noinline,noclone))
foo3 (__m128h x)
{
  return foo1(x[0]);
}

static void
do_test (void)
{
  union128h u = { -1.2f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f };
  union128h a, b = { -1.2f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f};
  __m128h v;
  v = foo3 (u.x);
  a.x = v;
  if (check_union128h (a, b.a))
    abort ();
}
