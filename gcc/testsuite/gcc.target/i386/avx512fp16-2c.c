/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512fp16-2a.c"

__m256h
__attribute__ ((noinline,noclone))
foo3 (__m256h x)
{
  return foo1(x[0]);
}

static void
do_test (void)
{
  _Float16 x = 25.3;
  union256h u = { x, 3.5f, -5.9f, 0.0f, 0.0f, 0.0f, 7.7f, 0.0f,
		  4.0f, -4.20f, 0.0f, 0.0f, 0.0f, -8.7f, 0.0f, 0.0f };

  union256h exp = { x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		    0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
  __m256h v;
  union256h a;
  memset (&v, -1, sizeof (v));
  v = foo3 (u.x);
  a.x = v;
  if (check_union256h (a, exp.a))
    abort ();
}
