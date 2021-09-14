/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512fp16-3a.c"

__m512h
__attribute__ ((noinline,noclone))
foo3 (__m512h x)
{
  return foo1(x[0]);
}

static void
do_test (void)
{
  _Float16 x = 25.3;
  union512h u = { x, 3.5f, -5.9f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		  2.0f, -2.3f, 0.0f, 0.0f, 10.4f, 0.0f, 0.0f, 0.0f,
		  3.0f, -3.2f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		  4.0f, -4.20f, 0.0f, 0.0f, 0.0f, -8.7f, 0.0f, 0.0f };

  union512h exp = { x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		    0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		    0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		    0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
  __m512h v;
  union512h a;
  memset (&v, -1, sizeof (v));
  v = foo3 (u.x);
  a.x = v;
  if (check_union512h (a, exp.a))
    abort ();
}
