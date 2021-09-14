/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

void
__attribute__ ((noinline, noclone))
foo128 (_Float16 *p, __m128h x)
{
  *p = ((__v8hf)x)[0];
}

void
__attribute__ ((noinline, noclone))
foo256 (_Float16 *p, __m256h x)
{
  *p = ((__v16hf)x)[0];
}

void
__attribute__ ((noinline, noclone))
foo512 (_Float16 *p, __m512h x)
{
  *p = ((__v32hf)x)[0];
}

static void
do_test (void)
{
  _Float16 x = 25.3;
  union128h u128 = { x, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f };
  union256h u256 = { x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
  union512h u512 = { x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
  _Float16 y;

  foo128 (&y, u128.x);
  if (x != y)
    abort ();

  foo256 (&y, u256.x);
  if (x != y)
    abort ();

  foo512 (&y, u512.x);
  if (x != y)
    abort ();
}
