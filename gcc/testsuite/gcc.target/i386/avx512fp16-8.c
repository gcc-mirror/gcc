/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

_Float16
__attribute__ ((noinline, noclone))
foo128 (__m128h x)
{
  return ((__v8hf)x)[4];
}

_Float16
__attribute__ ((noinline, noclone))
foo256 (__m256h x)
{
  return ((__v16hf)x)[10];
}

_Float16
__attribute__ ((noinline, noclone))
foo512 (__m512h x)
{
  return ((__v32hf)x)[30];
}

static void
do_test (void)
{
  _Float16 x = 25.3;
  union128h u128 = { 0.0f, 0.0f, 0.0f, 0.0f, x, 0.0f, 0.0f, 0.0f };
  union256h u256 = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
  union512h u512 = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, x, 0.0f };

  if (foo128 (u128.x) != x)
    abort ();

  if (foo256 (u256.x) != x)
    abort ();

  if (foo512 (u512.x) != x)
    abort ();
}
