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
  *p = ((__v8hf)x)[4];
}

void
__attribute__ ((noinline, noclone))
foo256 (_Float16 *p, __m256h x)
{
  *p = ((__v16hf)x)[10];
}

void
__attribute__ ((noinline, noclone))
foo512 (_Float16 *p, __m512h x)
{
  *p = ((__v32hf)x)[30];
}

static void
do_test (void)
{
  _Float16 x = 25.3;
  union128h u128 = { 0.0f, x, 0.0f, 0.0f, x, 0.0f, 0.0f, x };
  union256h u256 = { x, 0.0f, 0.0f, 0.0f, x, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, x, 0.0f, 0.0f, x, 0.0f, 0.0f };
  union512h u512 = { x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, 0.0f, x, 0.0f, 0.0f, x, 0.0f };
  __m128h v128 = _mm_setr_ph (0.0f, x, 0.0f, 0.0f,
			      x, 0.0f, 0.0f, x);
  __m256h v256 = _mm256_setr_ph (x, 0.0f, 0.0f, 0.0f,
				 x, 0.0f, 0.0f, 0.0f,
				 0.0f, 0.0f, x, 0.0f,
				 0.0f, x, 0.0f, 0.0f);
  __m512h v512 = _mm512_setr_ph (x, 0.0f, 0.0f, 0.0f,
				 0.0f, 0.0f, 0.0f, 0.0f,
				 0.0f, x, 0.0f, 0.0f,
				 0.0f, 0.0f, 0.0f, 0.0f,
				 0.0f, 0.0f, x, 0.0f,
				 0.0f, 0.0f, 0.0f, 0.0f,
				 0.0f, 0.0f, 0.0f, x,
				 0.0f, 0.0f, x, 0.0f);
  union128h a128;
  union256h a256;
  union512h a512;
  _Float16 y;

  a128.x = v128;
  if (check_union128h (a128, u128.a))
    abort ();

  a256.x = v256;
  if (check_union256h (a256, u256.a))
    abort ();

  a512.x = v512;
  if (check_union512h (a512, u512.a))
    abort ();

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
