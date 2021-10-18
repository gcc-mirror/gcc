/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

__m128h
__attribute__ ((noinline, noclone))
foo1 (_Float16 x)
{
  return __extension__ (__m128h)(__v8hf) { x, 0.0f, 0.0f, 0.0f,
                                           1.0f, 0.0f, 0.0f, 0.0f };
}

__m128h
__attribute__ ((noinline, noclone))
foo2 (_Float16 x, _Float16 y)
{
  return __extension__ (__m128h)(__v8hf) { x, 0.0f, 0.0f, y,
                                           3.0f, 0.0f, 0.0f, 0.0f };
}

__m256h
__attribute__ ((noinline, noclone))
foo3 (_Float16 x)
{
  return __extension__ (__m256h)(__v16hf) { x, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            1.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f };
}

__m256h
__attribute__ ((noinline, noclone))
foo4 (_Float16 x, _Float16 y)
{
  return __extension__ (__m256h)(__v16hf) { x, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, y,
                                            3.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f };
}

__m512h
__attribute__ ((noinline, noclone))
foo5 (_Float16 x)
{
  return __extension__ (__m512h)(__v32hf) { x, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            1.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f };
}

__m512h
__attribute__ ((noinline, noclone))
foo6 (_Float16 x, _Float16 y)
{
  return __extension__ (__m512h)(__v32hf) { x, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, y,
                                            3.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f };
}

static void
do_test (void)
{
  _Float16 x = 25.3;
  _Float16 y = -35.7;
  union128h u128 = { x, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f };
  union256h u256 = { x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
  union512h u512 = { x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
  __m128h v128;
  __m256h v256;
  __m512h v512;
  union128h a128;
  union256h a256;
  union512h a512;

  memset (&v128, -1, sizeof (v128));
  v128 = foo1 (x);
  a128.x = v128;
  if (check_union128h (a128, u128.a))
    abort ();
  memset (&v128, -1, sizeof (v128));
  u128.a[3] = y;
  u128.a[4] = 3.0f;
  v128 = foo2 (x, y);
  a128.x = v128;
  if (check_union128h (a128, u128.a))
    abort ();

  memset (&v256, -1, sizeof (v256));
  v256 = foo3 (x);
  a256.x = v256;
  if (check_union256h (a256, u256.a))
    abort ();
  memset (&v256, -1, sizeof (v256));
  u256.a[7] = y;
  u256.a[8] = 3.0f;
  v256 = foo4 (x, y);
  a256.x = v256;
  if (check_union256h (a256, u256.a))
    abort ();

  memset (&v512, -1, sizeof (v512));
  v512 = foo5 (x);
  a512.x = v512;
  if (check_union512h (a512, u512.a))
    abort ();
  memset (&v512, -1, sizeof (v512));
  u512.a[15] = y;
  u512.a[16] = 3.0f;
  v512 = foo6 (x, y);
  a512.x = v512;
  if (check_union512h (a512, u512.a))
    abort ();
}
