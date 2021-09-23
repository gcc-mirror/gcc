/* { dg-do run } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-options "-mavx512fp16" } */

#include <stdarg.h>
#include <assert.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

__m128 n1 = { -283.3, -23.3, 213.4, 1119.03 };
__m256d n2 = { -93.83, 893.318, 3994.3, -39484.0 };
__m128h n3 = { 11.5f16, -31.80f16, 242.3f16, 136.4f16, 42.8f16, -22.8f16, 343.8f16, 215.4f16 } ;
_Float16 n4 = 32.4f16;
double n5 = 103.3;
__m128h n6 = { -12.3f16, 2.0f16, 245.9f16, -432.1f16, 53.5f16, -13.4f16, 432.5f16, 482.4f16 };
__m128d n7 = { -91.387, -8193.518 };
__m256d n8 = { -123.3, 2.3, 3.4, -10.03 };
__m128 n9 = { -123.3, 2.3, 3.4, -10.03 };
__m128h n10 = { 123.3f16, -100.0f16, 246.9f16, 13.4f16, -134.4f16, 35.4f16, 156.5f16, 953.1f16 };
_Float16 n11 = 40.7f16;
double n12 = 304.9;
__m128h n13 = { 23.3f16, -11.0f16, 24.5f16, -24.5f16, 535.4f16, 35.4f16, -13.4f16, 14.5f16 };
__m256h n14 = { -123.3f16, 23.9f16, 34.4f16, -100.3f16, 284.4f16, 352.5f16, 131.5f16, -13.2f16,
		131.4f16, 382.5f16, 38.5f16, 99.6f16, 423.2f16, -12.44f16, 43.2f16, -34.45f16 };
__m512h n15 = { -39.3f16, -180.9f16, 13.4f16, 35.4f16, -41.1f16, -14.4f16, 24.5f16, 53.54f16,
		 238.4f16, -134.8f16, 24.5f16, 35.6f16, -346.7f16, -43.4f16, -535.3f16, 324.7f16,
		 82.5f16, 21.4f16, 24.4f16, 53.4f16, 23.5f16, -24.4f16, -34.5f16, -32.5f16,
		 23.6f16, -13.4f16, 24.5f16, 35.5f16, -34.4f16, -24.5f16, -34.5f16, 13.5f16 };
__m128d n16 = { 73.0, 63.18 };
__m256 n17 = { -183.3, -22.3, 13.9, -119.3, 483.1, 122.3, -33.4, -9.37 };
__m128 n18 = { -183.3, 22.3, 13.4, -19.03 };

__m128 e1;
__m256d e2;
__m128h e3;
_Float16 e4;
double e5;
__m128h e6;
__m128d e7;
__m256d e8;
__m128 e9;
__m128h e10;
_Float16 e11;
double e12;
__m128h e13;
__m256h e14;
__m512h e15;
__m128d e16;
__m256 e17;
__m128 e18;

static void
__attribute__((noinline))
foo (va_list va_arglist)
{
  e4 = va_arg (va_arglist, _Float16);
  e5 = va_arg (va_arglist, double);
  e6 = va_arg (va_arglist, __m128h);
  e7 = va_arg (va_arglist, __m128d);
  e8 = va_arg (va_arglist, __m256d);
  e9 = va_arg (va_arglist, __m128);
  e10 = va_arg (va_arglist, __m128h);
  e11 = va_arg (va_arglist, _Float16);
  e12 = va_arg (va_arglist, double);
  e13 = va_arg (va_arglist, __m128h);
  e14 = va_arg (va_arglist, __m256h);
  e15 = va_arg (va_arglist, __m512h);
  e16 = va_arg (va_arglist, __m128d);
  e17 = va_arg (va_arglist, __m256);
  e18 = va_arg (va_arglist, __m128);
  va_end (va_arglist);
}

static void
__attribute__((noinline))
test (__m128 a1, __m256d a2, __m128h a3, ...)
{
  va_list va_arglist;

  e1 = a1;
  e2 = a2;
  e3 = a3;
  va_start (va_arglist, a3);
  foo (va_arglist);
  va_end (va_arglist);
}

static void
do_test (void)
{
  test (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12,
	n13, n14, n15, n16, n17, n18);
  assert (__builtin_memcmp (&e1, &n1, sizeof (e1)) == 0);
  assert (__builtin_memcmp (&e2, &n2, sizeof (e2)) == 0);
  assert (__builtin_memcmp (&e3, &n3, sizeof (e3)) == 0);
  assert (n4 == e4);
  assert (n5 == e5);
  assert (__builtin_memcmp (&e6, &n6, sizeof (e6)) == 0);
  assert (__builtin_memcmp (&e7, &n7, sizeof (e7)) == 0);
  assert (__builtin_memcmp (&e8, &n8, sizeof (e8)) == 0);
  assert (__builtin_memcmp (&e9, &n9, sizeof (e9)) == 0);
  assert (__builtin_memcmp (&e10, &n10, sizeof (e10)) == 0);
  assert (n11 == e11);
  assert (n12 == e12);
  assert (__builtin_memcmp (&e13, &n13, sizeof (e13)) == 0);
  assert (__builtin_memcmp (&e14, &n14, sizeof (e14)) == 0);
  assert (__builtin_memcmp (&e15, &n15, sizeof (e15)) == 0);
  assert (__builtin_memcmp (&e16, &n16, sizeof (e16)) == 0);
  assert (__builtin_memcmp (&e17, &n17, sizeof (e17)) == 0);
  assert (__builtin_memcmp (&e18, &n18, sizeof (e18)) == 0);
}
