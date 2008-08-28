/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-mavx" } */

#include <stdarg.h>
#include <assert.h>

#include "avx-check.h"

struct m256d
{
  __m256d  v;
};

__m128 n1 = { -283.3, -23.3, 213.4, 1119.03 };
struct m256d n2 = { { -93.83, 893.318, 3994.3, -39484.0 } };
__m128i n3 = { 893, -3180 } ;
int n4 = 324;
double n5 = 103.3;
__m128i n6 = { -123, 2 };
__m128d n7 = { -91.387, -8193.518 };
struct m256d n8 = { { -123.3, 2.3, 3.4, -10.03 } };
__m128 n9 = { -123.3, 2.3, 3.4, -10.03 };
__m128i n10 = { 1233, -100 };
int n11 = 407;
double n12 = 304.9;
__m128i n13 = { 233, -110 };
__m256i n14 = { -1233, 23, 34, -1003 };
__m128i n15 = { -393, -180 };
__m128d n16 = { 73.0, 63.18 };
__m256 n17 = { -183.3, -22.3, 13.9, -119.3, 483.1, 122.3, -33.4, -9.37 };
__m128 n18 = { -183.3, 22.3, 13.4, -19.03 };

__m128 e1;
struct m256d e2;
__m128i e3;
int e4;
double e5;
__m128i e6;
__m128d e7;
struct m256d e8;
__m128 e9;
__m128i e10;
int e11;
double e12;
__m128i e13;
__m256i e14;
__m128i e15;
__m128d e16;
__m256 e17;
__m128 e18;

static void
__attribute__((noinline))
foo (va_list va_arglist)
{
  e4 = va_arg (va_arglist, int);
  e5 = va_arg (va_arglist, double);
  e6 = va_arg (va_arglist, __m128i);
  e7 = va_arg (va_arglist, __m128d);
  e8 = va_arg (va_arglist, struct m256d);
  e9 = va_arg (va_arglist, __m128);
  e10 = va_arg (va_arglist, __m128i);
  e11 = va_arg (va_arglist, int);
  e12 = va_arg (va_arglist, double);
  e13 = va_arg (va_arglist, __m128i);
  e14 = va_arg (va_arglist, __m256i);
  e15 = va_arg (va_arglist, __m128i);
  e16 = va_arg (va_arglist, __m128d);
  e17 = va_arg (va_arglist, __m256);
  e18 = va_arg (va_arglist, __m128);
  va_end (va_arglist);
}

static void
__attribute__((noinline))
test (__m128 a1, struct m256d a2, __m128i a3, ...)
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
avx_test (void)
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
