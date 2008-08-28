/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

#include <stdarg.h>
#include <assert.h>

#include "sse2-check.h"

struct m128
{
  __m128 v;
};

struct m128 n1 = { { -283.3, -23.3, 213.4, 1119.03 } };
__m128d n2 = { -93.83, 893.318 };
__m128i n3 = { 893, -3180 } ;
int n4 = 324;
double n5 = 103.3;
__m128i n6 = { -123, 2 };
__m128d n7 = { -91.387, -8193.518 };
struct m128 n8 = { { -123.3, 2.3, 3.4, -10.03 } };
__m128i n9 = { 1233, -100 };
int n10 = 407;
double n11 = 304.9;
__m128i n12 = { 233, -110 };
__m128i n13 = { -393, -180 };
__m128d n14 = { 73.0, 63.18 };
struct m128 n15 = { { -183.3, 22.3, 13.4, -19.03 } };

struct m128 e1;
__m128d e2;
__m128i e3;
int e4;
double e5;
__m128i e6;
__m128d e7;
struct m128 e8;
__m128i e9;
int e10;
double e11;
__m128i e12;
__m128i e13;
__m128d e14;
struct m128 e15;

static void
__attribute__((noinline))
test (struct m128 a1, __m128d a2, __m128i a3, ...)
{
  va_list va_arglist;

  e1 = a1;
  e2 = a2;
  e3 = a3;
  va_start (va_arglist, a3);
  e4 = va_arg (va_arglist, int);
  e5 = va_arg (va_arglist, double);
  e6 = va_arg (va_arglist, __m128i);
  e7 = va_arg (va_arglist, __m128d);
  e8 = va_arg (va_arglist, struct m128);
  e9 = va_arg (va_arglist, __m128i);
  e10 = va_arg (va_arglist, int);
  e11 = va_arg (va_arglist, double);
  e12 = va_arg (va_arglist, __m128i);
  e13 = va_arg (va_arglist, __m128i);
  e14 = va_arg (va_arglist, __m128d);
  e15 = va_arg (va_arglist, struct m128);
  va_end (va_arglist);
}

static void
sse2_test (void)
{
  test (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15);
  assert (__builtin_memcmp (&e1, &n1, sizeof (e1)) == 0);
  assert (__builtin_memcmp (&e2, &n2, sizeof (e2)) == 0);
  assert (__builtin_memcmp (&e3, &n3, sizeof (e3)) == 0);
  assert (n4 == e4);
  assert (n5 == e5);
  assert (__builtin_memcmp (&e6, &n6, sizeof (e6)) == 0);
  assert (__builtin_memcmp (&e7, &n7, sizeof (e7)) == 0);
  assert (__builtin_memcmp (&e8, &n8, sizeof (e8)) == 0);
  assert (__builtin_memcmp (&e9, &n9, sizeof (e9)) == 0);
  assert (n10 == e10);
  assert (n11 == e11);
  assert (__builtin_memcmp (&e12, &n12, sizeof (e12)) == 0);
  assert (__builtin_memcmp (&e13, &n13, sizeof (e13)) == 0);
  assert (__builtin_memcmp (&e14, &n14, sizeof (e14)) == 0);
  assert (__builtin_memcmp (&e15, &n15, sizeof (e15)) == 0);
}
