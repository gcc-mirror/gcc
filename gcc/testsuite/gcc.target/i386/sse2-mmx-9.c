/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-mmx" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

#include <string.h>

#define FLOAT_X	2.3456
#define FLOAT_Y	-4.5987

static float expected_x = FLOAT_X;
static float expected_y = FLOAT_Y;
static __v2sf expected1 = { FLOAT_X, FLOAT_Y };
static __v2sf expected2 = { FLOAT_X, 0 };
static __v2sf expected3 = { FLOAT_X, FLOAT_X };

float
__attribute__((noinline, noclone))
foo1 (__m64 x)
{
  return ((__v2sf) x)[0];
}

float
__attribute__((noinline, noclone))
foo2 (__m64 x)
{
  return ((__v2sf) x)[1];
}

__m64
__attribute__((noinline, noclone))
foo3 (float x)
{
  return __extension__ (__m64) (__v2sf) { x, 0 };
}

__m64
__attribute__((noinline, noclone))
foo4 (float x)
{
  return __extension__ (__m64) (__v2sf) { x, x };
}

__m64
__attribute__((noinline, noclone))
foo5 (float x, float y)
{
  return __extension__ (__m64) (__v2sf) { x, y };
}

void
__attribute__((noinline))
sse2_test (void)
{
  __m64 res;
  float x;

  x = foo1 ((__m64) expected1);
  if (x != expected_x)
    abort ();

  x = foo2 ((__m64) expected1);
  if (x != expected_y)
    abort ();

  res = foo3 (FLOAT_X); 
  if (memcmp (&res, &expected2, sizeof (res)))
    abort ();

  res = foo4 (FLOAT_X); 
  if (memcmp (&res, &expected3, sizeof (res)))
    abort ();

  res = foo5 (FLOAT_X, FLOAT_Y); 
  if (memcmp (&res, &expected1, sizeof (res)))
    abort ();
}
