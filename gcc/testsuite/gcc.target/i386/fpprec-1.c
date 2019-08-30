/* { dg-do run } */
/* { dg-options "-O2 -fno-math-errno -fno-trapping-math -msse2 -mfpmath=sse" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

double x[] = { __builtin_nan(""), __builtin_inf(), -__builtin_inf(),
	-0x1.fffffffffffffp+1023, 0x1.fffffffffffffp+1023,  /* +-DBL_MAX */
	-0x1p-52, 0x1p-52,				    /* +-DBL_EPSILON */
	/* nextafter/before 0.5, 1.0 and 1.5 */
	0x1.0000000000001p-1, 0x1.fffffffffffffp-2,
	0x1.0000000000001p+0, 0x1.fffffffffffffp-1,
	0x1.8000000000001p+0, 0x1.7ffffffffffffp+0,
	-0x1.0000000000001p-1, -0x1.fffffffffffffp-2,
	-0x1.0000000000001p+0, -0x1.fffffffffffffp-1,
	-0x1.8000000000001p+0, -0x1.7ffffffffffffp+0,
	-0.0, 0.0, -0.5, 0.5, -1.0, 1.0, -1.5, 1.5, -2.0, 2.0,
	-2.5, 2.5 };
#define NUM (sizeof(x)/sizeof(double))

double expect_round[] = { __builtin_nan(""), __builtin_inf(), -__builtin_inf(),
	-0x1.fffffffffffffp+1023, 0x1.fffffffffffffp+1023,
	-0.0, 0.0,
	1.0, 0.0, 1.0, 1.0, 2.0, 1.0,
	-1.0, -0.0, -1.0, -1.0, -2.0, -1.0,
	-0.0, 0.0, -1.0, 1.0, -1.0, 1.0, -2.0, 2.0, -2.0, 2.0,
	-3.0, 3.0 };

double expect_rint[] = { __builtin_nan(""), __builtin_inf(), -__builtin_inf(),
        -0x1.fffffffffffffp+1023, 0x1.fffffffffffffp+1023,
        -0.0, 0.0,
        1.0, 0.0, 1.0, 1.0, 2.0, 1.0,
        -1.0, -0.0, -1.0, -1.0, -2.0, -1.0,
        -0.0, 0.0, -0.0, 0.0, -1.0, 1.0, -2.0, 2.0, -2.0, 2.0,
        -2.0, 2.0 };

double expect_floor[] = { __builtin_nan(""), __builtin_inf(), -__builtin_inf(),
        -0x1.fffffffffffffp+1023, 0x1.fffffffffffffp+1023,
        -1.0, 0.0,
        0.0, 0.0, 1.0, 0.0, 1.0, 1.0,
        -1.0, -1.0, -2.0, -1.0, -2.0, -2.0,
        -0.0, 0.0, -1.0, 0.0, -1.0, 1.0, -2.0, 1.0, -2.0, 2.0,
        -3.0, 2.0 };

double expect_ceil[] = { __builtin_nan(""), __builtin_inf(), -__builtin_inf(),
        -0x1.fffffffffffffp+1023, 0x1.fffffffffffffp+1023,
        -0.0, 1.0,
        1.0, 1.0, 2.0, 1.0, 2.0, 2.0,
        -0.0, -0.0, -1.0, -0.0, -1.0, -1.0,
        -0.0, 0.0, -0.0, 1.0, -1.0, 1.0, -1.0, 2.0, -2.0, 2.0,
        -2.0, 3.0 };

double expect_trunc[] = { __builtin_nan(""), __builtin_inf(), -__builtin_inf(),
        -0x1.fffffffffffffp+1023, 0x1.fffffffffffffp+1023,
        -0.0, 0.0,
        0.0, 0.0, 1.0, 0.0, 1.0, 1.0,
        -0.0, -0.0, -1.0, -0.0, -1.0, -1.0,
        -0.0, 0.0, -0.0, 0.0, -1.0, 1.0, -1.0, 1.0, -2.0, 2.0,
        -2.0, 2.0 };


#define CHECK(fn) \
void check_ ## fn (void) \
{ \
  int i; \
  for (i = 0; i < NUM; ++i) \
    { \
      double res = __builtin_ ## fn (x[i]); \
      if (__builtin_memcmp (&res, &expect_ ## fn [i], sizeof(double)) != 0) \
        printf( # fn " [%i]: %.18e %.18e\n", i, expect_ ## fn [i], res), abort (); \
    } \
}

CHECK(round)
CHECK(rint)
CHECK(floor)
CHECK(ceil)
CHECK(trunc)

static void
sse2_test (void)
{
  check_round ();
  check_rint ();
  check_floor ();
  check_ceil ();
  check_trunc ();
}
