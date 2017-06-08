/* Tests for _FloatN / _FloatNx types: compile and execution tests for
   type-generic built-in functions: __builtin_iszero, __builtin_issubnormal.
   Before including this file, define WIDTH as the value N; define EXT to 1
   for _FloatNx and 0 for _FloatN.  */

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>

#define CONCATX(X, Y) X ## Y
#define CONCAT(X, Y) CONCATX (X, Y)
#define CONCAT3(X, Y, Z) CONCAT (CONCAT (X, Y), Z)
#define CONCAT4(W, X, Y, Z) CONCAT (CONCAT (CONCAT (W, X), Y), Z)

#if EXT
# define TYPE CONCAT3 (_Float, WIDTH, x)
# define CST(C) CONCAT4 (C, f, WIDTH, x)
# define MAX CONCAT3 (FLT, WIDTH, X_MAX)
# define MIN CONCAT3 (FLT, WIDTH, X_MIN)
# define TRUE_MIN CONCAT3 (FLT, WIDTH, X_TRUE_MIN)
#else
# define TYPE CONCAT (_Float, WIDTH)
# define CST(C) CONCAT3 (C, f, WIDTH)
# define MAX CONCAT3 (FLT, WIDTH, _MAX)
# define MIN CONCAT3 (FLT, WIDTH, _MIN)
# define TRUE_MIN CONCAT3 (FLT, WIDTH, _TRUE_MIN)
#endif

extern void exit (int);
extern void abort (void);

volatile TYPE inf = __builtin_inf (), nanval = __builtin_nan ("");
volatile TYPE neginf = -__builtin_inf (), negnanval = -__builtin_nan ("");
volatile TYPE zero = CST (0.0), negzero = -CST (0.0), one = CST (1.0);
volatile TYPE max = MAX, negmax = -MAX, min = MIN, negmin = -MIN;
volatile TYPE true_min = TRUE_MIN, negtrue_min = -TRUE_MIN;
volatile TYPE sub_norm = MIN / 2.0;

int
main (void)
{
  if (__builtin_iszero (inf) == 1)
    abort ();
  if (__builtin_iszero (nanval) == 1)
    abort ();
  if (__builtin_iszero (neginf) == 1)
    abort ();
  if (__builtin_iszero (negnanval) == 1)
    abort ();
  if (__builtin_iszero (zero) != 1)
    abort ();
  if (__builtin_iszero (negzero) != 1)
    abort ();
  if (__builtin_iszero (one) == 1)
    abort ();
  if (__builtin_iszero (max) == 1)
    abort ();
  if (__builtin_iszero (negmax) == 1)
    abort ();
  if (__builtin_iszero (min) == 1)
    abort ();
  if (__builtin_iszero (negmin) == 1)
    abort ();
  if (__builtin_iszero (true_min) == 1)
    abort ();
  if (__builtin_iszero (negtrue_min) == 1)
    abort ();
  if (__builtin_iszero (sub_norm) == 1)
    abort ();

  if (__builtin_issubnormal (inf) == 1)
    abort ();
  if (__builtin_issubnormal (nanval) == 1)
    abort ();
  if (__builtin_issubnormal (neginf) == 1)
    abort ();
  if (__builtin_issubnormal (negnanval) == 1)
    abort ();
  if (__builtin_issubnormal (zero) == 1)
    abort ();
  if (__builtin_issubnormal (negzero) == 1)
    abort ();
  if (__builtin_issubnormal (one) == 1)
    abort ();
  if (__builtin_issubnormal (max) == 1)
    abort ();
  if (__builtin_issubnormal (negmax) == 1)
    abort ();
  if (__builtin_issubnormal (min) == 1)
    abort ();
  if (__builtin_issubnormal (negmin) == 1)
    abort ();
  if (__builtin_issubnormal (true_min) != 1)
    abort ();
  if (__builtin_issubnormal (negtrue_min) != 1)
    abort ();
  if (__builtin_issubnormal (sub_norm) != 1)
    abort ();
  exit (0);
}
