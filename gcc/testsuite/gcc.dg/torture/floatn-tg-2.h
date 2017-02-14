/* Tests for _FloatN / _FloatNx types: compile and execution tests for
   type-generic built-in functions: __builtin_isinf_sign.  Before
   including this file, define WIDTH as the value N; define EXT to 1
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
#else
# define TYPE CONCAT (_Float, WIDTH)
# define CST(C) CONCAT3 (C, f, WIDTH)
# define MAX CONCAT3 (FLT, WIDTH, _MAX)
#endif

extern void exit (int);
extern void abort (void);

volatile TYPE inf = __builtin_inf (), nanval = __builtin_nan ("");
volatile TYPE neginf = -__builtin_inf (), negnanval = -__builtin_nan ("");
volatile TYPE zero = CST (0.0), negzero = -CST (0.0), one = CST (1.0);
volatile TYPE max = MAX, negmax = -MAX;

int
main (void)
{
  if (__builtin_isinf_sign (inf) != 1)
    abort ();
  if (__builtin_isinf_sign (neginf) != -1)
    abort ();
  if (__builtin_isinf_sign (nanval) != 0)
    abort ();
  if (__builtin_isinf_sign (negnanval) != 0)
    abort ();
  if (__builtin_isinf_sign (zero) != 0)
    abort ();
  if (__builtin_isinf_sign (negzero) != 0)
    abort ();
  if (__builtin_isinf_sign (one) != 0)
    abort ();
  if (__builtin_isinf_sign (max) != 0)
    abort ();
  if (__builtin_isinf_sign (negmax) != 0)
    abort ();
  exit (0);
}
