/* Tests for _FloatN / _FloatNx types: compile and execution tests for
   type-generic built-in functions.  Before including this file,
   define WIDTH as the value N; define EXT to 1 for _FloatNx and 0 for
   _FloatN.  */

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>

#define CONCATX(X, Y) X ## Y
#define CONCAT(X, Y) CONCATX (X, Y)
#define CONCAT3(X, Y, Z) CONCAT (CONCAT (X, Y), Z)
#define CONCAT4(W, X, Y, Z) CONCAT (CONCAT (CONCAT (W, X), Y), Z)

#if EXT
# define TYPE CONCAT3 (_Float, WIDTH, x)
# define CST(C) CONCAT4 (C, f, WIDTH, x)
# define TRUE_MIN CONCAT3 (FLT, WIDTH, X_TRUE_MIN)
#else
# define TYPE CONCAT (_Float, WIDTH)
# define CST(C) CONCAT3 (C, f, WIDTH)
# define TRUE_MIN CONCAT3 (FLT, WIDTH, _TRUE_MIN)
#endif

extern void exit (int);
extern void abort (void);

volatile TYPE inf = __builtin_inf (), nanval = __builtin_nan ("");
volatile TYPE zero = CST (0.0), negzero = -CST (0.0), one = CST (1.0);
volatile TYPE true_min = TRUE_MIN;

int
main (void)
{
  if (__builtin_signbit (inf) != 0)
    abort ();
  if (__builtin_signbit (zero) != 0)
    abort ();
  if (__builtin_signbit (negzero) == 0)
    abort ();
  if (__builtin_isfinite (nanval) != 0)
    abort ();
  if (__builtin_isfinite (inf) != 0)
    abort ();
  if (__builtin_isfinite (one) == 0)
    abort ();
  if (__builtin_isinf (nanval) != 0)
    abort ();
  if (__builtin_isinf (inf) == 0)
    abort ();
  if (__builtin_isnan (nanval) == 0)
    abort ();
  if (__builtin_isnan (inf) != 0)
    abort ();
  if (__builtin_isnormal (inf) != 0)
    abort ();
  if (__builtin_isnormal (one) == 0)
    abort ();
  if (__builtin_isnormal (nanval) != 0)
    abort ();
  if (__builtin_isnormal (zero) != 0)
    abort ();
  if (__builtin_isnormal (true_min) != 0)
    abort ();
  if (__builtin_islessequal (zero, one) != 1)
    abort ();
  if (__builtin_islessequal (one, zero) != 0)
    abort ();
  if (__builtin_islessequal (zero, negzero) != 1)
    abort ();
  if (__builtin_islessequal (zero, nanval) != 0)
    abort ();
  if (__builtin_isless (zero, one) != 1)
    abort ();
  if (__builtin_isless (one, zero) != 0)
    abort ();
  if (__builtin_isless (zero, negzero) != 0)
    abort ();
  if (__builtin_isless (zero, nanval) != 0)
    abort ();
  if (__builtin_isgreaterequal (zero, one) != 0)
    abort ();
  if (__builtin_isgreaterequal (one, zero) != 1)
    abort ();
  if (__builtin_isgreaterequal (zero, negzero) != 1)
    abort ();
  if (__builtin_isgreaterequal (zero, nanval) != 0)
    abort ();
  if (__builtin_isgreater (zero, one) != 0)
    abort ();
  if (__builtin_isgreater (one, zero) != 1)
    abort ();
  if (__builtin_isgreater (zero, negzero) != 0)
    abort ();
  if (__builtin_isgreater (zero, nanval) != 0)
    abort ();
  if (__builtin_islessgreater (zero, one) != 1)
    abort ();
  if (__builtin_islessgreater (one, zero) != 1)
    abort ();
  if (__builtin_islessgreater (zero, negzero) != 0)
    abort ();
  if (__builtin_islessgreater (zero, nanval) != 0)
    abort ();
  if (__builtin_isunordered (zero, one) != 0)
    abort ();
  if (__builtin_isunordered (one, zero) != 0)
    abort ();
  if (__builtin_isunordered (zero, negzero) != 0)
    abort ();
  if (__builtin_isunordered (zero, nanval) != 1)
    abort ();
  exit (0);
}
