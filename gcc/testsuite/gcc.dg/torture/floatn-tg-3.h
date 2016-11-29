/* Tests for _FloatN / _FloatNx types: compile and execution tests for
   type-generic built-in functions: __builtin_fpclassify.  Before
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

#define FP_NAN 0
#define FP_INFINITE 1
#define FP_ZERO 2
#define FP_SUBNORMAL 3
#define FP_NORMAL 4

#define fpclassify(X) __builtin_fpclassify (FP_NAN, FP_INFINITE,     \
					    FP_NORMAL, FP_SUBNORMAL, \
					    FP_ZERO, (X))

volatile TYPE inf = __builtin_inf (), nanval = __builtin_nan ("");
volatile TYPE neginf = -__builtin_inf (), negnanval = -__builtin_nan ("");
volatile TYPE zero = CST (0.0), negzero = -CST (0.0), one = CST (1.0);
volatile TYPE max = MAX, negmax = -MAX, min = MIN, negmin = -MIN;
volatile TYPE true_min = TRUE_MIN, negtrue_min = -TRUE_MIN;

/* Allow for any optimizations of comparisons involving the result of
   fpclassify by also testing case where result is stored in a
   volatile variable and so the comparison cannot be optimized.  */
#define CHECK_FPCLASSIFY(VAL, EXP)		\
  do						\
    {						\
      volatile int c;				\
      if (fpclassify (VAL) != (EXP))		\
	abort ();				\
      c = fpclassify (VAL);			\
      if (c != (EXP))				\
	abort ();				\
    }						\
  while (0)

int
main (void)
{
  CHECK_FPCLASSIFY (inf, FP_INFINITE);
  CHECK_FPCLASSIFY (neginf, FP_INFINITE);
  CHECK_FPCLASSIFY (nanval, FP_NAN);
  CHECK_FPCLASSIFY (negnanval, FP_NAN);
  CHECK_FPCLASSIFY (zero, FP_ZERO);
  CHECK_FPCLASSIFY (negzero, FP_ZERO);
  CHECK_FPCLASSIFY (one, FP_NORMAL);
  CHECK_FPCLASSIFY (max, FP_NORMAL);
  CHECK_FPCLASSIFY (negmax, FP_NORMAL);
  CHECK_FPCLASSIFY (min, FP_NORMAL);
  CHECK_FPCLASSIFY (negmin, FP_NORMAL);
  CHECK_FPCLASSIFY (true_min, FP_SUBNORMAL);
  CHECK_FPCLASSIFY (negtrue_min, FP_SUBNORMAL);
  exit (0);
}
