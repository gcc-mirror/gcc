/* Test vadd works correctly.  */
/* { dg-do run } */
/* { dg-options "--save-temps" } */

#include <arm_neon.h>

#define FLT_EPSILON __FLT_EPSILON__
#define DBL_EPSILON __DBL_EPSILON__

#define TESTA0 0.33333
#define TESTA1 -1.7777
#define TESTA2 0
#define TESTA3 1.23456
/* 2^54, double has 53 significand bits
   according to Double-precision floating-point format.  */
#define TESTA4 18014398509481984
#define TESTA5 (1.0 / TESTA4)

#define TESTB0 0.66667
#define TESTB1 2
#define TESTB2 0
#define TESTB3 -2
#define TESTB4 1.0
#define TESTB5 2.0

#define ANSW0 1
#define ANSW1 0.2223
#define ANSW2 0
#define ANSW3 -0.76544
#define ANSW4 TESTA4
#define ANSW5 2.0

extern void abort (void);

#define EPSILON __DBL_EPSILON__
#define ABS(a) __builtin_fabs (a)
#define ISNAN(a) __builtin_isnan (a)
#define FP_equals(a, b, epsilon)			\
  (							\
   ((a) == (b))						\
    || (ISNAN (a) && ISNAN (b))				\
    || (ABS (a - b) < epsilon)				\
   )

int
test_vadd_f64 ()
{
  float64x1_t a;
  float64x1_t b;
  float64x1_t c;

  a = TESTA0;
  b = TESTB0;
  c = ANSW0;

  a = vadd_f64 (a, b);
  if (!FP_equals (a, c, EPSILON))
    return 1;

  a = TESTA1;
  b = TESTB1;
  c = ANSW1;

  a = vadd_f64 (a, b);
  if (!FP_equals (a, c, EPSILON))
    return 1;

  a = TESTA2;
  b = TESTB2;
  c = ANSW2;

  a = vadd_f64 (a, b);
  if (!FP_equals (a, c, EPSILON))
    return 1;

  a = TESTA3;
  b = TESTB3;
  c = ANSW3;

  a = vadd_f64 (a, b);
  if (!FP_equals (a, c, EPSILON))
    return 1;

  a = TESTA4;
  b = TESTB4;
  c = ANSW4;

  a = vadd_f64 (a, b);
  if (!FP_equals (a, c, EPSILON))
    return 1;

  a = TESTA5;
  b = TESTB5;
  c = ANSW5;

  a = vadd_f64 (a, b);
  if (!FP_equals (a, c, EPSILON))
    return 1;

  return 0;
}

/* { dg-final { scan-assembler-times "fadd\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 6 } } */

int
main (int argc, char **argv)
{
  if (test_vadd_f64 ())
    abort ();

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
