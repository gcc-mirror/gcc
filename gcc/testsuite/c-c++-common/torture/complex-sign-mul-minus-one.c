/* Test complex arithmetic with signed zeros.  Pure complex
   multiplication with -1.0 + 0.0i.  */
/* { dg-do run } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-skip-if "double support is incomplete" { "avr-*-*" } } */

#include "complex-sign.h"

#define CHECK_MUL_INT(TYPE, COPY, ZERO, ZEROI, ONE, S1, S2, SR, SI)	\
  do {									\
    _Complex TYPE a1, b1, c1;						\
    volatile _Complex TYPE a2, b2, c2;					\
    a1 = ENCODE(ZERO, ZEROI, S1, S2);					\
    CHECK_RES (a1, COPY, S1, S2);					\
    b1 = -ONE + ZEROI;							\
    c1 = a1 * b1;							\
    CHECK_RES (c1, COPY, SR, SI);					\
    c1 = a1 * (-ONE + ZEROI);						\
    CHECK_RES (c1, COPY, SR, SI);					\
    a2 = ENCODE(ZERO, ZEROI, S1, S2);					\
    CHECK_RES (a2, COPY, S1, S2);					\
    b2 = -ONE + ZEROI;							\
    c2 = a2 * b2;							\
    CHECK_RES (c2, COPY, SR, SI);					\
    c2 = a2 * (-ONE + ZEROI);						\
    CHECK_RES (c2, COPY, SR, SI);					\
  } while (0)

#define CHECK_MUL(TYPE, COPY, ZERO, ZEROI, ONE)			\
  do {								\
    CHECK_MUL_INT (TYPE, COPY, ZERO, ZEROI, ONE, +, +, -, +);	\
    CHECK_MUL_INT (TYPE, COPY, ZERO, ZEROI, ONE, +, -, +, +);	\
    CHECK_MUL_INT (TYPE, COPY, ZERO, ZEROI, ONE, -, +, +, -);	\
    CHECK_MUL_INT (TYPE, COPY, ZERO, ZEROI, ONE, -, -, +, +);	\
  } while (0)

void
check_mul_float (void)
{
  CHECK_MUL (float, __builtin_copysignf, 0.0f, 0.0if, 1.0f);
}

void
check_mul_double (void)
{
  CHECK_MUL (double, __builtin_copysign, 0.0, 0.0i, 1.0);
}

void
check_mul_long_double (void)
{
  CHECK_MUL (long double, __builtin_copysignl, 0.0l, 0.0il, 1.0l);
}

int
main (void)
{
  check_mul_float ();
  check_mul_double ();
  check_mul_long_double ();
  exit (0);
}
