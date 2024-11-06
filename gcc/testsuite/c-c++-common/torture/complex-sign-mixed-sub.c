/* Test complex arithmetic with signed zeros.  Mixed real/complex
   subtraction.  */
/* { dg-do run } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-skip-if "double support is incomplete" { "avr-*-*" } } */

#include "complex-sign.h"

#define CHECK_SUB(TYPE, COPY, ZERO, ZEROI)				\
  do {									\
    CHECK_ARITH_RC (TYPE, COPY, ZERO, ZEROI, -, +, +, +, +, -);		\
    CHECK_ARITH_RC (TYPE, COPY, ZERO, ZEROI, -, +, +, -, +, +);		\
    CHECK_ARITH_RC (TYPE, COPY, ZERO, ZEROI, -, +, -, +, +, -);		\
    CHECK_ARITH_RC (TYPE, COPY, ZERO, ZEROI, -, +, -, -, +, +);		\
    CHECK_ARITH_RC (TYPE, COPY, ZERO, ZEROI, -, -, +, +, -, -);		\
    CHECK_ARITH_RC (TYPE, COPY, ZERO, ZEROI, -, -, +, -, -, +);		\
    CHECK_ARITH_RC (TYPE, COPY, ZERO, ZEROI, -, -, -, +, +, -);		\
    CHECK_ARITH_RC (TYPE, COPY, ZERO, ZEROI, -, -, -, -, +, +);		\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, -, +, +, +, ZERO, +, +);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, -, +, +, -, ZERO, +, +);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, -, +, -, +, ZERO, +, -);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, -, +, -, -, ZERO, +, -);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, -, -, +, +, ZERO, -, +);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, -, -, +, -, ZERO, +, +);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, -, -, -, +, ZERO, -, -);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, -, -, -, -, ZERO, +, -);	\
  } while (0)

void
check_sub_float (void)
{
  CHECK_SUB (float, __builtin_copysignf, 0.0f, 0.0if);
}

void
check_sub_double (void)
{
  CHECK_SUB (double, __builtin_copysign, 0.0, 0.0i);
}

void
check_sub_long_double (void)
{
  CHECK_SUB (long double, __builtin_copysignl, 0.0l, 0.0il);
}

int
main (void)
{
  check_sub_float ();
  check_sub_double ();
  check_sub_long_double ();
  exit (0);
}
