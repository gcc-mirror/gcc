/* Test complex arithmetic with signed zeros.  Mixed real/complex
   division.  */
/* { dg-do run } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-skip-if "double support is incomplete" { "avr-*-*" } } */

#include "complex-sign.h"

#define CHECK_DIV(TYPE, COPY, ZERO, ZEROI, ONE)				\
  do {									\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, /, +, +, +, ONE, +, +);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, /, +, +, -, ONE, -, -);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, /, +, -, +, ONE, +, -);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, /, +, -, -, ONE, -, +);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, /, -, +, +, ONE, -, +);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, /, -, +, -, ONE, +, -);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, /, -, -, +, ONE, -, -);	\
    CHECK_ARITH_CR (TYPE, COPY, ZERO, ZEROI, /, -, -, -, ONE, +, +);	\
  } while (0)

void
check_div_float (void)
{
  CHECK_DIV (float, __builtin_copysignf, 0.0f, 0.0if, 1.0f);
}

void
check_div_double (void)
{
  CHECK_DIV (double, __builtin_copysign, 0.0, 0.0i, 1.0);
}

void
check_div_long_double (void)
{
  CHECK_DIV (long double, __builtin_copysignl, 0.0l, 0.0il, 1.0l);
}

int
main (void)
{
  check_div_float ();
  check_div_double ();
  check_div_long_double ();
  exit (0);
}
