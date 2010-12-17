/* Test complex arithmetic with signed zeros.  Pure complex
   subtraction.  */
/* { dg-do run } */
/* { dg-options "-std=gnu99" { target c } } */

#include "complex-sign.h"

#define CHECK_SUB(TYPE, COPY, ZERO, ZEROI)			\
  do {								\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, +, +, +, +, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, +, +, +, -, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, +, +, -, +, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, +, +, -, -, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, +, -, +, +, +, -);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, +, -, +, -, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, +, -, -, +, +, -);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, +, -, -, -, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, -, +, +, +, -, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, -, +, +, -, -, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, -, +, -, +, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, -, +, -, -, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, -, -, +, +, -, -);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, -, -, +, -, -, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, -, -, -, +, +, -);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, -, -, -, -, -, +, +);	\
  } while (0)

void
check_sub_float (void)
{
#ifndef __SPU__
  CHECK_SUB (float, __builtin_copysignf, 0.0f, 0.0if);
#endif
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
