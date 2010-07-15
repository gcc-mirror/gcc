/* Test complex arithmetic with signed zeros.  Pure complex
   addition.  */
/* { dg-do run } */
/* { dg-options "-std=gnu99" { target c } } */

#include "complex-sign.h"

#define CHECK_ADD(TYPE, COPY, ZERO, ZEROI)			\
  do {								\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, +, +, +, +, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, +, +, +, -, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, +, +, -, +, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, +, +, -, -, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, +, -, +, +, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, +, -, +, -, +, -);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, +, -, -, +, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, +, -, -, -, +, -);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, -, +, +, +, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, -, +, +, -, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, -, +, -, +, -, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, -, +, -, -, -, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, -, -, +, +, +, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, -, -, +, -, +, -);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, -, -, -, +, -, +);	\
    CHECK_ARITH (TYPE, COPY, ZERO, ZEROI, +, -, -, -, -, -, -);	\
  } while (0)

void
check_add_float (void)
{
#ifndef __SPU__
  CHECK_ADD (float, __builtin_copysignf, 0.0f, 0.0if);
#endif
}

void
check_add_double (void)
{
  CHECK_ADD (double, __builtin_copysign, 0.0, 0.0i);
}

void
check_add_long_double (void)
{
  CHECK_ADD (long double, __builtin_copysignl, 0.0l, 0.0il);
}

int
main (void)
{
  check_add_float ();
  check_add_double ();
  check_add_long_double ();
  exit (0);
}
