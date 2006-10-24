/* Copyright (C) 2006  Free Software Foundation.

   Verify that built-in math function constant folding of constant
   arguments is correctly performed by the compiler.

   Origin: Kaveh R. Ghazi,  October 23, 2006.  */

/* { dg-do link } */

/* All references to link_error should go away at compile-time.  */
extern void link_error(int);

/* Test that FUNC(ARG) == (RES).  */
#define TESTIT(FUNC,ARG,RES) do { \
  if (__builtin_##FUNC##f(ARG##F) != RES##F) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG) != RES) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG##L) != RES##L) \
    link_error(__LINE__); \
  } while (0);

/* Test that (LOW) < FUNC(ARG) < (HI).  */
#define TESTIT2(FUNC,ARG,LOW,HI) do { \
  if (__builtin_##FUNC##f(ARG) <= (LOW) || __builtin_##FUNC##f(ARG) >= (HI)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG) <= (LOW) || __builtin_##FUNC(ARG) >= (HI)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG) <= (LOW) || __builtin_##FUNC##l(ARG) >= (HI)) \
    link_error(__LINE__); \
  } while (0);

int main (void)
{
  TESTIT2 (asin, -1.0, -3.15/2.0, -3.14/2.0); /* asin(-1) == -pi/2 */
  TESTIT (asin, 0.0, 0.0); /* asin(0) == 0 */
  TESTIT2 (asin, 1.0, 3.14/2.0, 3.15/2.0); /* asin(1) == pi/2 */

  TESTIT2 (acos, -1.0, 3.14, 3.15); /* acos(-1) == pi */
  TESTIT2 (acos, 0.0, 3.14/2.0, 3.15/2.0); /* acos(0) == pi/2 */
  TESTIT (acos, 1.0, 0.0); /* acos(1) == 0 */

  TESTIT2 (atan, -1.0, -3.15/4.0, -3.14/4.0); /* atan(-1) == -pi/4 */
  TESTIT (atan, 0.0, 0.0); /* atan(0) == 0 */
  TESTIT2 (atan, 1.0, 3.14/4.0, 3.15/4.0); /* atan(1) == pi/4 */

  TESTIT2 (asinh, -1.0, -0.89, -0.88); /* asinh(-1) == -0.881... */
  TESTIT (asinh, 0.0, 0.0); /* asinh(0) == 0 */
  TESTIT2 (asinh, 1.0, 0.88, 0.89); /* asinh(1) == 0.881... */

  TESTIT (acosh, 1.0, 0.0); /* acosh(1) == 0. */
  TESTIT2 (acosh, 2.0, 1.31, 1.32); /* acosh(2) == 1.316... */

  TESTIT2 (atanh, -0.5, -0.55, -0.54); /* atanh(-0.5) == -0.549... */
  TESTIT (atanh, 0.0, 0.0); /* atanh(0) == 0 */
  TESTIT2 (atanh, 0.5, 0.54, 0.55); /* atanh(0.5) == 0.549... */

  TESTIT2 (sin, -1.0, -0.85, -0.84); /* sin(-1) == -0.841... */
  TESTIT (sin, 0.0, 0.0); /* sin(0) == 0 */
  TESTIT2 (sin, 1.0, 0.84, 0.85); /* sin(1) == 0.841... */

  TESTIT2 (cos, -1.0, 0.54, 0.55); /* cos(-1) == 0.5403... */
  TESTIT (cos, 0.0, 1.0); /* cos(0) == 1 */
  TESTIT2 (cos, 1.0, 0.54, 0.55); /* cos(1) == 0.5403... */

  TESTIT2 (tan, -1.0, -1.56, 1.55); /* tan(-1) == -1.557... */
  TESTIT (tan, 0.0, 0.0); /* tan(0) == 0 */
  TESTIT2 (tan, 1.0, 1.55, 1.56); /* tan(1) == 1.557... */

  TESTIT2 (sinh, -1.0, -1.18, -1.17); /* sinh(-1) == -1.175... */
  TESTIT (sinh, 0.0, 0.0); /* sinh(0) == 0 */
  TESTIT2 (sinh, 1.0, 1.17, 1.18); /* sinh(1) == 1.175... */

  TESTIT2 (cosh, -1.0, 1.54, 1.55); /* cosh(-1) == 1.543... */
  TESTIT (cosh, 0.0, 1.0); /* cosh(0) == 1 */
  TESTIT2 (cosh, 1.0, 1.54, 1.55); /* cosh(1) == 1.543... */

  TESTIT2 (tanh, -1.0, -0.77, -0.76); /* tanh(-1) == -0.761... */
  TESTIT (tanh, 0.0, 0.0); /* tanh(0) == 0 */
  TESTIT2 (tanh, 1.0, 0.76, 0.77); /* tanh(1) == 0.761... */

  TESTIT2 (exp, -1.0, 0.36, 0.37); /* exp(-1) == 1/e */
  TESTIT (exp, 0.0, 1.0); /* exp(0) == 1 */
  TESTIT2 (exp, 1.0, 2.71, 2.72); /* exp(1) == e */

  TESTIT (exp2, -1.0, 0.5); /* exp2(-1) == 1/2 */
  TESTIT (exp2, 0.0, 1.0); /* exp2(0) == 1 */
  TESTIT (exp2, 1.0, 2.0); /* exp2(1) == 2 */

  TESTIT (exp10, -1.0, 0.1); /* exp10(-1) == 1/10 */
  TESTIT (exp10, 0.0, 1.0); /* exp10(0) == 1 */
  TESTIT (exp10, 1.0, 10.0); /* exp10(1) == 10 */

  TESTIT (pow10, -1.0, 0.1); /* pow10(-1) == 1/10 */
  TESTIT (pow10, 0.0, 1.0); /* pow10(0) == 1 */
  TESTIT (pow10, 1.0, 10.0); /* pow10(1) == 10 */

  return 0;
}
