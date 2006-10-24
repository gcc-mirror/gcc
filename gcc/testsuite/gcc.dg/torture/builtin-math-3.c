/* Copyright (C) 2006  Free Software Foundation.

   Verify that built-in math function constant folding of constant
   arguments is correctly performed by the compiler.

   Origin: Kaveh R. Ghazi, October 23, 2006.  */

/* { dg-do link } */

/* All references to link_error should go away at compile-time.  */
extern void link_error(int);

/* Test that FUNC(ARG) == (RES).  */
#define TESTIT(FUNC,ARG,RES) do { \
  if (__builtin_##FUNC##f(ARG) != RES) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG) != RES) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG) != RES) \
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
  TESTIT2 (asin, -1, -3.15/2, -3.14/2); /* asin(-1) == -pi/2 */
  TESTIT (asin, 0, 0); /* asin(0) == 0 */
  TESTIT2 (asin, 1, 3.14/2, 3.15/2); /* asin(1) == pi/2 */

  TESTIT2 (acos, -1, 3.14, 3.15); /* acos(-1) == pi */
  TESTIT2 (acos, 0, 3.14/2, 3.15/2); /* acos(0) == pi/2 */
  TESTIT (acos, 1, 0); /* acos(1) == 0 */

  TESTIT2 (atan, -1, -3.15/4, -3.14/4); /* atan(-1) == -pi/4 */
  TESTIT (atan, 0, 0); /* atan(0) == 0 */
  TESTIT2 (atan, 1, 3.14/4, 3.15/4); /* atan(1) == pi/4 */

  TESTIT2 (asinh, -1, -0.89, -0.88); /* asinh(-1) == -0.881... */
  TESTIT (asinh, 0, 0); /* asinh(0) == 0 */
  TESTIT2 (asinh, 1, 0.88, 0.89); /* asinh(1) == 0.881... */

  TESTIT (acosh, 1, 0); /* acosh(1) == 0. */
  TESTIT2 (acosh, 2, 1.31, 1.32); /* acosh(2) == 1.316... */

  TESTIT2 (atanh, -0.5, -0.55, -0.54); /* atanh(-0.5) == -0.549... */
  TESTIT (atanh, 0, 0); /* atanh(0) == 0 */
  TESTIT2 (atanh, 0.5, 0.54, 0.55); /* atanh(0.5) == 0.549... */

  TESTIT2 (sinh, -1, -1.18, -1.17); /* sinh(-1) == -1.175... */
  TESTIT (sinh, 0, 0); /* sinh(0) == 0 */
  TESTIT2 (sinh, 1, 1.17, 1.18); /* sinh(1) == 1.175... */

  TESTIT2 (cosh, -1, 1.54, 1.55); /* cosh(-1) == 1.543... */
  TESTIT (cosh, 0, 1); /* cosh(0) == 1 */
  TESTIT2 (cosh, 1, 1.54, 1.55); /* cosh(1) == 1.543... */

  TESTIT2 (tanh, -1, -0.77, -0.76); /* tanh(-1) == -0.761... */
  TESTIT (tanh, 0, 0); /* tanh(0) == 0 */
  TESTIT2 (tanh, 1, 0.76, 0.77); /* tanh(1) == 0.761... */

  return 0;
}
