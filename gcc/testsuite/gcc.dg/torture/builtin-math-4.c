/* Copyright (C) 2007  Free Software Foundation.

   Verify that built-in math function constant folding of constant
   arguments is correctly performed by the compiler.  This testcase is
   for functionality that was available as of mpfr-2.3.0.

   Origin: Kaveh R. Ghazi,  April 23, 2007.  */

/* { dg-do link } */
/* Expect failures at least until mpfr-2.3.0 is released. */
/* { dg-xfail-if "mpfr-2.3.0" { *-*-* } } */

/* All references to link_error should go away at compile-time.  */
extern void link_error(int);

/* Return TRUE if the sign of X != sign of Y.  This is important when
   comparing signed zeros.  */
#define CKSGN_F(X,Y) \
  (__builtin_copysignf(1.0F,(X)) != __builtin_copysignf(1.0F,(Y)))
#define CKSGN(X,Y) \
  (__builtin_copysign(1.0,(X)) != __builtin_copysign(1.0,(Y)))
#define CKSGN_L(X,Y) \
  (__builtin_copysignl(1.0L,(X)) != __builtin_copysignl(1.0L,(Y)))

/* Test that FUNC(ARG) == (RES).  */
#define TESTIT(FUNC,ARG,RES) do { \
  if (__builtin_##FUNC##f(ARG##F) != RES##F \
      || CKSGN_F(__builtin_##FUNC##f(ARG##F),RES##F)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG) != RES \
      || CKSGN(__builtin_##FUNC(ARG),RES)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG##L) != RES##L \
      || CKSGN_L(__builtin_##FUNC##l(ARG##L),RES##L)) \
    link_error(__LINE__); \
  } while (0)

/* Range test, check that (LOW) < FUNC(ARG) < (HI).  */
#define TESTIT_R(FUNC,ARG,LOW,HI) do { \
  if (__builtin_##FUNC##f(ARG) <= (LOW) || __builtin_##FUNC##f(ARG) >= (HI)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG) <= (LOW) || __builtin_##FUNC(ARG) >= (HI)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG) <= (LOW) || __builtin_##FUNC##l(ARG) >= (HI)) \
    link_error(__LINE__); \
  } while (0)

/* Test that FUNC(ARG1, ARG2) == (RES).  */
#define TESTIT2(FUNC,ARG1,ARG2,RES) do { \
  if (__builtin_##FUNC##f(ARG1, ARG2##F) != RES##F \
      || CKSGN_F(__builtin_##FUNC##f(ARG1,ARG2##F),RES##F)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG1, ARG2) != RES \
      || CKSGN(__builtin_##FUNC(ARG1,ARG2),RES)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG1, ARG2##L) != RES##L \
      || CKSGN_L(__builtin_##FUNC##l(ARG1,ARG2##L),RES##L)) \
    link_error(__LINE__); \
  } while (0)

/* Range test, check that (LOW) < FUNC(ARG1,ARG2) < (HI).  */
#define TESTIT2_R(FUNC,ARG1,ARG2,LOW,HI) do { \
  if (__builtin_##FUNC##f(ARG1, ARG2##F) <= (LOW) \
      || __builtin_##FUNC##f(ARG1, ARG2##F) >= (HI)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG1, ARG2) <= (LOW) \
      || __builtin_##FUNC(ARG1, ARG2) >= (HI)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG1, ARG2##L) <= (LOW) \
      || __builtin_##FUNC##l(ARG1, ARG2##L) >= (HI)) \
    link_error(__LINE__); \
  } while (0)

int main (void)
{
  TESTIT (j0, 0.0, 1.0); /* j0(0) == 1 */
  TESTIT (j0, -0.0, 1.0); /* j0(-0) == 1 */
  TESTIT_R (j0, 1.0, 0.765, 0.766); /* j0(1) == 0.7651... */
  TESTIT_R (j0, -1.0, 0.765, 0.766); /* j0(-1) == 0.7651... */

  TESTIT (j1, 0.0, 0.0); /* j1(0) == 0 */
  TESTIT (j1, -0.0, -0.0); /* j1(-0) == -0 */
  TESTIT_R (j1, 1.0, 0.44, 0.45); /* j1(1) == 0.440... */
  TESTIT_R (j1, -1.0, -0.45, -0.44); /* j1(-1) == -0.440... */

  TESTIT2 (jn, 5, 0.0, 0.0); /* jn(5,0) == 0 */
  TESTIT2 (jn, 5, -0.0, -0.0); /* jn(5,-0) == -0 */
  TESTIT2 (jn, 6, 0.0, 0.0); /* jn(6,0) == 0 */
  TESTIT2 (jn, 6, -0.0, 0.0); /* jn(6,-0) == 0 */

  TESTIT2 (jn, -5, 0.0, -0.0); /* jn(-5,0) == -0 */
  TESTIT2 (jn, -5, -0.0, 0.0); /* jn(-5,-0) == 0 */
  TESTIT2 (jn, -6, 0.0, 0.0); /* jn(-6,0) == 0 */
  TESTIT2 (jn, -6, -0.0, 0.0); /* jn(-6,-0) == 0 */

  TESTIT2_R (jn, 2, 1.0, 0.11, 0.12); /* jn(2,1) == 0.114... */
  TESTIT2_R (jn, 2, -1.0, 0.11, 0.12); /* jn(2,-1) == 0.114... */
  TESTIT2_R (jn, 3, 5.0, 0.36, 0.37); /* jn(3,5) == 0.364... */
  TESTIT2_R (jn, 3, -5.0, -0.37, -0.36); /* jn(3,-5) == -0.364... */

  TESTIT2_R (jn, -2, 1.0, 0.11, 0.12); /* jn(-2,1) == 0.114... */
  TESTIT2_R (jn, -2, -1.0, 0.11, 0.12); /* jn(-2,-1) == 0.114... */
  TESTIT2_R (jn, -3, 5.0, -0.37, -0.36); /* jn(-3,5) == -0.364... */
  TESTIT2_R (jn, -3, -5.0, 0.36, 0.37); /* jn(-3,-5) == 0.364... */

  TESTIT2_R (jn, 4, 3.5, 0.20, 0.21); /* jn(4,3.5) == 0.204... */
  TESTIT2_R (jn, 4, -3.5, 0.20, 0.21); /* jn(4,-3.5) == 0.204... */
  TESTIT2_R (jn, 5, 4.6, 0.20, 0.21); /* jn(5,4.6) == 0.207... */
  TESTIT2_R (jn, 5, -4.6, -0.21, -0.20); /* jn(5,-4.6) == -0.207... */

  TESTIT2_R (jn, -4, 3.5, 0.20, 0.21); /* jn(-4,3.5) == 0.204... */
  TESTIT2_R (jn, -4, -3.5, 0.20, 0.21); /* jn(-4,-3.5) == 0.204... */
  TESTIT2_R (jn, -5, 4.6, -0.21, -0.20); /* jn(-5,4.6) == -0.207... */
  TESTIT2_R (jn, -5, -4.6, 0.20, 0.21); /* jn(-5,-4.6) == 0.207... */

  TESTIT_R (y0, 5.0, -0.31, -0.30); /* y0(5) == -0.308... */
  TESTIT_R (y0, 0.1, -1.54, -1.53); /* y0(0.1) == -1.534... */

  TESTIT_R (y1, 5.0, 0.14, 0.15); /* y1(5) == 0.147... */
  TESTIT_R (y1, 0.1, -6.46, -6.45); /* y1(0.1) == -6.458... */

  TESTIT2_R (yn, -1, 3.0, -0.33, -0.32); /* yn(-1,3) == -0.324... */
  TESTIT2_R (yn, -1, 0.25, 2.70, 2.71); /* yn(-1,0.25) == 2.704... */

  TESTIT2_R (yn, 2, 4.0, 0.21, 0.22); /* yn(2,4) == 0.215... */
  TESTIT2_R (yn, 2, 0.9, -1.95, -1.94); /* yn(2,0.9) == -1.945... */
  TESTIT2_R (yn, -2, 4.0, 0.21, 0.22); /* yn(-2,4) == 0.215... */
  TESTIT2_R (yn, -2, 0.9, -1.95, -1.94); /* yn(-2,0.9) == -1.945... */

  TESTIT2_R (yn, 3, 6.0, 0.32, 0.33); /* yn(3,6) == 0.328... */
  TESTIT2_R (yn, 3, 0.89, -8.03, -8.02); /* yn(3,0.89) == -8.020... */
  TESTIT2_R (yn, -3, 8.0, -0.03, -0.02); /* yn(-3,8) == -0.026... */
  TESTIT2_R (yn, -3, 0.99, 5.98, 5.99); /* yn(-3,0.99) == 5.982... */
  
  return 0;
}
