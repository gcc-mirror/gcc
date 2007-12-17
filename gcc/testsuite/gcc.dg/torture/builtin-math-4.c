/* Copyright (C) 2007  Free Software Foundation.

   Verify that built-in math function constant folding of constant
   arguments is correctly performed by the compiler.  This testcase is
   for functionality that was available as of mpfr-2.3.0.

   Origin: Kaveh R. Ghazi,  April 23, 2007.  */

/* { dg-do link } */

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

/* Test that remquo(ARG0, ARG1, &ARG_Q) == RES and ARG_Q == RES_Q.
   Also test remainder/drem (ARG0,ARG1) == RES.  */
#define TESTIT2_REMQUO(ARG0,ARG1,ARG_Q,RES,RES_Q) do { \
  ARG_Q = 12345; \
  if (__builtin_remquof(ARG0##F, ARG1##F, &ARG_Q) != RES##F \
      || CKSGN_F(__builtin_remquof(ARG0##F, ARG1##F, &ARG_Q),RES##F) \
      || ARG_Q != RES_Q \
      || __builtin_remainderf(ARG0##F, ARG1##F) != RES##F \
      || CKSGN_F(__builtin_remainderf(ARG0##F, ARG1##F),RES##F) \
      || __builtin_dremf(ARG0##F, ARG1##F) != RES##F \
      || CKSGN_F(__builtin_dremf(ARG0##F, ARG1##F),RES##F)) \
    link_error(__LINE__); \
  ARG_Q = 12345; \
  if (__builtin_remquo(ARG0, ARG1, &ARG_Q) != RES \
      || CKSGN(__builtin_remquo(ARG0, ARG1, &ARG_Q),RES) \
      || ARG_Q != RES_Q \
      || __builtin_remainder(ARG0, ARG1) != RES \
      || CKSGN(__builtin_remainder(ARG0, ARG1),RES) \
      || __builtin_drem(ARG0, ARG1) != RES \
      || CKSGN(__builtin_drem(ARG0, ARG1),RES)) \
    link_error(__LINE__); \
  ARG_Q = 12345; \
  if (__builtin_remquol(ARG0##L, ARG1##L, &ARG_Q) != RES##L \
      || CKSGN_L(__builtin_remquol(ARG0##L, ARG1##L, &ARG_Q),RES##L) \
      || ARG_Q != RES_Q \
      || __builtin_remainderl(ARG0##L, ARG1##L) != RES##L \
      || CKSGN_L(__builtin_remainderl(ARG0##L, ARG1##L),RES##L) \
      || __builtin_dreml(ARG0##L, ARG1##L) != RES##L \
      || CKSGN_L(__builtin_dreml(ARG0##L, ARG1##L),RES##L)) \
    link_error(__LINE__); \
  } while (0)

/* Test that FUNC(ARG,&SG) == (RES) && SG == RES_SG.  */
#define TESTIT_LGAMMA_REENT(FUNC,ARG,RES,RES_SG) do { \
  int sg; \
  sg = 123; \
  if (__builtin_##FUNC##f_r(ARG##F,&sg) != RES##F \
      || sg != RES_SG \
      || CKSGN_F(__builtin_##FUNC##f_r(ARG##F,&sg),RES##F)) \
    link_error(__LINE__); \
  sg = 123; \
  if (__builtin_##FUNC##_r(ARG,&sg) != RES \
      || sg != RES_SG \
      || CKSGN(__builtin_##FUNC##_r(ARG,&sg),RES)) \
    link_error(__LINE__); \
  sg = 123; \
  if (__builtin_##FUNC##l_r(ARG##L,&sg) != RES##L \
      || sg != RES_SG \
      || CKSGN_L(__builtin_##FUNC##l_r(ARG##L,&sg),RES##L)) \
    link_error(__LINE__); \
  } while (0)

/* Range test, check that (LOW) < FUNC(ARG,&SG) < (HI), and also test
   that SG == RES_SG.  */
#define TESTIT_LGAMMA_REENT_R(FUNC,ARG,LOW,HI,RES_SG) do { \
  int sg; \
  sg = 123; \
  if (__builtin_##FUNC##f_r(ARG,&sg) <= (LOW) || __builtin_##FUNC##f_r(ARG,&sg) >= (HI) \
      || sg != RES_SG) \
    link_error(__LINE__); \
  sg = 123; \
  if (__builtin_##FUNC##_r(ARG,&sg) <= (LOW) || __builtin_##FUNC##_r(ARG,&sg) >= (HI) \
      || sg != RES_SG) \
    link_error(__LINE__); \
  sg = 123; \
  if (__builtin_##FUNC##l_r(ARG,&sg) <= (LOW) || __builtin_##FUNC##l_r(ARG,&sg) >= (HI) \
      || sg != RES_SG) \
    link_error(__LINE__); \
  } while (0)

int main (void)
{
#ifdef __OPTIMIZE__
  int q;
#endif

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

#ifdef __OPTIMIZE__
  /* These tests rely on propagating the variable q, which happens
     only when optimization is turned on.  This macro also tests
     remainder/drem.  */
  TESTIT2_REMQUO (0.0, 1.0, q, 0.0, 0); /* remquo(0,1,&q)==0, q==0 */
  TESTIT2_REMQUO (1.0, 1.0, q, 0.0, 1); /* remquo(1,1,&q)==0, q==1 */
  TESTIT2_REMQUO (2.0, 1.0, q, 0.0, 2); /* remquo(2,1,&q)==0, q==2 */
  TESTIT2_REMQUO (-0.0, 1.0, q, -0.0, 0); /* remquo(-0,1,&q)==-0, q==0 */
  TESTIT2_REMQUO (-1.0, 1.0, q, -0.0, -1); /* remquo(-1,1,&q)==-0, q==-1 */
  TESTIT2_REMQUO (-2.0, 1.0, q, -0.0, -2); /* remquo(-2,1,&q)==-0, q==-2 */

  TESTIT2_REMQUO (0.0, -1.0, q, 0.0, 0); /* remquo(0,-1,&q)==0, q==0 */
  TESTIT2_REMQUO (1.0, -1.0, q, 0.0, -1); /* remquo(1,-1,&q)==0, q==-1 */
  TESTIT2_REMQUO (2.0, -1.0, q, 0.0, -2); /* remquo(2,-1,&q)==0, q==-2 */
  TESTIT2_REMQUO (-0.0, -1.0, q, -0.0, 0); /* remquo(-0,-1,&q)==-0, q==0 */
  TESTIT2_REMQUO (-1.0, -1.0, q, -0.0, 1); /* remquo(-1,-1,&q)==-0, q==1 */
  TESTIT2_REMQUO (-2.0, -1.0, q, -0.0, 2); /* remquo(-2,-1,&q)==-0, q==2 */

  TESTIT2_REMQUO (1.0, 2.0, q, 1.0, 0); /* remquo(1,2,&q)==1, q==0 */
  TESTIT2_REMQUO (3.0, 2.0, q, -1.0, 2); /* remquo(3,2,&q)==-1, q==2 */
  TESTIT2_REMQUO (5.0, 2.0, q, 1.0, 2); /* remquo(5,2,&q)==1, q==2 */
  TESTIT2_REMQUO (-1.0, 2.0, q, -1.0, 0); /* remquo(-1,2,&q)==-1, q==0 */
  TESTIT2_REMQUO (-3.0, 2.0, q, 1.0, -2); /* remquo(-3,2,&q)==1, q==-2 */
  TESTIT2_REMQUO (-5.0, 2.0, q, -1.0, -2); /* remquo(-5,2,&q)==-1, q==-2 */

  TESTIT2_REMQUO (1.0, -2.0, q, 1.0, 0); /* remquo(1,-2,&q)==1, q==0 */
  TESTIT2_REMQUO (3.0, -2.0, q, -1.0, -2); /* remquo(3,-2,&q)==-1, q==-2 */
  TESTIT2_REMQUO (5.0, -2.0, q, 1.0, -2); /* remquo(5,-2,&q)==1, q==-2 */
  TESTIT2_REMQUO (-1.0, -2.0, q, -1.0, 0); /* remquo(-1,-2,&q)==-1, q==0 */
  TESTIT2_REMQUO (-3.0, -2.0, q, 1.0, 2); /* remquo(-3,-2,&q)==1, q==2 */
  TESTIT2_REMQUO (-5.0, -2.0, q, -1.0, 2); /* remquo(-5,-2,&q)==-1, q==2 */

  /* Test that the maximum possible value can be generated into the
     int quotient, and check for wrap around (modulo) when that value
     is exceeded.  We can only check for this when the mantissa has
     enough bits to hold an INT_MAX value with complete precision.  */

#define MAXIT(FUNC,X,R) do { \
  q = 12345; \
  if (__builtin_##FUNC((X), 1, &q) != 0 || q != (R)) \
    link_error (__LINE__); \
} while (0)
  
  if (sizeof(int)*__CHAR_BIT__ <= __FLT_MANT_DIG__)
  {
    MAXIT(remquof, __INT_MAX__-1.0F, __INT_MAX__-1);
    MAXIT(remquof, __INT_MAX__+0.0F, __INT_MAX__);
    MAXIT(remquof, __INT_MAX__+1.0F, 0);
    MAXIT(remquof, __INT_MAX__+2.0F, 1);

    MAXIT(remquof, -(__INT_MAX__-1.0F), -(__INT_MAX__-1));
    MAXIT(remquof, -(__INT_MAX__+0.0F), -__INT_MAX__);
    MAXIT(remquof, -(__INT_MAX__+1.0F), 0);
    MAXIT(remquof, -(__INT_MAX__+2.0F), -1);
  }

  if (sizeof(int)*__CHAR_BIT__ <= __DBL_MANT_DIG__)
  {
    MAXIT(remquo, __INT_MAX__-1.0, __INT_MAX__-1);
    MAXIT(remquo, __INT_MAX__+0.0, __INT_MAX__);
    MAXIT(remquo, __INT_MAX__+1.0, 0);
    MAXIT(remquo, __INT_MAX__+2.0, 1);

    MAXIT(remquo, -(__INT_MAX__-1.0), -(__INT_MAX__-1));
    MAXIT(remquo, -(__INT_MAX__+0.0), -__INT_MAX__);
    MAXIT(remquo, -(__INT_MAX__+1.0), 0);
    MAXIT(remquo, -(__INT_MAX__+2.0), -1);
  }

  if (sizeof(int)*__CHAR_BIT__ <= __LDBL_MANT_DIG__)
  {
    MAXIT(remquo, __INT_MAX__-1.0L, __INT_MAX__-1);
    MAXIT(remquo, __INT_MAX__+0.0L, __INT_MAX__);
    MAXIT(remquo, __INT_MAX__+1.0L, 0);
    MAXIT(remquo, __INT_MAX__+2.0L, 1);

    MAXIT(remquol, -(__INT_MAX__-1.0L), -(__INT_MAX__-1));
    MAXIT(remquol, -(__INT_MAX__+0.0L), -__INT_MAX__);
    MAXIT(remquol, -(__INT_MAX__+1.0L), 0);
    MAXIT(remquol, -(__INT_MAX__+2.0L), -1);
  }

  /* These tests rely on propagating the variable sg which contains
     signgam.  This happens only when optimization is turned on.  */
  TESTIT_LGAMMA_REENT_R (lgamma, -2.5, -0.06, -0.05, -1); /* lgamma_r(-2.5) == -0.056... */
  TESTIT_LGAMMA_REENT_R (lgamma, -1.5, 0.86, 0.87, 1); /* lgamma_r(-1.5) == 0.860... */
  TESTIT_LGAMMA_REENT_R (lgamma, -0.5, 1.26, 1.27, -1); /* lgamma_r(-0.5) == 1.265... */
  TESTIT_LGAMMA_REENT_R (lgamma, 0.5, 0.57, 0.58, 1); /* lgamma_r(0.5) == 0.572... */
  TESTIT_LGAMMA_REENT (lgamma, 1.0, 0.0, 1); /* lgamma_r(1) == 0 */
  TESTIT_LGAMMA_REENT_R (lgamma, 1.5, -0.13, -0.12, 1); /* lgamma_r(1.5) == -0.120... */
  TESTIT_LGAMMA_REENT (lgamma, 2.0, 0.0, 1); /* lgamma_r(2) == 0 */
  TESTIT_LGAMMA_REENT_R (lgamma, 2.5, 0.28, 0.29, 1); /* lgamma_r(2.5) == 0.284... */

  TESTIT_LGAMMA_REENT_R (gamma, -2.5, -0.06, -0.05, -1); /* gamma_r(-2.5) == -0.056... */
  TESTIT_LGAMMA_REENT_R (gamma, -1.5, 0.86, 0.87, 1); /* gamma_r(-1.5) == 0.860... */
  TESTIT_LGAMMA_REENT_R (gamma, -0.5, 1.26, 1.27, -1); /* gamma_r(-0.5) == 1.265... */
  TESTIT_LGAMMA_REENT_R (gamma, 0.5, 0.57, 0.58, 1); /* gamma_r(0.5) == 0.572... */
  TESTIT_LGAMMA_REENT (gamma, 1.0, 0.0, 1); /* gamma_r(1) == 0 */
  TESTIT_LGAMMA_REENT_R (gamma, 1.5, -0.13, -0.12, 1); /* gamma_r(1.5) == -0.120... */
  TESTIT_LGAMMA_REENT (gamma, 2.0, 0.0, 1); /* gamma_r(2) == 0 */
  TESTIT_LGAMMA_REENT_R (gamma, 2.5, 0.28, 0.29, 1); /* gamma_r(2.5) == 0.284... */
#endif
  
  return 0;
}
