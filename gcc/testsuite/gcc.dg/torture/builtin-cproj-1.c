/* Copyright (C) 2010  Free Software Foundation.

   Verify that folding of built-in cproj is correctly performed by the
   compiler.

   Origin: Kaveh R. Ghazi,  April 9, 2010.  */

/* { dg-do link } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-add-options ieee } */

/* All references to link_error should go away at compile-time.  The
   argument is the __LINE__ number.  It appears in the tree dump file
   and aids in debugging should any of the tests fail.  */
extern void link_error(int);

#define CPROJ(X) __builtin_cproj(X)
#define CPROJF(X) __builtin_cprojf(X)
#define CPROJL(X) __builtin_cprojl(X)
#ifndef __SPU__
#define INF __builtin_inff()
#else
#define INF __builtin_inf()
#endif
#define I 1i
#define CPSGN(X,Y) __builtin_copysignf((X),(Y))
#define CIMAG(X) __builtin_cimagf(X)
#define CREAL(X) __builtin_crealf(X)

/* Check that the signs of the real and/or imaginary parts of two
   complex numbers match.  */
#define CKSGN(X,Y) (CKSGN_R(X,Y) || CKSGN_I(X,Y))
#define CKSGN_R(X,Y) (CPSGN(1,CREAL(X)) != CPSGN(1,CREAL(Y)))
#define CKSGN_I(X,Y) (CPSGN(1,CIMAG(X)) != CPSGN(1,CIMAG(Y)))

/* Test that (cproj(X) == ZERO+Inf) and that the signs of the
   imaginary parts match.  ZERO is +/- 0i.  */
#ifndef __SPU__
#define TEST_CST_INF(X,ZERO) do { \
  if (CPROJF(X) != ZERO+INF || CKSGN_I(CPROJF(X),ZERO+INF)) \
    link_error(__LINE__); \
  if (CPROJ(X) != ZERO+INF || CKSGN_I(CPROJ(X),ZERO+INF)) \
    link_error(__LINE__); \
  if (CPROJL(X) != ZERO+INF || CKSGN_I(CPROJL(X),ZERO+INF)) \
    link_error(__LINE__); \
} while (0)
#else
#define TEST_CST_INF(X,ZERO) do { \
  if (CPROJ(X) != ZERO+INF || CKSGN_I(CPROJ(X),ZERO+INF)) \
    link_error(__LINE__); \
  if (CPROJL(X) != ZERO+INF || CKSGN_I(CPROJL(X),ZERO+INF)) \
    link_error(__LINE__); \
} while (0)
#endif

/* Test that (cproj(X) == X) for all finite (X).  */
#define TEST_CST(X) do { \
  if (CPROJF(X) != (X) || CKSGN(CPROJF(X),(X))) \
    link_error(__LINE__); \
} while (0)

/* Test that cproj(X + I*INF) -> (ZERO + INF), where ZERO is +-0i.
   NEG is either blank or a minus sign when ZERO is negative.  */
#ifndef __SPU__
#define TEST_IMAG_INF(NEG,ZERO) do { \
  if (CPROJF(f+I*NEG INF) != ZERO+INF \
      || CKSGN_I (CPROJF(f+I*NEG INF), ZERO+INF)) \
    link_error(__LINE__); \
  if (CPROJ(d+I*NEG INF) != ZERO+INF \
      || CKSGN_I (CPROJ(d+I*NEG INF), ZERO+INF)) \
    link_error(__LINE__); \
  if (CPROJL(ld+I*NEG INF) != ZERO+INF \
      || CKSGN_I (CPROJL(ld+I*NEG INF), ZERO+INF)) \
    link_error(__LINE__); \
} while (0)
#else
#define TEST_IMAG_INF(NEG,ZERO) do { \
  if (CPROJ(d+I*NEG INF) != ZERO+INF \
      || CKSGN_I (CPROJ(d+I*NEG INF), ZERO+INF)) \
    link_error(__LINE__); \
  if (CPROJL(ld+I*NEG INF) != ZERO+INF \
      || CKSGN_I (CPROJL(ld+I*NEG INF), ZERO+INF)) \
    link_error(__LINE__); \
} while (0)
#endif

/* Like TEST_IMAG_INF, but check that side effects are honored.  */
#ifndef __SPU__
#define TEST_IMAG_INF_SIDE_EFFECT(NEG,ZERO) do { \
  int side = 4; \
  if (CPROJF(++side+I*NEG INF) != ZERO+INF \
      || CKSGN_I (CPROJF(++side+I*NEG INF), ZERO+INF)) \
    link_error(__LINE__); \
  if (CPROJ(++side+I*NEG INF) != ZERO+INF \
      || CKSGN_I (CPROJ(++side+I*NEG INF), ZERO+INF)) \
    link_error(__LINE__); \
  if (CPROJL(++side+I*NEG INF) != ZERO+INF \
      || CKSGN_I (CPROJL(++side+I*NEG INF), ZERO+INF)) \
    link_error(__LINE__); \
  if (side != 10) \
    link_error(__LINE__); \
} while (0)
#else
#define TEST_IMAG_INF_SIDE_EFFECT(NEG,ZERO) do { \
  int side = 4; \
  if (CPROJ(++side+I*NEG INF) != ZERO+INF \
      || CKSGN_I (CPROJ(++side+I*NEG INF), ZERO+INF)) \
    link_error(__LINE__); \
  if (CPROJL(++side+I*NEG INF) != ZERO+INF \
      || CKSGN_I (CPROJL(++side+I*NEG INF), ZERO+INF)) \
    link_error(__LINE__); \
  if (side != 8) \
    link_error(__LINE__); \
} while (0)
#endif

/* Test that cproj(INF, POSITIVE) -> INF+0i.  NEG is either blank or a
   minus sign to test negative INF.  */
#ifndef __SPU__
#define TEST_REAL_INF(NEG) do { \
  __real cf = NEG INF; \
  __imag cf = (x ? 4 : 5); \
  if (CPROJF(cf) != INF \
      || CKSGN_I (CPROJF(cf), INF)) \
    link_error(__LINE__); \
  __real cd = NEG INF; \
  __imag cd = (x ? 4 : 5); \
  if (CPROJ(cd) != INF \
      || CKSGN_I (CPROJ(cd), INF)) \
    link_error(__LINE__); \
  __real cld = NEG INF; \
  __imag cld = (x ? 4 : 5); \
  if (CPROJL(cld) != INF \
      || CKSGN_I (CPROJL(cld), INF)) \
    link_error(__LINE__); \
} while (0)
#else
#define TEST_REAL_INF(NEG) do { \
  __real cd = NEG INF; \
  __imag cd = (x ? 4 : 5); \
  if (CPROJ(cd) != INF \
      || CKSGN_I (CPROJ(cd), INF)) \
    link_error(__LINE__); \
  __real cld = NEG INF; \
  __imag cld = (x ? 4 : 5); \
  if (CPROJL(cld) != INF \
      || CKSGN_I (CPROJL(cld), INF)) \
    link_error(__LINE__); \
} while (0)
#endif

/* Like TEST_REAL_INF, but check that side effects are honored.  */
#ifndef __SPU__
#define TEST_REAL_INF_SIDE_EFFECT(NEG) do { \
  int side = -9; \
  __real cf = NEG INF; \
  __imag cf = (x ? 4 : 5); \
  if (CPROJF((++side,cf)) != INF \
      || CKSGN_I (CPROJF((++side,cf)), INF)) \
    link_error(__LINE__); \
  __real cd = NEG INF; \
  __imag cd = (x ? 4 : 5); \
  if (CPROJ((++side,cd)) != INF \
      || CKSGN_I (CPROJ((++side,cd)), INF)) \
    link_error(__LINE__); \
  __real cld = NEG INF; \
  __imag cld = (x ? 4 : 5); \
  if (CPROJL((++side,cld)) != INF \
      || CKSGN_I (CPROJL((++side,cld)), INF)) \
    link_error(__LINE__); \
  if (side != -3) \
    link_error(__LINE__); \
} while (0)
#else
#define TEST_REAL_INF_SIDE_EFFECT(NEG) do { \
  int side = -9; \
  __real cd = NEG INF; \
  __imag cd = (x ? 4 : 5); \
  if (CPROJ((++side,cd)) != INF \
      || CKSGN_I (CPROJ((++side,cd)), INF)) \
    link_error(__LINE__); \
  __real cld = NEG INF; \
  __imag cld = (x ? 4 : 5); \
  if (CPROJL((++side,cld)) != INF \
      || CKSGN_I (CPROJL((++side,cld)), INF)) \
    link_error(__LINE__); \
  if (side != -5) \
    link_error(__LINE__); \
} while (0)
#endif

void foo (_Complex long double cld, _Complex double cd, _Complex float cf,
	  long double ld, double d, float f, int x)
{
  TEST_CST_INF (INF+0I, 0);
  TEST_CST_INF (INF-0I, -0.FI);
  TEST_CST_INF (INF+4I, 0);
  TEST_CST_INF (INF-4I, -0.FI);
  TEST_CST_INF (-INF+0I, 0);
  TEST_CST_INF (-INF-0I, -0.FI);
  TEST_CST_INF (-INF+4I, 0);
  TEST_CST_INF (-INF-4I, -0.FI);

  TEST_CST_INF (0+I*INF, 0);
  TEST_CST_INF (0-I*INF, -0.FI);
  TEST_CST_INF (23+I*INF, 0);
  TEST_CST_INF (23-I*INF, -0.FI);
  TEST_CST_INF (-0.F+I*INF, 0);
  TEST_CST_INF (-0.F-I*INF, -0.FI);
  TEST_CST_INF (-23+I*INF, 0);
  TEST_CST_INF (-23-I*INF, -0.FI);

  TEST_CST_INF (INF+I*INF, 0);
  TEST_CST_INF (INF-I*INF, -0.FI);
  TEST_CST_INF (-INF+I*INF, 0);
  TEST_CST_INF (-INF-I*INF, -0.FI);
  
  TEST_CST (0);
  TEST_CST (-0.F);
  TEST_CST (0-0.FI);
  TEST_CST (-0.F-0.FI);
  
  TEST_CST (22+3I);
  TEST_CST (22-3I);
  TEST_CST (-22+3I);
  TEST_CST (-22-3I);

  TEST_IMAG_INF (,0.FI);
  TEST_IMAG_INF (-,-0.FI);

#ifdef __OPTIMIZE__
  TEST_REAL_INF( );
  TEST_REAL_INF(-);
  
  TEST_IMAG_INF_SIDE_EFFECT (,0.FI);
  TEST_IMAG_INF_SIDE_EFFECT (-,-0.FI);

  TEST_REAL_INF_SIDE_EFFECT( );
  TEST_REAL_INF_SIDE_EFFECT(-);
#endif

  return;
}

int main (void)
{
  return 0;
}
