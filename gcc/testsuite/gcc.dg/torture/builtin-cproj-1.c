/* Copyright (C) 2010  Free Software Foundation.

   Verify that folding of built-in cproj is correctly performed by the
   compiler.

   Origin: Kaveh R. Ghazi,  April 9, 2010.  */

/* { dg-do link } */

/* All references to link_error should go away at compile-time.  The
   argument is the __LINE__ number.  It appears in the tree dump file
   and aids in debugging should any of the tests fail.  */
extern void link_error(int);

#define CPROJ(X) __builtin_cproj(X)
#define CPROJF(X) __builtin_cprojf(X)
#define CPROJL(X) __builtin_cprojl(X)
#define INF __builtin_inff()
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
#define TEST_CST_INF(X,ZERO) do { \
  if (CPROJF(X) != ZERO+INF || CKSGN_I(CPROJF(X),ZERO+INF)) \
    link_error(__LINE__); \
  if (CPROJ(X) != ZERO+INF || CKSGN_I(CPROJ(X),ZERO+INF)) \
    link_error(__LINE__); \
  if (CPROJL(X) != ZERO+INF || CKSGN_I(CPROJL(X),ZERO+INF)) \
    link_error(__LINE__); \
} while (0)

/* Test that (cproj(X) == X) for all finite (X).  */
#define TEST_CST(X) do { \
  if (CPROJF(X) != (X) || CKSGN(CPROJF(X),(X))) \
    link_error(__LINE__); \
} while (0)

void foo (void)
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

  return;
}

int main (void)
{
  return 0;
}
