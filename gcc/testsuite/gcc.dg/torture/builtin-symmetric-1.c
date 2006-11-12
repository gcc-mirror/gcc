/* Copyright (C) 2006  Free Software Foundation.

   Verify that built-in math function folding of symmetric even and
   odd functions is correctly performed by the compiler.

   Origin: Kaveh R. Ghazi,  November 09, 2006.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" } */

/* All references to link_error should go away at compile-time.  */
extern void link_error(int);

/* Test that FUNC(-ARG) == FUNC(ARG).  */
#define TESTIT_EVEN(FUNC) do { \
  if (__builtin_##FUNC##f(-xf) != __builtin_##FUNC##f(xf)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(-x) != __builtin_##FUNC(x)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(-xl) != __builtin_##FUNC##l(xl)) \
    link_error(__LINE__); \
  } while (0)

/* Test that FUNC(-VAR) == FUNC(VAR), where VAR has an int type.  */
#define TESTIT_EVEN_I(FUNC,VAR) do { \
  if (__builtin_##FUNC(-VAR) != __builtin_##FUNC(VAR)) \
    link_error(__LINE__); \
  } while (0)

/* Test that -FUNC(ARG) == FUNC(-ARG).  */
#define TESTIT_ODD(FUNC) do { \
  if (-__builtin_##FUNC##f(-xf) != __builtin_##FUNC##f(xf)) \
    link_error(__LINE__); \
  if (-__builtin_##FUNC(-x) != __builtin_##FUNC(x)) \
    link_error(__LINE__); \
  if (-__builtin_##FUNC##l(-xl) != __builtin_##FUNC##l(xl)) \
    link_error(__LINE__); \
  } while (0)

void foo (float xf, double x, long double xl,
	  int i, long l, long long ll, __INTMAX_TYPE__ im)
{
  TESTIT_EVEN(cos);
  TESTIT_EVEN(cosh);
  TESTIT_EVEN(fabs);

  TESTIT_EVEN_I(abs, i);
  TESTIT_EVEN_I(imaxabs, im);
  TESTIT_EVEN_I(labs, l);
  TESTIT_EVEN_I(llabs, ll);

  TESTIT_ODD(asin);
  TESTIT_ODD(asinh);
  TESTIT_ODD(atan);
  TESTIT_ODD(atanh);
  TESTIT_ODD(cbrt);
  TESTIT_ODD(erf);
  TESTIT_ODD(llrint);
  TESTIT_ODD(llround);
  TESTIT_ODD(lrint);
  TESTIT_ODD(lround);
  TESTIT_ODD(nearbyint);
  TESTIT_ODD(rint);
  TESTIT_ODD(round);
  TESTIT_ODD(sin);
  TESTIT_ODD(sinh);
  TESTIT_ODD(tan);
  TESTIT_ODD(tanh);
  TESTIT_ODD(trunc);
}

int main()
{
  foo (1,1,1,1,1,1,1);
  return 0;
}
