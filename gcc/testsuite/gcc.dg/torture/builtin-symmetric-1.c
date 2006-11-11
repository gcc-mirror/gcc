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

/* Test that -FUNC(ARG) == FUNC(-ARG).  */
#define TESTIT_ODD(FUNC) do { \
  if (-__builtin_##FUNC##f(-xf) != __builtin_##FUNC##f(xf)) \
    link_error(__LINE__); \
  if (-__builtin_##FUNC(-x) != __builtin_##FUNC(x)) \
    link_error(__LINE__); \
  if (-__builtin_##FUNC##l(-xl) != __builtin_##FUNC##l(xl)) \
    link_error(__LINE__); \
  } while (0)

void foo (float xf, double x, long double xl)
{
  TESTIT_EVEN(cos);

  TESTIT_ODD(asin);
  TESTIT_ODD(asinh);
  TESTIT_ODD(atan);
  TESTIT_ODD(atanh);
  TESTIT_ODD(cbrt);
  TESTIT_ODD(sin);
  TESTIT_ODD(sinh);
  TESTIT_ODD(tan);
  TESTIT_ODD(tanh);
  TESTIT_ODD(erf);
}

int main()
{
  foo (1,1,1);
  return 0;
}
