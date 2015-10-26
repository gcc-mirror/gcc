/* Copyright (C) 2006  Free Software Foundation.

   Verify that built-in math function folding of fmin/fmax is
   correctly performed by the compiler.

   Origin: Kaveh R. Ghazi,  November 13, 2006.  */

/* { dg-do link } */
/* { dg-options "-fno-math-errno" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

/* All references to link_error should go away at compile-time.  */
extern void link_error(int);

#define DECLARE(FUNC) \
  extern float FUNC##f (float); \
  extern double FUNC (double); \
  extern long double FUNC##l (long double)
#define DECLARE2(FUNC) \
  extern float FUNC##f (float, float); \
  extern double FUNC (double, double); \
  extern long double FUNC##l (long double, long double)

DECLARE2(fmin);
DECLARE2(fmax);
DECLARE(fabs);
extern int pure(int) __attribute__ ((__pure__));

/* Test that FUNC(x,x) == x.  We cast to (long) so "!=" folds.  */
#define TEST_EQ(FUNC) do { \
  if ((long)FUNC##f(xf,xf) != (long)xf) \
    link_error(__LINE__); \
  if ((long)FUNC(x,x) != (long)x) \
    link_error(__LINE__); \
  if ((long)FUNC##l(xl,xl) != (long)xl) \
    link_error(__LINE__); \
  } while (0)

/* Test that FUNC(purefn,purefn) == purefn.  We cast to (long) so "!=" folds.  */
#define TEST_EQ_PURE(FUNC) do { \
  if ((long)FUNC##f(pure(i),pure(i)) != (long)FUNC##f(pure(i),pure(i))) \
    link_error(__LINE__); \
  if ((long)FUNC(pure(i),pure(i)) != (long)FUNC(pure(i),pure(i))) \
    link_error(__LINE__); \
  if ((long)FUNC##l(pure(i),pure(i)) != (long)FUNC##l(pure(i),pure(i))) \
    link_error(__LINE__); \
  } while (0)

/* Test that FIXFUNC(FUNC(int1,int2)) == (TYPE)FUNC(int1,int2),
   i.e. FIXFUNC should be folded away and replaced with a cast.  */
#define TEST_FIXFUNC(FUNC,FIXFUNC,TYPE) do { \
  if (FIXFUNC##f(FUNC##f(i,j)) != (TYPE)FUNC##f(i,j)) \
    link_error(__LINE__); \
  if (FIXFUNC(FUNC(i,j)) != (TYPE)FUNC(i,j)) \
    link_error(__LINE__); \
  if (FIXFUNC##l(FUNC##l(i,j)) != (TYPE)FUNC##l(i,j)) \
    link_error(__LINE__); \
  } while (0)

/* Test that FUNC(int1,int2) has an integer return type.  */
#define TEST_INT(FUNC) do { \
  TEST_FIXFUNC(FUNC,__builtin_lround,long); \
  TEST_FIXFUNC(FUNC,__builtin_llround,long long); \
  TEST_FIXFUNC(FUNC,__builtin_lrint,long); \
  TEST_FIXFUNC(FUNC,__builtin_llrint,long long); \
  TEST_FIXFUNC(FUNC,__builtin_lceil,long); \
  TEST_FIXFUNC(FUNC,__builtin_llceil,long long); \
  TEST_FIXFUNC(FUNC,__builtin_lfloor,long); \
  TEST_FIXFUNC(FUNC,__builtin_llfloor,long long); \
  } while (0)

/* Test that (long)fabs(FUNC(fabs(x),fabs(y))) ==
   (long)FUNC(fabs(x),fabs(y)).  We cast to (long) so "!=" folds.  */
#define TEST_NONNEG(FUNC) do { \
  if ((long)fabsf(FUNC##f(fabsf(xf),fabsf(yf))) != (long)FUNC##f(fabsf(xf),fabsf(yf))) \
    link_error(__LINE__); \
  if ((long)fabs(FUNC(fabs(x),fabs(y))) != (long)FUNC(fabs(x),fabs(y))) \
    link_error(__LINE__); \
  if ((long)fabsl(FUNC##l(fabsl(xl),fabsl(yl))) != (long)FUNC##l(fabsl(xl),fabsl(yl))) \
    link_error(__LINE__); \
  } while (0)

/* Test that FUNC(NaN,x) == x.  We cast to (long) so "!=" folds.  Set
   parameter SIGNAL to `s' for testing signaling NaN.  */
#define TEST_NAN(FUNC,SIGNAL) do { \
  if ((long)FUNC##f(__builtin_nan##SIGNAL##f(""),xf) != (long)xf) \
    link_error(__LINE__); \
  if ((long)FUNC##f(xf,__builtin_nan##SIGNAL##f("")) != (long)xf) \
    link_error(__LINE__); \
  if ((long)FUNC(__builtin_nan##SIGNAL(""),x) != (long)x) \
    link_error(__LINE__); \
  if ((long)FUNC(x,__builtin_nan##SIGNAL("")) != (long)x) \
    link_error(__LINE__); \
  if ((long)FUNC##l(__builtin_nan##SIGNAL##l(""),xl) != (long)xl) \
    link_error(__LINE__); \
  if ((long)FUNC##l(xl,__builtin_nan##SIGNAL##l("")) != (long)xl) \
    link_error(__LINE__); \
  } while (0)

void __attribute__ ((__noinline__))
     foo (float xf, double x, long double xl,
	  float yf, double y, long double yl,
	  int i, int j)
{
  TEST_EQ(fmin);
  TEST_EQ(fmax);

#ifdef __OPTIMIZE__
  TEST_EQ_PURE(fmin);
  TEST_EQ_PURE(fmax);
#endif

  TEST_INT(fmin);
  TEST_INT(fmax);
  
  TEST_NONNEG(fmin);
  TEST_NONNEG(fmax);

  TEST_NAN(fmin,);
  TEST_NAN(fmax,);
  TEST_NAN(fmin,s);
  TEST_NAN(fmax,s);
}

int main()
{
  foo (1,1,1,1,1,1,1,1);
  return 0;
}
