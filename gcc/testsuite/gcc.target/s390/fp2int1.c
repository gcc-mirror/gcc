/* Test for the 32 bit fp to 64 bit int conversion routines.
   
   On S/390 32 bit we use our own implementations in order to be IEEE
   complaint as we are with our machine instructions.  These missed to
   throw FE_INVALID exceptions in a bunch of cases.  */

/* { dg-do run { target s390-*-* } } */
/* { dg-options "-O3 -mesa" } */
/* { dg-require-effective-target fenv_exceptions } */

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <fenv.h>

#define INFINITYf       (__builtin_inff())
#define INFINITY        (__builtin_inf())
#define INFINITYl       (__builtin_infl())
#define NANf            (__builtin_nanf (""))
#define NAN             (__builtin_nan (""))
#define NANl            (__builtin_nanl (""))

#define TESTEXCEPT_FUNC(FUNC, TYPE_FROM, TYPE_TO)			\
  TYPE_TO								\
  __attribute__((noinline)) FUNC (TYPE_FROM a)				\
  {									\
    asm volatile ("" : : "f" (a));					\
    return (TYPE_TO)a;							\
  }

#define TESTEXCEPT(FUNC, EXCEPT, EXPECT, VALUE, TYPE_TO)		\
  {									\
    TYPE_TO b;								\
    feclearexcept (FE_ALL_EXCEPT);					\
    b = FUNC (VALUE);							\
    if ((fetestexcept (EXCEPT) & (EXCEPT)) != EXPECT)			\
      {									\
	printf ("FAIL in line: %d\n", __LINE__);			\
	abort ();							\
      }									\
  }

#define TESTEXCEPT_FUNC_ALLFLOATS(FUNC, TYPE_TO)		\
  TESTEXCEPT_FUNC (FUNC##_f, float, TYPE_TO);			\
  TESTEXCEPT_FUNC (FUNC##_d, double, TYPE_TO);			\
  TESTEXCEPT_FUNC (FUNC##_l, long double, TYPE_TO);		\

#define TESTEXCEPT_ALLFLOATS(FUNC, EXCEPT, EXPECT, VALUE, TYPE_TO)	\
  TESTEXCEPT (FUNC##_f, EXCEPT, EXPECT, VALUE##f, TYPE_TO);		\
  TESTEXCEPT (FUNC##_d, EXCEPT, EXPECT, VALUE, TYPE_TO);		\
  TESTEXCEPT (FUNC##_l, EXCEPT, EXPECT, VALUE##l, TYPE_TO);		\

TESTEXCEPT_FUNC_ALLFLOATS (a, unsigned long long);
TESTEXCEPT_FUNC_ALLFLOATS (u, long long);


int
main ()
{
  /* Prevent getting signals.  */
  fedisableexcept (FE_INVALID);

  /* To unsigned long long */

  TESTEXCEPT_ALLFLOATS (a, FE_INVALID, FE_INVALID, INFINITY, unsigned long long);
  TESTEXCEPT_ALLFLOATS (a, FE_INVALID, FE_INVALID, -INFINITY, unsigned long long);
  TESTEXCEPT_ALLFLOATS (a, FE_INVALID, FE_INVALID, NAN, unsigned long long);
  TESTEXCEPT_ALLFLOATS (a, FE_INVALID, FE_INVALID, -NAN, unsigned long long);

  /* Negative values >-1.0 must not cause FE_INVALID.  */
  TESTEXCEPT_ALLFLOATS (a, FE_INVALID, 0, -0x0.ffffffp0, unsigned long long);
  /* -1.0 instead must.  */
  TESTEXCEPT_ALLFLOATS (a, FE_INVALID, FE_INVALID, -0x1.0p+0, unsigned long long);
  TESTEXCEPT_ALLFLOATS (a, FE_INVALID, 0, 0x1.0p+63, unsigned long long);
  TESTEXCEPT_ALLFLOATS (a, FE_INVALID, FE_INVALID, 0x1.0p+64, unsigned long long);

  /* To signed long long */

  TESTEXCEPT_ALLFLOATS (u, FE_INVALID, FE_INVALID, INFINITY, long long);
  TESTEXCEPT_ALLFLOATS (u, FE_INVALID, FE_INVALID, -INFINITY, long long);
  TESTEXCEPT_ALLFLOATS (u, FE_INVALID, FE_INVALID, NAN, long long);
  TESTEXCEPT_ALLFLOATS (u, FE_INVALID, FE_INVALID, -NAN, long long);

  TESTEXCEPT_ALLFLOATS (u, FE_INVALID, 0, -0x1.0p+63, long long);
  TESTEXCEPT_ALLFLOATS (u, FE_INVALID, FE_INVALID, -0x1.1p+63, long long);
  TESTEXCEPT_ALLFLOATS (u, FE_INVALID, 0, 0x0.fffffp+63, long long);
  TESTEXCEPT_ALLFLOATS (u, FE_INVALID, FE_INVALID, 0x1.0p+63, long long);

  /* If there are additional bits which would not make it into the
     integer value no exception is supposed to occur.  */
  TESTEXCEPT (u_l, FE_INVALID,          0, -0x1.000000000000000123p+63l, long long);
  TESTEXCEPT (u_l, FE_INVALID, FE_INVALID, -0x1.000000000000000223p+63l, long long);

  return 0;
}
