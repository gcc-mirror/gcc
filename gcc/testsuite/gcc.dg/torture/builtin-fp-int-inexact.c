/* Test -fno-fp-int-builtin-inexact.  */
/* { dg-do run } */
/* { dg-options "-fno-fp-int-builtin-inexact" } */
/* { dg-add-options c99_runtime } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>

/* Define functions locally to ensure that if the calls are not
   expanded inline, failures do not occur because of libm raising
   "inexact".  */

#define LOCAL_FN(NAME, TYPE)			\
  __attribute__ ((noinline, noclone)) TYPE	\
  NAME (TYPE x)					\
  {						\
    return x;					\
  }

#define LOCAL_FNS(NAME)				\
  LOCAL_FN (NAME, double)			\
  LOCAL_FN (NAME ## f, float)			\
  LOCAL_FN (NAME ## l, long double)

LOCAL_FNS (ceil)
LOCAL_FNS (floor)
LOCAL_FNS (round)
LOCAL_FNS (trunc)

extern void abort (void);
extern void exit (int);

#define TEST(FN, TYPE)				\
  do						\
    {						\
      volatile TYPE a = 1.5, b;			\
      b = FN (a);				\
      if (fetestexcept (FE_INEXACT))		\
	abort ();				\
    }						\
  while (0)

#define FN_TESTS(FN)					\
  do							\
    {							\
      TEST (__builtin_ ## FN, double);			\
      TEST (__builtin_ ## FN ## f, float);		\
      TEST (__builtin_ ## FN ## l, long double);	\
    }							\
  while (0)

static void
main_test (void)
{
  FN_TESTS (ceil);
  FN_TESTS (floor);
  FN_TESTS (round);
  FN_TESTS (trunc);
}

/* This file may be included by architecture-specific tests.  */

#ifndef ARCH_MAIN

int
main (void)
{
  main_test ();
  exit (0);
}

#endif
