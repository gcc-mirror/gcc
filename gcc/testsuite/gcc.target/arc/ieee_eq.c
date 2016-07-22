/* { dg-do run } */
/* { dg-options "-O2" } */

#include <stdio.h>
#include <float.h>

#define TEST_EQ(TYPE,X,Y,RES)				\
  do {							\
    volatile TYPE a, b;					\
    a = (TYPE) X;					\
    b = (TYPE) Y;					\
    if ((a == b) != RES)				\
      {							\
	printf ("Runtime computation error @%d. %g "	\
		"!= %g\n", __LINE__, a, b);		\
	error = 1;					\
      }							\
  } while (0)

#ifndef __HS__
/* Special type of NaN found when using double FPX instructions.  */
static const unsigned long long __nan = 0x7FF0000080000000ULL;
# define W (*(double *) &__nan)
#else
# define W __builtin_nan ("")
#endif

#define Q __builtin_nan ("")
#define H __builtin_inf ()

int main (void)
{
  int error = 0;

  TEST_EQ (double, 1, 1, 1);
  TEST_EQ (double, 1, 2, 0);
  TEST_EQ (double, W, W, 0);
  TEST_EQ (double, Q, Q, 0);
  TEST_EQ (double, __DBL_MAX__, __DBL_MAX__, 1);
  TEST_EQ (double, __DBL_MIN__, __DBL_MIN__, 1);
  TEST_EQ (double, H, H, 1);

  if (error)
    __builtin_abort ();

  return 0;
}
