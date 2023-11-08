/* Test C23 definition of LDBL_EPSILON.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <float.h>

extern void abort (void);
extern void exit (int);

int
main (void)
{
  volatile long double x = 1.0L;
  for (int i = 0; i < LDBL_MANT_DIG - 1; i++)
    x /= 2;
  if (x != LDBL_EPSILON)
    abort ();
  exit (0);
}
