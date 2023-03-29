/* Test C11 definition of LDBL_EPSILON.  Based on
   gcc.target/powerpc/rs6000-ldouble-2.c.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <float.h>

extern void abort (void);
extern void exit (int);

int
main (void)
{
  volatile long double ee = 1.0;
  long double eps = ee;
  while (ee + 1.0 != 1.0)
    {
      eps = ee;
      ee = eps / 2;
    }
  if (eps != LDBL_EPSILON)
    abort ();
  exit (0);
}
