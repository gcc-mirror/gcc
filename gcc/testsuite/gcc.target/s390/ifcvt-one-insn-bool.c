/* Check load on condition for bool.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O2 -march=z13 -mzarch" } */

/* { dg-final { scan-assembler "lochile\t%r.?,1" } } */
#include <stdbool.h>

int foo (int *a, unsigned int n)
{
  int min = 999999;
  bool bla = false;
  for (int i = 0; i < n; i++)
    {
      if (a[i] < min)
	{
	  bla = true;
	}
    }

  if (bla)
    min += 1;
  return min;
}
