/* Check load on condition for global char.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O2 -march=z13" } */

/* { dg-final { scan-assembler "locrnh\t%r.?,%r.?" } } */
#include <stdbool.h>

char g = 42;

int foo (int *a, unsigned int n)
{
  int min = 999999;
  char bla = 3;
  for (int i = 0; i < n; i++)
    {
      if (a[i] < min)
	{
	  bla = g;
	}
    }

  if (bla == 42)
    min += 1;
  return min;
}
