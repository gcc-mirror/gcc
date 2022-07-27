/* Check if conversion for two instructions.  */

/* { dg-do run } */
/* { dg-options "-O2 -march=z13 --save-temps" } */

/* { dg-final { scan-assembler "locghih\t%r.?,1" } } */
/* { dg-final { scan-assembler "locgrh\t.*" } } */
#include <limits.h>
#include <stdio.h>
#include <assert.h>

__attribute__ ((noinline))
long foo (long *a, unsigned long n)
{
  long min = 999999;
  long bla = 0;
  for (int i = 0; i < n; i++)
    {
      if (a[i] < min)
	{
	  min = a[i];
	  bla = 1;
	}
    }

  if (bla)
    min += 1;
  return min;
}

int main()
{
  long a[] = {2, 1, -13, LONG_MAX, LONG_MIN, 0};

  long res = foo (a, sizeof (a) / sizeof (a[0]));

  assert (res == (LONG_MIN + 1));
}
