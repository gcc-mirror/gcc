/* Make sure if conversion for two instructions does not break
   anything (if it runs).  */

/* { dg-do run } */
/* { dg-options "-O2 -std=c99" } */

#include <limits.h>
#include <assert.h>

__attribute__ ((noinline))
int foo (int *a, int n)
{
  int min = 999999;
  int bla = 0;
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
  int a[] = {2, 1, -13, INT_MAX, INT_MIN, 0};

  int res = foo (a, sizeof (a) / sizeof (a[0]));

  assert (res == (INT_MIN + 1));
}
