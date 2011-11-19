/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O2" } */

int foo(int *arr, int v)
{
  int r = 0;
  int i;
  __transaction_atomic {
    for (i = 0; i < 10; ++i)
      if (arr[i] < 27)
	r += arr[i] += v;
  }
  return r;
}
