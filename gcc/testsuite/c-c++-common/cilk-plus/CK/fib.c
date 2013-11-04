/* { dg-options "-fcilkplus" } */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fcilkplus -lcilkrts" { target { i?86-*-* x86_64-*-* } } } */

#if HAVE_IO
#include <stdio.h>
#endif

int fib        (int);
int fib_serial (int);

int main(void)
{
  int ii = 0, error = 0;
  int fib_result[41], fib_serial_result[41];
#if HAVE_IO

  for (ii = 0; ii <= 40; ii++)
    printf("fib (%2d) = %10d\n", ii, fib (ii));
#else
  for (ii = 0; ii <= 40; ii++)
    {
      fib_result[ii]        = fib (ii);
      fib_serial_result[ii] = fib_serial (ii);
    }

  for (ii = 0; ii <= 40; ii++)
    {
      if (fib_result[ii] != fib_serial_result[ii])
	error = 1;
    }
#endif
  return error;
}

int fib_serial (int n)
{
  int x = 0, y = 0;
  if (n < 2)
    return n;
  else
    {
      x = fib (n-1);
      y = fib (n-2);
      return (x+y);
    }
}

int fib(int n)
{
  int x = 0, y = 0;
  if (n < 2) 
    return n;
  else
  {
    x = _Cilk_spawn fib(n-1);
    y = fib(n-2);
    _Cilk_sync;
    return (x+y);
  }
}
