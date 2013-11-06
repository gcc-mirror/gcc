/* { dg-options "-fcilkplus" } */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fcilkplus -lcilkrts" { target { i?86-*-* x86_64-*-* } } } */

#if HAVE_IO
#include <stdio.h>
#endif

#define FIB_ITERATION 30

void fib        (int *, int);
int fib_serial (int);

int main(void)
{
  int ii = 0, error = 0;
  int fib_result[FIB_ITERATION+1], fib_serial_result[FIB_ITERATION+1];

#if HAVE_IO
  for (ii = 0; ii <= FIB_ITERATION; ii++)
    {
      int result = 0;
      fib (&result, ii); 
      printf("fib (%2d) = %10d\n", ii, result);
    }
#else
  for (ii = 0; ii <= FIB_ITERATION; ii++)
    {
      fib (&fib_result[ii], ii);
    }
  
  fib_serial_result[0] = 0;
  fib_serial_result[1] = 1;
  
  for (ii = 2; ii <= FIB_ITERATION; ii++)
    fib_serial_result[ii] = fib_serial_result[ii-1] + fib_serial_result[ii-2];
  
  for (ii = 0; ii <= FIB_ITERATION; ii++)
    {
      if (fib_result[ii] != fib_serial_result[ii])
	__builtin_abort ();
    }
#endif
  
  return 0;
}

int fib_serial (int n)
{
  int x = 0, y = 0;
  if (n < 2)
    return n;
  else
    {
      x = fib_serial (n-1);
      y = fib_serial (n-2);
      return (x+y);
    }
}

void fib(int *result, int n)
{
  int x = 0, y = 0;
  if (n < 2) 
    x = n;
  else
  {
    _Cilk_spawn fib(&x, n-1);
    fib(&y, n-2);
    _Cilk_sync;
  } 
 *result = (x+y);
}
