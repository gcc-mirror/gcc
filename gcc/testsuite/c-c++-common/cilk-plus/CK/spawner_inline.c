/* { dg-do run  { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-fcilkplus" } */
/* { dg-options "-lcilkrts" { target { i?86-*-* x86_64-*-* } } } */

#include <stdlib.h>
#define DEFAULT_VALUE 30
int fib (int n)
{
  if (n<2)
    return n;
  else
    {
      int x, y;
      x = _Cilk_spawn fib (n-1);
      y = _Cilk_spawn fib (n-2);
      _Cilk_sync;
      return (x+y);
      return 5;
    }
}

int main_parallel (int argc, char *argv[])
{
  int n, result;
  if (argc == 2)
    n = atoi(argv[1]);
  else
    n = DEFAULT_VALUE;
  result = _Cilk_spawn fib(n);
  _Cilk_sync; 
  return result;
}

int fib_serial (int n)
{
  int x, y;
  if (n < 2)
    return n;
  else
    {
      x = fib (n-1);
      y = fib (n-2);
      return (x+y);
    }
}
  
int main_serial (int argc, char *argv[])
{
  int n, result;

  if (argc == 2)
    n = atoi (argv[1]);
  else
    n = DEFAULT_VALUE;
  result = fib_serial (n);

  return result;
}

int main (void)
{
  if (main_serial (1, 0) != main_parallel (1,0))
    return 1;
  else 
    return 0;
}

