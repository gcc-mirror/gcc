/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus -w" } */

#include <stdio.h>
#include <stdlib.h>
#define DEFAULT_VALUE "30"

int fib (char *n_char)
{
  int n;
  char n_char_minus_one[20], n_char_minus_two[20];
  if (n_char)
    n = atoi (n_char);
  else
    n = atoi(DEFAULT_VALUE);
  
  if (n < 2)
    return n;
  else
    {	   
      int x, y;
      sprintf (n_char_minus_one,"%d", n-1); 
      sprintf (n_char_minus_two,"%d", n-2); 
      x = _Cilk_spawn fib (n_char_minus_one);
      y = _Cilk_spawn fib (n_char_minus_two);
      _Cilk_sync;
      return (x+y);
    }
}

int fib_serial (int n)
{
  int x, y;
  if (n < 2)
    return n;
  else
    {
      x = fib_serial (n-1);
      y = fib_serial (n-2);
      return (x+y);
    }
  return 0;
}

int main2_parallel (int argc, char *argv[])
{
  int n, result_parallel = 0;

  if (argc == 2)
    {
      result_parallel = _Cilk_spawn fib (argv[1]);
      _Cilk_sync; 
    }
  else
    {
      result_parallel = _Cilk_spawn fib((char *)"30");
      _Cilk_sync; 
    }
  return result_parallel;
}

int main2_serial (int argc, char *argv[])
{
  int n, result_serial = 0;
  if (argc == 2) 
    result_serial = fib_serial (atoi (argv[1]));
  else
    result_serial = fib_serial (atoi (DEFAULT_VALUE));

  return result_serial;
}

int main (void)
{
  if (main2_serial (1, 0) != main2_parallel (1,0))
    return 1;
  return 0;
}

