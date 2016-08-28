/* __builtin_ms_va_list is only supported for x86_64 -m64.  */
/* { dg-do run { target { x86_64-*-* && { ! ilp32 } } } } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto" } */

#include <stdio.h>

int __attribute__((ms_abi))
foo (int n, ...)
{
  __builtin_ms_va_list ap;
  int sum = 0;

  __builtin_ms_va_start (ap, n);

  while (n--)
    {
      sum += __builtin_va_arg (ap, int);
      printf ("sum = %d\n", sum);
    }

  __builtin_ms_va_end (ap);

  return sum;
}

int
main (void)
{
  int res = foo (10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

  if (res != 55)
    __builtin_abort ();

  return 0;
}
