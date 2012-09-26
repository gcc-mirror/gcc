/* Performing a 64-bit division should not pull in the unwinder.  */

/* The test is expected to fail for GNU/Linux; see PR54723.  */
/* { dg-do run { xfail *-*-linux* } } */
/* { dg-options "-O0" } */

#include <stdlib.h>

long long
foo (long long c, long long d)
{
  return c/d;
}

long long x = 0;
long long y = 1;

extern int (*_Unwind_RaiseException) (void *) __attribute__((weak));

int main(void)
{
  if (&_Unwind_RaiseException != NULL)
    abort ();;
  return foo (x, y);
}
