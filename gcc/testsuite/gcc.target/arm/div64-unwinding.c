/* Performing a 64-bit division should not pull in the unwinder.  */

/* { dg-do run { target { { ! *-*-linux* } && { ! *-*-uclinux* } } } } */
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
