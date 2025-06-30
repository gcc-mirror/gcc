/* Test checking that the backtrace on large frame size with additional
   SP shift in the prologue won't broken when compiled with the
   -fstack-clash-protection option.  */
/* { dg-do run { target { *-*-linux* } } } */
/* -O0 does not have enough optimizations.
   -O2/-O3 does inline and reduces number of addresses in the backtrace.  */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O2" "-O3" } } */
/* { dg-options "-g -fstack-clash-protection" } */

#include <execinfo.h>

#define MAX 4000

void goo ()
{
  int addresses;
  void *buffer[10];

  addresses = backtrace (buffer, 10);
  if (addresses != 6)
    __builtin_abort ();
}

int foo (int a)
{
  long long A[MAX];
  for (int i = 0; i < MAX; i++)
    A[i] = i;

  goo ();

  return A[a % MAX];
}

int main ()
{
  if (foo (20) != 20)
    __builtin_abort ();
  return 0;
}
