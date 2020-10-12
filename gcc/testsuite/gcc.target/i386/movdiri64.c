/* { dg-do run { target { movdir && { ! ia32 } } } } */
/* { dg-options "-mmovdiri -O2" } */

#include <x86intrin.h>

unsigned long long int dest = -1LL;

int
main ()
{
  if (!__builtin_cpu_supports ("movdiri"))
    return 0;

  _directstoreu_u64 (&dest, 0x12345678badbeef);

  if (dest != 0x12345678badbeef)
    abort ();

  return 0;
}
