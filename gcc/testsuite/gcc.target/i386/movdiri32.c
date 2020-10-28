/* { dg-do run { target movdir } } */
/* { dg-options "-mmovdiri -O2" } */

#include <x86intrin.h>

unsigned int dest = -1;

int
main ()
{
  if (!__builtin_cpu_supports ("movdiri"))
    return 0;

  _directstoreu_u32 (&dest, 0xbadbeef);

  if (dest != 0xbadbeef)
    abort ();

  return 0;
}
