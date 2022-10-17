/* { dg-do compile } */
/* { dg-require-effective-target ti_c64xp } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "dpackx" } } */

#include <stdlib.h>

unsigned long long z = 0x012389ab4567cdefull;

unsigned long long __attribute__ ((noinline,noclone,noipa)) bar ()
{
  return (z << 48) | (z >> 16);
}

int main ()
{
  if (bar() != 0xcdef012389ab4567ull)
    abort ();
  exit (0);
}
