/* { dg-do run } */
/* { dg-skip-if "ARC700 always has mpy option on" { arc700 } } */
/* { dg-skip-if "ARC600 doesn't have mpy instruction" { arc6xx } } */
/* { dg-options "-O2 -mmpy-option=0 -w" } */

#include <stdlib.h>

/* Hide value propagation from the optimizers.  */
static int
id (int i)
{
  asm ("": "+r" (i));
  return i;
}

static int
mulhigh (unsigned a, unsigned b)
{
  return (unsigned long long) a * b >> 32;
}

int
main (void)
{
  if (mulhigh (id (0x12345678), id (0x90abcdef)) != 0xa49a83e)
    abort ();
  return 0;
}

/* { dg-final { scan-assembler-not "mpyhu\[ \t\]" } } */
/* { dg-final { scan-assembler-not "@__muldi3" } } */
/* { dg-final { scan-assembler "@__umulsi3_highpart" } } */
