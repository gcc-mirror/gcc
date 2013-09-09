/* { dg-do run } */
/* { dg-options "-O2 -mARC700 --save-temps -mno-mpy" } */

#include <stdlib.h>

/* Hide value propagation from the optimizers.  */
static int
id (int i)
{
  asm ("": "+Xr" (i));
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
