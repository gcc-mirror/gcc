/* { dg-do run } */
/* { dg-options "-O2 -mARC700 --save-temps" } */

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

/* { dg-final { scan-assembler "mpyhu\[ \t\]" } } */
