/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

/* Vector string clear right-most bytes of unsigned char.  */
vector unsigned char
clrr (vector unsigned char arg, int n)
{
  return vec_clrr (arg, n);
}

/* { dg-final { scan-assembler {\mvclrrb\M} { target be } } } */
/* { dg-final { scan-assembler {\mvclrlb\M} { target le } } } */
