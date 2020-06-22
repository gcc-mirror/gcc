/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

/* Vector string clear left-most bytes of unsigned char.  */
vector unsigned char
clrl (vector unsigned char arg, int n)
{
  return vec_clrl (arg, n);
}

/* { dg-final { scan-assembler {\mvclrlb\M} { target be } } } */
/* { dg-final { scan-assembler {\mvclrrb\M} { target le } } } */
