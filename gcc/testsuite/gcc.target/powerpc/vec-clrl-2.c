/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

/* Vector string clear left-most bytes of unsigned char.  */
vector signed char
clrl (vector signed char arg, int n)
{
  return vec_clrl (arg, n);
}

/* { dg-final { scan-assembler {\mvclrlb\M} { target be } } } */
/* { dg-final { scan-assembler {\mvclrrb\M} { target le } } } */
