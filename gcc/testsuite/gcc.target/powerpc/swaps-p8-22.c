/* { dg-do compile { target { *-*-aix* || { *-*-linux* && lp64 } } } } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx -maltivec -mcmodel=large" } */
/* { dg-require-effective-target powerpc_vsx } */

/* The expansion for vector character multiply introduces a vperm operation.
   This tests that changing the vperm mask allows us to remove all swaps
   from the generated code.  It is a duplicate of swaps-p8-21.c, except
   that it applies the large code model, which requires an extra indirection
   in the load of the constant mask.  */

#include <altivec.h>

void abort ();

vector unsigned char r;
vector unsigned char v =
  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
vector unsigned char i =
  { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

int main ()
{
  int j;
  r = v * i;
  return 0;
}

/* { dg-final { scan-assembler-times "vperm" 1 } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */
