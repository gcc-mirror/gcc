/* Verify that overloaded built-ins for vec_abs with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2 -fwrapv" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed short
test3 (vector signed short x)
{
  return vec_abs (x);
}

/* { dg-final { scan-assembler-times "vspltisw|xxspltib|vxor" 1 } } */
/* { dg-final { scan-assembler-times "vsubuhm" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsh" 1 } } */
