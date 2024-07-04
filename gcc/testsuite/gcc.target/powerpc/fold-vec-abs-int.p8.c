/* Verify that overloaded built-ins for vec_abs with int
   inputs produce the right code when -mcpu=power8 is specified.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2 -mdejagnu-cpu=power8" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed int
test1 (vector signed int x)
{
  return vec_abs (x);
}

/* { dg-final { scan-assembler-times "vspltisw|vxor" 1 } } */
/* { dg-final { scan-assembler-times "vsubuwm" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsw" 1 } } */

