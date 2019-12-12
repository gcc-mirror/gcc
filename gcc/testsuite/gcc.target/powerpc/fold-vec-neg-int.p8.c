/* Verify that overloaded built-ins for vec_neg with int
   inputs produce the right code when -mcpu=power8 is specified.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

#include <altivec.h>

vector signed int
test1 (vector signed int x)
{
  return vec_neg (x);
}

/* { dg-final { scan-assembler-times "xxspltib|vspltisw|vxor" 1 } } */
/* { dg-final { scan-assembler-times "vsubuwm" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsw" 0 } } */
