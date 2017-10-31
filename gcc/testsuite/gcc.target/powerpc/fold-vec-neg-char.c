/* Verify that overloaded built-ins for vec_neg with char
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector signed char
test2 (vector signed char x)
{
  return vec_neg (x);
}

/* { dg-final { scan-assembler-times "xxspltib|vspltisw|vxor" 1 } } */
/* { dg-final { scan-assembler-times "vsububm" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsb" 0 } } */

