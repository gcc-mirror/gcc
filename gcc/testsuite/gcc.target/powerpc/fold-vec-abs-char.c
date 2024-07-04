/* Verify that overloaded built-ins for vec_abs with char
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed char
test2 (vector signed char x)
{
  return vec_abs (x);
}

/* { dg-final { scan-assembler-times "vspltisw|vxor|xxspltib" 1 } } */
/* { dg-final { scan-assembler-times "vsububm" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsb" 1 } } */
