/* Verify that overloaded built-ins for vec_unpackh and vec_unpackl with char
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector bool short
testbc_l (vector bool char vbc2)
{
  return vec_unpackl (vbc2);
}

vector signed short
testsc_l (vector signed char vsc2)
{
  return vec_unpackl (vsc2);
}

vector bool short
testbc_h (vector bool char vbc2)
{
  return vec_unpackh (vbc2);
}

vector signed short
testsc_h (vector signed char vsc2)
{
  return vec_unpackh (vsc2);
}

/* { dg-final { scan-assembler-times "vupkhsb" 2 } } */
/* { dg-final { scan-assembler-times "vupklsb" 2 } } */

