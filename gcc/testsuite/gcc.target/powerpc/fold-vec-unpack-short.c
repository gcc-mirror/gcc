/* Verify that overloaded built-ins for vec_unpackh and vec_unpackl with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector bool int
testbi_el (vector bool short vbs2)
{
  return vec_unpackl (vbs2);
}

vector signed int
testsi_el (vector signed short vss2)
{
  return vec_unpackl (vss2);
}

vector bool int
testbi_eh (vector bool short vbs2)
{
  return vec_unpackh (vbs2);
}

vector signed int
testsi_eh (vector signed short vss2)
{
  return vec_unpackh (vss2);
}

/* { dg-final { scan-assembler-times "vupkhsh" 2 } } */
/* { dg-final { scan-assembler-times "vupklsh" 2 } } */

