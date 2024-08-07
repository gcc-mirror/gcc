/* Verify that overloaded built-ins for vec_sel with float
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector float
test1_0 (vector float x, vector float y, vector bool int z)
{
  return vec_sel (x, y, z);
}

vector float
test1_1 (vector float x, vector float y, vector unsigned int z)
{
  return vec_sel (x, y, z);
}

/* { dg-final { scan-assembler-times {\mxxsel\M|\mvsel\M} 2 } } */
