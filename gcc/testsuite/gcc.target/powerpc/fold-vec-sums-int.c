/* Verify that overloaded built-ins for vec_sums with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed int
test_vec_sums (vector signed int vsi2, vector signed int vsi3)
{
  return vec_sums (vsi2, vsi3);
}

/* { dg-final { scan-assembler-times "vsumsws" 1 } } */
