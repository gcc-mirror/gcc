/* Test for the __builtin_altivec_vsumsws_be() builtin.
   It produces just the instruction vsumsws in LE and BE modes.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed int
test_vec_sums (vector signed int vsi2, vector signed int vsi3)
{
  return  __builtin_altivec_vsumsws_be (vsi2, vsi3);
}

/* { dg-final { scan-assembler-times "vsumsws" 1 } } */
