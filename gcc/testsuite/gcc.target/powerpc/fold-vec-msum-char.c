/* Verify that overloaded built-ins for vec_msum() with char inputs
   produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector unsigned int
test_msum_ui_uc_uc_ui (vector unsigned char vuc2, vector unsigned char vuc3,
		       vector unsigned int vui2)
{
  return vec_msum (vuc2, vuc3, vui2);
}

vector signed int
test_msum_si_sc_uc_si (vector signed char vsc2, vector unsigned char vuc3,
		       vector signed int vsi2)
{
  return vec_msum (vsc2, vuc3, vsi2);
}

/* { dg-final { scan-assembler-times "vmsumubm" 1 } } */
/* { dg-final { scan-assembler-times "vmsummbm" 1 } } */
