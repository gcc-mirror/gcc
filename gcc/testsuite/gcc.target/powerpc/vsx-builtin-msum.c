/* Verify that overloaded built-ins for vec_msum with __int128
   inputs generate the proper code.  */

/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx  -O3" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target int128 } */

#include <altivec.h>

vector signed __int128
test_msum_si (vector signed long long vsll_1, vector signed long long vsll_2,
	   vector signed __int128 vsi128)
{
  return vec_msum (vsll_1, vsll_2, vsi128);
}

vector unsigned __int128
test_msum_ui (vector unsigned long long vull_1, vector unsigned long long vull_2,
	   vector unsigned __int128 vui128)
{
  return vec_msum (vull_1, vull_2, vui128);
}

/* { dg-final { scan-assembler-times "vmsumudm" 2 } } */

