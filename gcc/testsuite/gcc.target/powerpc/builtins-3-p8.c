/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8" } */

#include <altivec.h>

vector bool long long
test_eq_long_long (vector bool long long x, vector bool long long y)
{
	return vec_cmpeq (x, y);
}

vector float
test_pack_float (vector double x, vector double y)
{
  return vec_pack (x, y);
}

vector long long
test_nabs_long_long (vector long long x)
{
  return vec_nabs (x);
}

vector signed int
test_vsi_packs_vsll_vsll (vector signed long long x,
                          vector signed long long y)
{
  return vec_packs (x, y);
}

vector unsigned int
test_vui_packs_vull_vull (vector unsigned long long x,
                          vector unsigned long long y)
{
  return vec_packs (x, y);
}

/* Expected test results:

     test_eq_long_long          1 vcmpequd inst
     test_pack_float            1 vpkudum inst
     test_nabs_long_long        1 vspltisw, 1 vsubudm, 1 vminsd
     test_vsi_packs_vsll_vsll   1 vpksdss
     test_vui_packs_vull_vull   1 vpkudus */

/* { dg-final { scan-assembler-times "vcmpequd" 1 } } */
/* { dg-final { scan-assembler-times "vpkudum"  1 } } */
/* { dg-final { scan-assembler-times "vspltisw" 1 } } */
/* { dg-final { scan-assembler-times "vsubudm"  1 } } */
/* { dg-final { scan-assembler-times "vminsd"   1 } } */
/* { dg-final { scan-assembler-times "vpksdss"  1 } } */
/* { dg-final { scan-assembler-times "vpkudus"  1 } } */  
