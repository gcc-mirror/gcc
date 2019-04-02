/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8" } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */

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

vector unsigned char
test_vsi_packs_vusi_vusi (vector unsigned short x,
                          vector unsigned short y)
{
  return vec_packs (x, y);
}

vector signed char
test_vsi_packs_vssi_vssi (vector signed short x,
                          vector signed short y)
{
  return vec_packs (x, y);
}

vector signed short int
test_vsi_packs_vsi_vsi (vector signed int x,
			vector signed int y)
{
  return vec_packs (x, y);
}

vector unsigned short int
test_vsi_packs_vui_vui (vector unsigned int x,
			vector unsigned int y)
{
  return vec_packs (x, y);
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

vector unsigned char
test_vsi_packsu_vssi_vssi (vector signed short x,
			   vector signed short y)
{
  return vec_packsu (x, y);
}

vector unsigned char
test_vsi_packsu_vusi_vusi (vector unsigned short x,
			   vector unsigned short y)
{
  return vec_packsu (x, y);
}

vector unsigned int
test_vsi_packsu_vsll_vsll (vector signed long long x,
			   vector signed long long y)
{
  return vec_packsu (x, y);
}

vector unsigned int
test_vsi_packsu_vull_vull (vector unsigned long long x,
			   vector unsigned long long y)
{
  return vec_packsu (x, y);
}

vector unsigned short int
test_vsi_packsu_vsi_vsi (vector signed int x,
			 vector signed int y)
{
  return vec_packsu (x, y);
}

vector unsigned short int
test_vsi_packsu_vui_vui (vector unsigned int x,
			 vector unsigned int y)
{
  return vec_packsu (x, y);
}

/* Expected test results:

     test_eq_long_long          1 vcmpequd inst
     test_pack_float            1 vpkudum inst
     test_nabs_long_long        1 vspltisw, 1 vsubudm, 1 vminsd
     test_vsi_packs_vsll_vsll   1 vpksdss
     test_vui_packs_vull_vull   1 vpkudus
     test_vui_packs_vssi_vssi   1 vpkshss
     test_vsi_packsu_vssi_vssi  1 vpkshus */

/* { dg-final { scan-assembler-times "vcmpequd" 1 } } */
/* { dg-final { scan-assembler-times "vpkudum"  1 } } */
/* { dg-final { scan-assembler-times "vspltisw" 1 } } */
/* { dg-final { scan-assembler-times "vsubudm"  1 } } */
/* { dg-final { scan-assembler-times "vminsd"   1 } } */
/* { dg-final { scan-assembler-times "vpksdss"  1 } } */
/* { dg-final { scan-assembler-times "vpkudus"  2 } } */  
/* { dg-final { scan-assembler-times "vpkuhus"  2 } } */
/* { dg-final { scan-assembler-times "vpkshss"  1 } } */
/* { dg-final { scan-assembler-times "vpkshus"  1 } } */
/* { dg-final { scan-assembler-times "vpksdus"  1 } } */
/* { dg-final { scan-assembler-times "vpkuwus"  2 } } */
