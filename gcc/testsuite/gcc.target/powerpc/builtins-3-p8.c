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
test_unsigned_char_popcnt_signed_char (vector signed char x)
{
	return vec_popcnt (x);
}

vector unsigned char
test_unsigned_char_popcnt_unsigned_char (vector unsigned char x)
{
	return vec_popcnt (x);
}

vector unsigned short
test_unsigned_short_popcnt_signed_short (vector short x)
{
	return vec_popcnt (x);
}

vector unsigned short
test_unsigned_short_popcnt_unsigned_short (vector unsigned short x)
{
	return vec_popcnt (x);
}

vector unsigned int
test_unsigned_int_popcnt_signed_int (vector int x)
{
	return vec_popcnt (x);
}

vector unsigned int
test_unsigned_int_popcnt_unsigned_int (vector unsigned x)
{
   return vec_popcnt (x);
}

vector unsigned long long
test_unsigned_long_lont_popcnt_signed_long (vector long long x)
{
	return vec_popcnt (x);
}

vector unsigned long long
test_unsigned_long_long_popcnt_unsigned_long (vector unsigned long long x)
{
	return vec_popcnt (x);
}

vector signed short
test_vss_mradds_vss_vss (vector signed short x, vector signed short y,
                         vector signed short z)
{
	return vec_mradds (x, y, z);
}

vector signed long long
test_vsll_mule_vsi_vsi (vector signed int x, vector signed int y)
{
	return vec_mule (x, y);
}

vector unsigned long long
test_vull_mule_vui_vui (vector unsigned int x, vector unsigned int y)
{
	return vec_mule (x, y);
}

vector signed long long
test_vsll_mulo_vsi_vsi (vector signed int x, vector signed int y)
{
	return vec_mulo (x, y);
}

vector unsigned long long
test_vull_mulo_vui_vui (vector unsigned int x, vector unsigned int y)
{
	return vec_mulo (x, y);
}

/* Expected test results:

     test_eq_long_long                         1 vcmpequd inst
     test_pack_float                           1 vpkudum inst
     test_vsi_packs_vsll_vsll                  1 vpksdss
     test_vui_packs_vull_vull                  1 vpkudus
     test_unsigned_char_popcnt_signed_char     1 vpopcntb
     test_unsigned_char_popcnt_unsigned_char   1 vpopcntb
     test_unsigned_short_popcnt_signed_short   1 vpopcnth
     test_unsigned_short_popcnt_unsigned_short 1 vpopcnth
     test_unsigned_int_popcnt_signed_int       2 vpopcntw
     test_unsigned_int_popcnt_unsigned_int     1 vpopcntd
     test_unsigned_long_long_popcnt_unsigned_long 1 vpopcntd
     test_vss_mradds_vss_vsss                  1 vmhraddshs
     test_vsll_mulo_vsi_vsi                    1 vmulosw
     test_vull_mulo_vui_vui                    1 vmulouw
     test_vsll_mule_vsi_vsi                    1 vmulesw
     test_vull_mule_vui_vui                    1 vmuleuw
 */

/* { dg-final { scan-assembler-times "vcmpequd" 1 } } */
/* { dg-final { scan-assembler-times "vpkudum"  1 } } */
/* { dg-final { scan-assembler-times "vpksdss"  1 } } */
/* { dg-final { scan-assembler-times "vpkudus"  1 } } */  
/* { dg-final { scan-assembler-times "vpopcntb" 2 } } */
/* { dg-final { scan-assembler-times "vpopcnth" 2 } } */
/* { dg-final { scan-assembler-times "vpopcntw" 2 } } */
/* { dg-final { scan-assembler-times "vpopcntd" 2 } } */
/* { dg-final { scan-assembler-times "vmhraddshs"  1 } } */
/* { dg-final { scan-assembler-times "vmulosw"  1 } } */
/* { dg-final { scan-assembler-times "vmulouw"  1 } } */
/* { dg-final { scan-assembler-times "vmulesw"  1 } } */
/* { dg-final { scan-assembler-times "vmuleuw"  1 } } */
