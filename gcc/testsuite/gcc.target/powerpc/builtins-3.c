/* { dg-do compile } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector bool char
test_eq_char (vector bool char x, vector bool char y)
{
	return vec_cmpeq (x, y);
}

vector bool short
test_eq_short (vector bool short x, vector bool short y)
{
	return vec_cmpeq (x, y);
}

vector bool int
test_eq_int (vector bool int x, vector bool int y)
{
	return vec_cmpeq (x, y);
}

vector signed char
test_nabs_char (vector signed char x)
{
	return vec_nabs (x);
}

vector short
test_nabs_short (vector short x)
{
  return vec_nabs (x);
}

vector int
test_nabs_int (vector int x)
{
  return vec_nabs (x);
}

vector signed char
test_sll_vsc_vsc_vsuc (vector signed char x, vector unsigned char y)
{
	return vec_sll (x, y);
}

vector unsigned char
test_sll_vuc_vuc_vuc (vector unsigned char x, vector unsigned char y)
{
	return vec_sll (x, y);
}

vector signed int
test_sll_vsi_vsi_vuc (vector signed int x, vector unsigned char y)
{
	return vec_sll (x, y);
}

vector unsigned int
test_sll_vui_vui_vuc (vector unsigned int x, vector unsigned char y)
{
	return vec_sll (x, y);
}

vector pixel
test_sll_vp_vp_vuc (vector pixel x, vector unsigned char y)
{
	return vec_sll (x, y);
}

vector signed short int
test_sll_vssi_vssi_vuc (vector signed short x, vector unsigned char y)
{
	return vec_sll (x, y);
}

vector unsigned short int
test_sll_vusi_vusi_vuc (vector unsigned short x, vector unsigned char y)
{
	return vec_sll (x, y);
}

vector signed char
test_slo_vsc_vsc_vsc (vector signed char x, vector signed char y)
{
	return vec_slo (x, y);
}

vector signed char
test_slo_vsc_vsc_vuc (vector signed char x, vector unsigned char y)
{
	return vec_slo (x, y);
}

vector unsigned char
test_slo_vuc_vuc_vsc (vector unsigned char x, vector signed char y)
{
	return vec_slo (x, y);
}

vector unsigned char
test_slo_vuc_vuc_vuc (vector unsigned char x, vector unsigned char y)
{
	return vec_slo (x, y);
}

vector signed int
test_slo_vsi_vsi_vsc (vector signed int x, vector signed char y)
{
	return vec_slo (x, y);
}

vector signed int
test_slo_vsi_vsi_vuc (vector signed int x, vector unsigned char y)
{
	return vec_slo (x, y);
}

vector unsigned int
test_slo_vui_vui_vsc (vector unsigned int x, vector signed char y)
{
	return vec_slo (x, y);
}

vector unsigned int
test_slo_vui_vui_vuc (vector unsigned int x, vector unsigned char y)
{
	return vec_slo (x, y);
}


vector pixel
test_slo_vp_vp_vsc (vector pixel int x, vector signed char y)
{
	return vec_slo (x, y);
}

vector pixel
test_slo_vp_vp_vuc (vector pixel int x, vector unsigned char y)
{
	return vec_slo (x, y);
}

vector signed short int
test_slo_vssi_vssi_vsc (vector signed short int x, vector signed char y)
{
	return vec_slo (x, y);
}

vector signed short int
test_slo_vssi_vssi_vuc (vector signed short int x, vector unsigned char y)
{
	return vec_slo (x, y);
}

vector unsigned short int
test_slo_vusi_vusi_vsc (vector unsigned short int x, vector signed char y)
{
	return vec_slo (x, y);
}

vector unsigned short int
test_slo_vusi_vusi_vuc (vector unsigned short int x, vector unsigned char y)
{
	return vec_slo (x, y);
}

vector float
test_slo_vf_vf_vsc (vector float x, vector signed char y)
{
	return vec_slo (x, y);
}

vector float
test_slo_vf_vf_vuc (vector float x, vector unsigned char y)
{
	return vec_slo (x, y);
}

vector int
test_cmpb_float (vector float x, vector float y)
{
	return vec_cmpb (x, y);
}

/* Expected test results:

     test_eq_char              1 vcmpequb inst
     test_eq_short             1 vcmpequh inst
     test_eq_int               1 vcmpequw inst
     test_nabs_char            1 vsububm, 1 vminsb
     test_nabs_short           1 vsubuhm, 1 vminsh
     test_nabs_int             1 vsubuwm, 1 vminsw
     test_sll_vsc_vsc_vsuc     1 vsl
     test_sll_vuc_vuc_vuc      1 vsl
     test_sll_vsi_vsi_vuc      1 vsl
     test_sll_vui_vui_vuc      1 vsl
     test_sll_vp_vp_vuc        1 vsl
     test_sll_vssi_vssi_vuc    1 vsl
     test_sll_vusi_vusi_vuc    1 vsl
     test_slo_vsc_vsc_vsc      1 vslo
     test_slo_vsc_vsc_vuc      1 vslo
     test_slo_vuc_vuc_vsc      1 vslo
     test_slo_vuc_vuc_vuc      1 vslo
     test_slo_vsi_vsi_vsc      1 vslo
     test_slo_vsi_vsi_vuc      1 vslo
     test_slo_vui_vui_vsc      1 vslo
     test_slo_vui_vui_vuc      1 vslo
     test_slo_vp_vp_vsc        1 vslo
     test_slo_vp_vp_vuc        1 vslo
     test_slo_vssi_vssi_vsc    1 vslo
     test_slo_vssi_vssi_vuc    1 vslo
     test_slo_vusi_vusi_vsc    1 vslo
     test_slo_vusi_vusi_vuc    1 vslo
     test_slo_vf_vf_vsc        1 vslo
     test_slo_vf_vf_vuc        1 vslo
     test_cmpb_float           1 vcmpbfp */

/* { dg-final { scan-assembler-times "vcmpequb" 1 } } */
/* { dg-final { scan-assembler-times "vcmpequh" 1 } } */
/* { dg-final { scan-assembler-times "vcmpequw" 1 } } */
/* { dg-final { scan-assembler-times "vsububm"  1 } } */
/* { dg-final { scan-assembler-times "vsubuhm"  1 } } */
/* { dg-final { scan-assembler-times "vsubuwm"  1 } } */
/* { dg-final { scan-assembler-times "vminsb"   1 } } */
/* { dg-final { scan-assembler-times "vminsh"   1 } } */
/* { dg-final { scan-assembler-times "vminsw"   1 } } */
/* { dg-final { scan-assembler-times "vslo"    16 } } */
/* { dg-final { scan-assembler-times "vcmpbfp"  1 } } */
/* { dg-final { scan-assembler-times "vsl"     23 } } */
