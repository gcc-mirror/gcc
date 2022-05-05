/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx" } */

#include <altivec.h>

vector double
test_shift_left_double (vector double x, vector double y)
{
	return vec_sld (x, y, /* shift_by */ 10);
}

vector float
test_nabs_float (vector float x)
{
  return vec_nabs (x);
}

vector double
test_nabs_double (vector double x)
{
	return vec_nabs (x);
}

vector bool long long
test_sll_vbll_vbll_vuc (vector bool long long x,
			vector unsigned char y)
{
	return vec_sll (x, y);
}

vector bool long long
test_sll_vbll_vbll_vull (vector bool long long x,
			vector unsigned long long y)
{
	return vec_sll (x, y);
}

vector bool long long
test_sll_vbll_vbll_vus (vector bool long long x,
			vector unsigned short y)
{
	return vec_sll (x, y);
}

vector signed long long
test_slo_vsll_slo_vsll_vsc (vector signed long long x, vector signed char y)
{
	return vec_slo (x, y);
}

vector signed long long
test_slo_vsll_slo_vsll_vuc (vector signed long long x, vector unsigned char y)
{
	return vec_slo (x, y);
}

vector unsigned long long
test_slo_vull_slo_vull_vsc (vector unsigned long long x, vector signed char y)
{
	return vec_slo (x, y);
}

vector unsigned long long
test_slo_vull_slo_vull_vuc (vector unsigned long long x,
			    vector unsigned char y)
{
	return vec_slo (x, y);
}

vector signed char
test_vsc_sldw_vsc_vsc (vector signed char x, vector signed char y)
{
	return vec_sldw (x, y, 1);
}

vector unsigned char
test_vuc_sldw_vuc_vuc (vector unsigned char x, vector unsigned char y)
{
	return vec_sldw (x, y, 3);
}

vector signed short int
test_vssi_sldw_vssi_vssi (vector signed short int x,
                          vector signed short int y)
{
	return vec_sldw (x, y, 1);
}

vector unsigned short int
test_vusi_sldw_vusi_vusi (vector unsigned short int x,
                          vector unsigned short int y)
{
	return vec_sldw (x, y, 3);
}

vector signed int
test_vsi_sldw_vsi_vsi (vector signed int x, vector signed int y)
{
	return vec_sldw (x, y, 1);
}

vector unsigned int
test_vui_sldw_vui_vui (vector unsigned int x, vector unsigned int y)
{
	return vec_sldw (x, y, 3);
}

vector signed long long
test_vsl_sldw_vsl_vsl (vector signed long long x, vector signed long long y)
{
	return vec_sldw (x, y, 1);
}

vector unsigned long long
test_vul_sldw_vul_vul (vector unsigned long long x,
                       vector unsigned long long y)
{
	return vec_sldw (x, y, 3);
}

vector float
test_vf_sldw_vf_vf (vector float x, vector float y)
{
  return vec_sldw (x, y, 3);
}

vector double
test_vd_sldw_vd_vd (vector double x, vector double y)
{
  return vec_sldw (x, y, 1);
}

vector signed int long long
test_sll_vsill_vsill_vuc (vector signed long long int x,
			  vector unsigned char y)
{
	return vec_sll (x, y);
}

vector unsigned int long long
test_sll_vuill_vuill_vuc (vector unsigned long long int x,
			  vector unsigned char y)
{
	return vec_sll (x, y);
}


/* Expected test results:

     test_shift_left_double        1 vsldoi
     test_nabs_float               1 xvnabssp
     test_nabs_double              1 xvnabsdp
     test_sll_vbll_vbll_vuc        1 vsl
     test_sll_vbll_vbll_vull       1 vsl
     test_sll_vbll_vbll_vus        1 vsl
     test_slo_vsll_slo_vsll_vsc    1 vslo
     test_slo_vsll_slo_vsll_vuc    1 vslo
     test_slo_vull_slo_vull_vsc    1 vslo
     test_slo_vull_slo_vull_vuc    1 vslo
     test_vsc_sldw_vsc_vsc         1 xxlor, 1 xxsldwi
     test_vuc_sldw_vuc_vuc         1 xxlor, 1 xxsldwi
     test_vssi_sldw_vssi_vssi      1 xxlor, 1 xxsldwi
     test_vusi_sldw_vusi_vusi      1 xxlor, 1 xxsldwi
     test_vsi_sldw_vsi_vsi         1 xxlor, 1 xxsldwi
     test_vui_sldw_vui_vui         1 xxlor, 1 xxsldwi
     test_vsl_sldw_vsl_vsl         1 xxlor, 1 xxsldwi
     test_vul_sldw_vul_vul         1 xxlor, 1 xxsldwi
     test_vf_sldw_vf_vf            1 xxlor, 1 xxsldwi
     test_vd_sldw_vd_vd            1 xxlor, 1 xxsldwi
     test_sll_vsill_vsill_vuc      1 vsl
     test_sll_vuill_vuill_vuc      1 vsl  */

/* { dg-final { scan-assembler-times "vsldoi"    1 } } */
/* { dg-final { scan-assembler-times "xvnabssp"  1 } } */
/* { dg-final { scan-assembler-times "xvnabsdp"  1 } } */
/* { dg-final { scan-assembler-times "vslo"      4 } } */
/* { dg-final { scan-assembler-times "xxlor"     32 } } */
/* { dg-final { scan-assembler-times {\mvsl\M}   5 } } */
/* { dg-final { scan-assembler-times {\mxxsldwi\M} 10 } } */
