/* { dg-do compile { target { powerpc*-*-* && vmx_hw } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

vector unsigned char vmului(vector unsigned char v,
			    vector unsigned char i)
{
	return v * i;
}

vector signed char vmulsi(vector signed char v,
			  vector signed char i)
{
	return v * i;
}

/* { dg-final { scan-assembler-times "vmulesb" 2 } } */
/* { dg-final { scan-assembler-times "vmulosb" 2 } } */
/* { dg-final { scan-assembler-times {\m(?:v|xx)permr?\M} 2 } } */
