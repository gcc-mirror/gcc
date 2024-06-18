/* Verify that overloaded built-ins for vec_merge* with char
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed char 
test_misc () {
	vector signed char vsc1c = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
	vector signed char vsc2c = {9,10,11,12,13,14,15,16,0,1,2,3,4,5,6,7};
	return vec_mergel (vsc1c, vsc2c);
}

vector bool char
testbc_l (vector bool char vbc2, vector bool char vbc3)
{
  return vec_mergel (vbc2, vbc3);
}

vector signed char
testsc_l (vector signed char vsc2, vector signed char vsc3)
{
  return vec_mergel (vsc2, vsc3);
}

vector unsigned char
testuc_l (vector unsigned char vuc2, vector unsigned char vuc3)
{
  return vec_mergel (vuc2, vuc3);
}

vector bool char
testbc_h (vector bool char vbc2, vector bool char vbc3)
{
  return vec_mergeh (vbc2, vbc3);
}

vector signed char
testsc_h (vector signed char vsc2, vector signed char vsc3)
{
  return vec_mergeh (vsc2, vsc3);
}

vector unsigned char
testuc_h (vector unsigned char vuc2, vector unsigned char vuc3)
{
  return vec_mergeh (vuc2, vuc3);
}

/* { dg-final { scan-assembler-times "vmrghb" 3 } } */
/* { dg-final { scan-assembler-times "vmrglb" 3 } } */

