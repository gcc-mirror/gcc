/* Verify that overloaded built-ins for vec_perm with long long
   inputs produce the right code.  */

/* { dg-do compile {target lp64} } */
// 'long long' in Altivec types is invalid without -mvsx.
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector bool long long
testbl (vector bool long long vbl2, vector bool long long vbl3,
	vector unsigned char vuc)
{
  return vec_perm (vbl2, vbl3, vuc);
}

vector signed long long
testsl (vector signed long long vsl2, vector signed long long vsl3,
	vector unsigned char vuc)
{
  return vec_perm (vsl2, vsl3, vuc);
}

vector unsigned long long
testul (vector unsigned long long vul2, vector unsigned long long vul3,
	vector unsigned char vuc)
{
  return vec_perm (vul2, vul3, vuc);
}

/* { dg-final { scan-assembler-times "vperm" 3 } } */
