/* Verify that overloaded built-ins for vec_merge* with long long
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include <altivec.h>

vector bool long long
testbl_l (vector bool long long vbl2, vector bool long long vbl3)
{
  return vec_mergel (vbl2, vbl3);
}

vector signed long long
testsl_l (vector signed long long vsl2, vector signed long long vsl3)
{
  return vec_mergel (vsl2, vsl3);
}

vector unsigned long long
testul_l (vector unsigned long long vul2, vector unsigned long long vul3)
{
  return vec_mergel (vul2, vul3);
}

vector bool long long
testbl_h (vector bool long long vbl2, vector bool long long vbl3)
{
  return vec_mergeh (vbl2, vbl3);
}

vector signed long long
testsl_h (vector signed long long vsl2, vector signed long long vsl3)
{
  return vec_mergeh (vsl2, vsl3);
}

vector unsigned long long
testul_h (vector unsigned long long vul2, vector unsigned long long vul3)
{
  return vec_mergeh (vul2, vul3);
}

/* mergeh with longlong types use xxpermdi (1 ea).  */
/* { dg-final { scan-assembler-times "xxpermdi" 6 } } */

