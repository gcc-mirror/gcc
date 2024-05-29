/* Verify that overloaded built-ins for vec_cntlz with long long
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed long long
testsl (vector signed long long vsl2)
{
  return vec_cntlz (vsl2);
}

vector unsigned long long
testul (vector unsigned long long vul2)
{
  return vec_cntlz (vul2);
}

/* { dg-final { scan-assembler-times "vclzd" 2 } } */
