/* Verify that overloaded built-ins for vec_cntlz with int
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed short
testsi (vector signed short vss2)
{
  return vec_cntlz (vss2);
}

vector unsigned short
testui (vector unsigned short vus2)
{
  return vec_cntlz (vus2);
}

/* { dg-final { scan-assembler-times "vclzh" 2 } } */
