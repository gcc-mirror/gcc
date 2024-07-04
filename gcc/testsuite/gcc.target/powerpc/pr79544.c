/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector unsigned long long
test_sra (vector unsigned long long x, vector unsigned long long y)
{
  return vec_sra (x, y);
}

vector unsigned long long
test_vsrad (vector unsigned long long x, vector unsigned long long y)
{
  return vec_vsrad (x, y);
}

/* { dg-final { scan-assembler-times {\mvsrad\M} 2 } } */

