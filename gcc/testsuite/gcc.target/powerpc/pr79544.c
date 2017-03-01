/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2" } */

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

