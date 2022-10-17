/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-avx512f" } */

#include<complex.h>

_Complex _Float16
foo (_Complex _Float16 x)
{
  return x;
}

/* { dg-final { scan-assembler {(?n)movd[\t ].*%xmm0} } } */
