/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

__vector unsigned int
get_exponents (__vector float *p)
{
  __vector float source = *p;

  return vec_extract_exp (source);
}

/* { dg-final { scan-assembler "xvxexpsp" } } */
