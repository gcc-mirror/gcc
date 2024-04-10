/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

__vector unsigned int
get_exponents (__vector float *p)
{
  __vector float source = *p;

  return vec_extract_exp (source);
}

/* { dg-final { scan-assembler "xvxexpsp" } } */
