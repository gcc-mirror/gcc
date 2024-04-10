/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

__vector unsigned long long int
get_significands (__vector double *p)
{
  __vector double source = *p;

  return vec_extract_sig (source);
}

/* { dg-final { scan-assembler "xvxsigdp" } } */
