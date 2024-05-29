/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target has_arch_ppc64 } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>

unsigned long long int
get_significand (double *p)
{
  double source = *p;

  return scalar_extract_sig (source);
}

/* { dg-final { scan-assembler "xsxsigdp" } } */
