/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>

unsigned long long int
get_significand (double *p)
{
  double source = *p;

  return scalar_extract_sig (source);
}

/* { dg-final { scan-assembler "xsxsigdp" } } */
