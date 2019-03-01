/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* This test only runs on 32-bit configurations, producing a compiler
   error because the builtin requires 64 bits.  */
#include <altivec.h>

unsigned long long int
get_significand (double *p)
{
  double source = *p;

  return __builtin_vec_scalar_extract_sig (source); /* { dg-error "builtin function '__builtin_vec_scalar_extract_sig' not supported in this compiler configuration" } */
}
