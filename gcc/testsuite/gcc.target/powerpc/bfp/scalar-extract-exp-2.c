/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

/* This test only runs on 32-bit configurations, where a compiler error
   should be issued because this builtin is not available on 
   32-bit configurations.  */

#include <altivec.h>

unsigned int
get_exponent (double *p)
{
  double source = *p;

  return scalar_extract_exp (source);	/* { dg-error "builtin function '__builtin_vec_scalar_extract_exp' not supported in this compiler configuration" } */
}


