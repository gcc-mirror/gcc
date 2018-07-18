/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

/* This test only runs on 32-bit configurations, where a compiler error
   should be issued because this builtin is not available on 
   32-bit configurations.  */

#include <altivec.h>

double
insert_exponent (unsigned long long int *significand_p,
		 unsigned long long int *exponent_p)
{
  unsigned long long int significand = *significand_p;
  unsigned long long int exponent = *exponent_p;

  return scalar_insert_exp (significand, exponent); /* { dg-error "builtin function '__builtin_vec_scalar_insert_exp' not supported in this compiler configuration" } */
}
