/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* This test only runs on 32-bit configurations, where a compiler error
   should be issued because this builtin is not available on
   32-bit configurations.  */

#include <altivec.h>

__ieee128
insert_exponent (__ieee128 *significand_p,
		 unsigned long long int *exponent_p)
{
  __ieee128 significand = *significand_p;
  unsigned long long int exponent = *exponent_p;

  return scalar_insert_exp (significand, exponent); /* { dg-error "requires ISA 3.0 IEEE 128-bit floating point" } */
}
