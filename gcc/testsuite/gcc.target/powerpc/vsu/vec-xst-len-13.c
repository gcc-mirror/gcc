/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

#include <stddef.h>
#include <altivec.h>

/* This test only runs on 32-bit configurations, where a compiler
   error should be issued because this built-in function is not
   available on 32-bit configurations. */
void
store_data (vector double *datap, double *address, size_t length)
{
  vector double data = *datap;

  __builtin_vec_stxvl (data, address, length); /* { dg-error "Builtin function __builtin_altivec_stxvl requires" } */
}
