/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

#include <stddef.h>
#include <altivec.h>

/* The vec_xst_len() function is not available on power8 configurations.  */

void
store_data (vector double *datap, double *address, size_t length)
{
  vector double data = *datap;

  __builtin_vec_stxvl (data, address, length); /* { dg-error "'__builtin_vec_stxvl' is not supported in this compiler configuration" } */
}
