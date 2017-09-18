/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

/* This test only runs on 32-bit configurations, producing a compiler
   error because the builtin requires 64 bits.  */
#include <altivec.h>

unsigned __int128 /* { dg-error "'__int128' is not supported on this target" } */
get_significand (__ieee128 *p)
{
  __ieee128 source = *p;

  return __builtin_vec_scalar_extract_sig (source); /* { dg-error "builtin function '__builtin_vec_scalar_extract_sig' not supported in this compiler configuration" } */
}
