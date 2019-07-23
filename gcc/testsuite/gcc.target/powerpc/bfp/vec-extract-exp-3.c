/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

#include <altivec.h>

__vector unsigned int
get_exponents (__vector float *p)
{
  __vector float source = *p;

  return __builtin_vec_extract_exp (source);	/* { dg-error "'__builtin_vsx_extract_exp_sp' requires" } */
}
