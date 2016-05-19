/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mcpu=power9" } */
/* { dg-require-effective-target lp64 } */

#include <altivec.h>

long long get_conditioned_random ()
{
  return __builtin_darn ();
}

/* { dg-final { scan-assembler	   "darn" } } */
