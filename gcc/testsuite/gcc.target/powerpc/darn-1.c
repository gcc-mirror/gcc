/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>

long long get_conditioned_random ()
{
  return __builtin_darn ();
}

/* { dg-final { scan-assembler	   "darn" } } */
