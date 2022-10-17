/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>

long get_conditioned_random ()
{
  return __builtin_darn ();
}

/* { dg-final { scan-assembler	   "darn" } } */
