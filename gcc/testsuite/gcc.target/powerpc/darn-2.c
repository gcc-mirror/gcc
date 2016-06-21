/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>

long long get_raw_random ()
{
  return __builtin_darn_raw ();
}

/* { dg-final { scan-assembler	   "darn" } } */
