/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mcpu=power9" } */
/* { dg-require-effective-target lp64 } */

#include <altivec.h>

long long get_raw_random ()
{
  return __builtin_darn_raw ();
}

/* { dg-final { scan-assembler	   "darn" } } */
