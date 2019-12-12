/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power8" } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

/* Though the command line specifies power8 target, this function is
   to support power9.  */
__attribute__((target("cpu=power9")))
int get_random ()
{
  return __builtin_darn_32 ();
}

/* { dg-final { scan-assembler	   "darn" } } */
