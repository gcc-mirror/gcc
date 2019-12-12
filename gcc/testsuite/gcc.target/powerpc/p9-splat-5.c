/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */
/* { dg-final { scan-assembler "vspltish" } } */
/* { dg-final { scan-assembler-not "xxspltib" } } */

/* Make sure we don't use an inefficient sequence for small integer splat.  */

#include <altivec.h>

vector short
foo ()
{
  return vec_splat_s16 (5);
}
