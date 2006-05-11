/* Test vec_dst* functions with float pointer as first argument.  */
/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

extern int i;
extern float *fp;
extern vector float vf;

void
foo ()
{
  vec_dst (fp, i, 1);
  vec_dstst (fp, i, 1);
  vec_dststt (fp, i, 1);
  vec_dstt (fp, i, 1);
}
