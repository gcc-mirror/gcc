/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O2" } */

#include <altivec.h>

/* PR 78953: mem = vec_extract (V4SI, <n>) failed if the vector was in a
   traditional FPR register.  */

void
foo (vector int *vp, int *ip)
{
  vector int v = *vp;
  __asm__ (" # fpr %x0" : "+d" (v));
  ip[4] = vec_extract (v, 0);
}

/* { dg-final { scan-assembler {\mxxextractuw\M|\mvextuw[lr]x\M} } } */
