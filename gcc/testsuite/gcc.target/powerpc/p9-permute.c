/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */
/* { dg-require-effective-target powerpc_p9vector_ok } */

#include <altivec.h>

vector long long
permute (vector long long *p, vector long long *q, vector unsigned char mask)
{
  vector long long a = *p;
  vector long long b = *q;

  /* Force a, b to be in FPR registers.  */
  __asm__ (" # a: %x0, b: %x1" : "+d" (a), "+d" (b));

  return vec_perm (a, b, mask);
}

/* expect xxpermr on little-endian, xxperm on big-endian */
/* { dg-final { scan-assembler	   "xxperm" } } */
/* { dg-final { scan-assembler-not "vperm"  } } */
