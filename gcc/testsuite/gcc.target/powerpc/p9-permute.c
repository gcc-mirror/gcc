/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

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
