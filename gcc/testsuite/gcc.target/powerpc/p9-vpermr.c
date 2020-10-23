/* { dg-do compile { target le } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* Test generation of VPERMR/XXPERMR on ISA 3.0 in little endian.  */

#include <altivec.h>

vector long long
permute (vector long long *p, vector long long *q, vector unsigned char mask)
{
  vector long long a = *p;
  vector long long b = *q;

  /* Force a, b to be in altivec registers to select vpermr insn.  */
  __asm__ (" # a: %x0, b: %x1" : "+v" (a), "+v" (b));

  return vec_perm (a, b, mask);
}

/* { dg-final { scan-assembler "vpermr\|xxpermr" } } */
