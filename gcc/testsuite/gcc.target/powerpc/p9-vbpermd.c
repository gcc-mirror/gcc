/* { dg-do compile { target lp64 } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Verify P9 vector bit-permute doubleword instruction.  */

#include <altivec.h>

vector unsigned long long
test_vbpermd (vector unsigned long long a, vector unsigned char b)
{
  return vec_bperm (a, b);
}

/* { dg-final { scan-assembler "vbpermd" } } */
