/* { dg-do compile { target lp64 } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector int
foo_r (int a)
{
  return (vector int) { a, a, a, a };		/* mtvsrws */
}

vector int
foo_r2 (int a)
{
  return vec_splats (a);			/* mtvsrws */
}

vector int
foo_p (int *a)
{
  return (vector int) { *a, *a, *a, *a };	/* lxvwsx */
}

/* { dg-final { scan-assembler-times "mtvsrws" 2 } } */
/* { dg-final { scan-assembler-times "lxvwsx"  1 } } */
