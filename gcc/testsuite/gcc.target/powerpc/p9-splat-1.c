/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */
/* { dg-require-effective-target powerpc_p9vector_ok } */

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
