/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

#include <altivec.h>

double
add_double_n (vector double *p, double x, long n)
{
  return vec_extract (*p, n) + x;
}

long
add_long_n (vector long *p, long x, long n)
{
  return vec_extract (*p, n) + x;
}

/* { dg-final { scan-assembler-not "lxvd2x"   } } */
/* { dg-final { scan-assembler-not "lxvw4x"   } } */
/* { dg-final { scan-assembler-not "lxvx"     } } */
/* { dg-final { scan-assembler-not "lxv"      } } */

/* With recent enhancements to the code generator, it is considered
 * legal to implement vec_extract with lvx and xxpermdi.  Previous
 * versions of this test forbid both instructions.  */
