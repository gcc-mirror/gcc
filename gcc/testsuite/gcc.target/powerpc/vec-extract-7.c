/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

#include <altivec.h>

float
add_float_0 (vector float *p)
{
  return vec_extract (*p, 0) + 1.0f;
}

float
add_float_1 (vector float *p)
{
  return vec_extract (*p, 1) + 1.0f;
}

float
add_float_2 (vector float *p)
{
  return vec_extract (*p, 2) + 1.0f;
}

float
add_float_3 (vector float *p)
{
  return vec_extract (*p, 3) + 1.0f;
}

float
add_float_n (vector float *p, long n)
{
  return vec_extract (*p, n) + 1.0f;
}

/* { dg-final { scan-assembler-not "lxvd2x"   } } */
/* { dg-final { scan-assembler-not "lxvw4x"   } } */
/* { dg-final { scan-assembler-not "lxvx"     } } */
/* { dg-final { scan-assembler-not "lxv"      } } */

/* With recent enhancements to the code generator, it is considered
 * legal to implement vec_extract with lvx and xxpermdi.  Previous
 * versions of this test forbid both instructions.  */
