/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

int
add_int_0 (vector int *p)
{
  return vec_extract (*p, 0) + 1;
}

int
add_int_1 (vector int *p)
{
  return vec_extract (*p, 1) + 1;
}

int
add_int_2 (vector int *p)
{
  return vec_extract (*p, 2) + 1;
}

int
add_int_3 (vector int *p)
{
  return vec_extract (*p, 3) + 1;
}

int
add_int_n (vector int *p, int n)
{
  return vec_extract (*p, n) + 1;
}

/* { dg-final { scan-assembler-not "lxvd2x"   } } */
/* { dg-final { scan-assembler-not "lxvw4x"   } } */
/* { dg-final { scan-assembler-not "lxvx"     } } */
/* { dg-final { scan-assembler-not "lxv"      } } */

/* With recent enhancements to the code generator, it is considered
 * legal to implement vec_extract with lvx and xxpermdi.  Previous
 * versions of this test forbid both instructions.  */
