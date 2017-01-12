/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3 " } */
/* { dg-final { scan-assembler-times "lxvw4x" 2 } } */
/* { dg-final { scan-assembler "stxvw4x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

/* Verify that swap optimization does not interfere with element-reversing
   loads and stores.  */

/* Test case to resolve PR79044.  */

#include <altivec.h>

void pr79044 (float *x, float *y, float *z)
{
  vector float a = __builtin_vec_xl (0, x);
  vector float b = __builtin_vec_xl (0, y);
  vector float c = __builtin_vec_mul (a, b);
  __builtin_vec_xst (c, 0, z);
}
