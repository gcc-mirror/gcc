/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_fp } */

/* Reset fpu to a value compatible with the next pragmas.  */
#pragma GCC target ("fpu=vfp")
#pragma GCC push_options

#pragma GCC target ("fpu=neon")
#include <arm_neon.h>

/* Check that pragma target is used.  */
int8x8_t 
my (int8x8_t __a, int8x8_t __b)
{
  return __a + __b;
}

#pragma GCC pop_options

/* Check that command line option is restored.  */
int8x8_t 
my1 (int8x8_t __a, int8x8_t __b)
{
  return __a + __b;
}

/* { dg-final { scan-assembler-times "\.fpu vfp" 1 } } */
/* { dg-final { scan-assembler-times "\.fpu neon" 1 } } */
/* { dg-final { scan-assembler "vadd" } } */


