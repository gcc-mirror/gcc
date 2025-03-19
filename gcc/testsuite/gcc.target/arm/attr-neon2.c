/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_h_ok } */
/* { dg-options "-Ofast" } */
/* { dg-add-options arm_neon_h } */

/* Reset fpu to a value compatible with the next pragmas.  */
#pragma GCC target ("fpu=vfp")
#pragma GCC push_options

#pragma GCC target ("fpu=neon")
#include <arm_neon.h>

/* Check that pragma target is used.  */
/*
**my:
**	...
**	vadd.f32	d[0-9]+, d[0-9]+, d[0-9]+
**	...
**	bx	lr
*/
float32x2_t
my (float32x2_t __a, float32x2_t __b)
{
  return __a + __b;
}

#pragma GCC pop_options

/* Check that fpu=vfp is restored.  */
/*
**my1:
**	...
**	vadd.f32	s[0-9]+, s[0-9]+, s[0-9]+
**	vadd.f32	s[0-9]+, s[0-9]+, s[0-9]+
**	...
**	bx	lr
*/
float32x2_t
my1 (float32x2_t __a, float32x2_t __b)
{
  return __a + __b;
}

/* { dg-final { scan-assembler "\.fpu\\s+vfp\n" } } */
/* { dg-final { scan-assembler "\.fpu\\s+neon\n" } } */
/* { dg-final { check-function-bodies "**" "" } } */
