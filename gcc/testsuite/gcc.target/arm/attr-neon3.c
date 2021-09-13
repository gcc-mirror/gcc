/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-options "-Ofast" } */
/* { dg-add-options arm_fp } */

/* Reset fpu to a value compatible with the next pragmas.  */
#pragma GCC target ("fpu=vfp")
#pragma GCC push_options

#include <arm_neon.h>

/* Check that neon is used.  */
/*
**my:
**	...
**	vadd.f32	d[0-9]+, d[0-9]+, d[0-9]+
**	...
**	bx	lr
*/
float32x2_t __attribute__ ((target("fpu=neon")))
my (float32x2_t __a, float32x2_t __b)
{
  return __a + __b;
}

/* Check that crypto builtins are recognized.  */
/*
**foo:
**	...
** (
**	vld1.64	{d[0-9]+-d[0-9]+}, \[r[0-9]+:64\]
** |
**	vld1.64	{d[0-9]+}, \[r[0-9]+:64\]!
**	vld1.64	{d[0-9]+}, \[r[0-9]+:64\]
** )
**	...
**	bx	lr
*/

poly128_t __attribute__ ((target("fpu=crypto-neon-fp-armv8")))
foo (poly128_t* ptr)
{
  return vldrq_p128 (ptr);
}

/* Check that fpu=vfp is restored.  */
/*
**my1:
**	...
**	vadd.f32	s[0-9]+, s[0-9]+, s[0-9]+
**	vadd.f32	s[0-9]+, s[0-9]+, s[0-9]+
**	...
**	bx	lr
*/float32x2_t
my1 (float32x2_t __a, float32x2_t __b)
{
  return __a + __b;
}

/* { dg-final { scan-assembler "\.fpu\\s+vfp\n" } } */
/* { dg-final { scan-assembler "\.fpu\\s+neon\n" } } */
/* { dg-final { scan-assembler "\.fpu\\s+crypto-neon-fp-armv8\n" } } */
/* { dg-final { check-function-bodies "**" "" } } */
