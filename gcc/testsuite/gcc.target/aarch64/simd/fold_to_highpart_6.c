/* { dg-do compile } */
/* { dg-require-effective-target aarch64_little_endian } */
/* { dg-options "-O2 -march=armv8-a+sve" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_neon_sve_bridge.h>

typedef int16_t int16x16_t __attribute__ ((vector_size (32)));

/* Edge cases where we don't/can't fold, reject these gracefully.  */

int8x16_t z;

/*
** test_addressable:
**	adrp	x([0-9]+), \.LANCHOR0\+8
**	ldr	d([0-9]+), \[x\1, #?:lo12:\.LANCHOR0\+8\]
**	sxtl	v0\.8h, v\2\.8b
**	ret
*/
int16x8_t
test_addressable ()
{
  return vmovl_s8 (vget_high_s8 (z));
}

/*
** test_scalable_type:
**	sxtl2	v0\.8h, v0\.16b
**	ret
*/
int16x8_t
test_scalable_type (svint8_t scalable)
{
  return vmovl_s8 (vget_high_s8 (svget_neonq_s8 (scalable)));
}

/*
** test_scalar_type:
**	fmov	d([0-9]+), x1
**	sxtl	v0\.8h, v\1\.8b
**	ret
*/
int16x8_t
test_scalar_type (__int128_t foo)
{
  return vmovl_s8 (vget_high_s8 (vreinterpretq_s8_p128 (foo)));
}

/*
** test_256b_type:
**	ldr	d([0-9]+), \[x0, #?8\]
**	sxtl	v0\.4s, v\1\.4h
**	ret
*/
int32x4_t
test_256b_type (int16x16_t foo)
{
  return vmovl_s16 ((int16x4_t) { foo[4], foo[5], foo[6], foo[7] });
}
