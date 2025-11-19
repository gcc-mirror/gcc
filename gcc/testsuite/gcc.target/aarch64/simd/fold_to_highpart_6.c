/* { dg-do compile } */
/* { dg-require-effective-target aarch64_little_endian } */
/* { dg-options "-O2 -march=armv8-a+sve" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_neon_sve_bridge.h>

typedef int16_t int16x16_t __attribute__ ((vector_size (32)));

/* Edge cases where we don't/can't fold, reject these gracefully.  */

int8x16_t z;

int16x8_t
test_addressable ()
{
  return vmovl_s8 (vget_high_s8 (z));
}

/*
** test_scalable_type:
**	sxtl2	v0.8h, v0.16b
**	ret
*/
int16x8_t
test_scalable_type (svint8_t scalable)
{
  return vmovl_s8 (vget_high_s8 (svget_neonq_s8 (scalable)));
}

int16x8_t
test_scalar_type (__int128_t foo)
{
  return vmovl_s8 (vget_high_s8 (vreinterpretq_s8_p128 (foo)));
}

int32x4_t
test_256b_type (int16x16_t foo)
{
  return vmovl_s16 ((int16x4_t) { foo[4], foo[5], foo[6], foo[7] });
}

/* { dg-final { scan-assembler-times {sxtl2\t} 1 } } */
/* { dg-final { scan-assembler-times {sxtl\t} 3 } } */
