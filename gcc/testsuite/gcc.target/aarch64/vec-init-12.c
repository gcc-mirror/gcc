/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_neon.h>

/*
** s32_1:
**	ldr	q0, \[x0\]
**	ret
*/
int32x4_t s32_1(int32x2_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return vcombine_s32 (ptr[1], ptr[0]);
  else
    return vcombine_s32 (ptr[0], ptr[1]);
}
/*
** s32_2:
**	add	x([0-9])+, x0, #?8
**	ld1	{v0\.d}\[1\], \[x\1\]
**	ret
*/
int32x4_t s32_2(int32x2_t a0, int32x2_t *ptr) {
  return vcombine_s32 (a0, ptr[1]);
}
/*
** s32_3:
**	ldr	d0, \[x0\], #?16
**	ld1	{v0\.d}\[1\], \[x0\]
**	ret
*/
int32x4_t s32_3(int32x2_t *ptr) {
  return vcombine_s32 (ptr[0], ptr[2]);
}

/*
** f32_1:
**	ldr	q0, \[x0\]
**	ret
*/
float32x4_t f32_1(float32x2_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return vcombine_f32 (ptr[1], ptr[0]);
  else
    return vcombine_f32 (ptr[0], ptr[1]);
}
/*
** f32_2:
**	add	x([0-9])+, x0, #?8
**	ld1	{v0\.d}\[1\], \[x\1\]
**	ret
*/
float32x4_t f32_2(float32x2_t a0, float32x2_t *ptr) {
  return vcombine_f32 (a0, ptr[1]);
}
/*
** f32_3:
**	ldr	d0, \[x0\], #?16
**	ld1	{v0\.d}\[1\], \[x0\]
**	ret
*/
float32x4_t f32_3(float32x2_t *ptr) {
  return vcombine_f32 (ptr[0], ptr[2]);
}
