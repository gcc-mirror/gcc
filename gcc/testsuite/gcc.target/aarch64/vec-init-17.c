/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_neon.h>

/*
** s32_1:
**	fmov	s0, w0
**	ret
*/
int32x2_t s32_1(int32_t a0) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int32x2_t) { 0, a0 };
  else
    return (int32x2_t) { a0, 0 };
}
/*
** s32_2:
**	ldr	s0, \[x0\]
**	ret
*/
int32x2_t s32_2(int32_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int32x2_t) { 0, ptr[0] };
  else
    return (int32x2_t) { ptr[0], 0 };
}
/*
** s32_3:
**	ldr	s0, \[x0, #?4\]
**	ret
*/
int32x2_t s32_3(int32_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int32x2_t) { 0, ptr[1] };
  else
    return (int32x2_t) { ptr[1], 0 };
}

/*
** f32_1:
**	fmov	s0, s0
**	ret
*/
float32x2_t f32_1(float32_t a0) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float32x2_t) { 0, a0 };
  else
    return (float32x2_t) { a0, 0 };
}
/*
** f32_2:
**	ldr	s0, \[x0\]
**	ret
*/
float32x2_t f32_2(float32_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float32x2_t) { 0, ptr[0] };
  else
    return (float32x2_t) { ptr[0], 0 };
}
/*
** f32_3:
**	ldr	s0, \[x0, #?4\]
**	ret
*/
float32x2_t f32_3(float32_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float32x2_t) { 0, ptr[1] };
  else
    return (float32x2_t) { ptr[1], 0 };
}
