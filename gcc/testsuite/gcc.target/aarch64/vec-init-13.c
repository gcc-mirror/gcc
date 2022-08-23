/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_neon.h>

/*
** s64q_1:
**	fmov	d0, x0
**	ret
*/
int64x2_t s64q_1(int64_t a0) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int64x2_t) { 0, a0 };
  else
    return (int64x2_t) { a0, 0 };
}
/*
** s64q_2:
**	ldr	d0, \[x0\]
**	ret
*/
int64x2_t s64q_2(int64_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int64x2_t) { 0, ptr[0] };
  else
    return (int64x2_t) { ptr[0], 0 };
}
/*
** s64q_3:
**	ldr	d0, \[x0, #?8\]
**	ret
*/
int64x2_t s64q_3(int64_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int64x2_t) { 0, ptr[1] };
  else
    return (int64x2_t) { ptr[1], 0 };
}

/*
** f64q_1:
**	fmov	d0, d0
**	ret
*/
float64x2_t f64q_1(float64_t a0) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float64x2_t) { 0, a0 };
  else
    return (float64x2_t) { a0, 0 };
}
/*
** f64q_2:
**	ldr	d0, \[x0\]
**	ret
*/
float64x2_t f64q_2(float64_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float64x2_t) { 0, ptr[0] };
  else
    return (float64x2_t) { ptr[0], 0 };
}
/*
** f64q_3:
**	ldr	d0, \[x0, #?8\]
**	ret
*/
float64x2_t f64q_3(float64_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float64x2_t) { 0, ptr[1] };
  else
    return (float64x2_t) { ptr[1], 0 };
}

/*
** s32q_1:
**	fmov	d0, d0
**	ret
*/
int32x4_t s32q_1(int32x2_t a0, int32x2_t a1) {
  return vcombine_s32 (a0, (int32x2_t) { 0, 0 });
}
/*
** s32q_2:
**	ldr	d0, \[x0\]
**	ret
*/
int32x4_t s32q_2(int32x2_t *ptr) {
  return vcombine_s32 (ptr[0], (int32x2_t) { 0, 0 });
}
/*
** s32q_3:
**	ldr	d0, \[x0, #?8\]
**	ret
*/
int32x4_t s32q_3(int32x2_t *ptr) {
  return vcombine_s32 (ptr[1], (int32x2_t) { 0, 0 });
}

/*
** f32q_1:
**	fmov	d0, d0
**	ret
*/
float32x4_t f32q_1(float32x2_t a0, float32x2_t a1) {
  return vcombine_f32 (a0, (float32x2_t) { 0, 0 });
}
/*
** f32q_2:
**	ldr	d0, \[x0\]
**	ret
*/
float32x4_t f32q_2(float32x2_t *ptr) {
  return vcombine_f32 (ptr[0], (float32x2_t) { 0, 0 });
}
/*
** f32q_3:
**	ldr	d0, \[x0, #?8\]
**	ret
*/
float32x4_t f32q_3(float32x2_t *ptr) {
  return vcombine_f32 (ptr[1], (float32x2_t) { 0, 0 });
}
