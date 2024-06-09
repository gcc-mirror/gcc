/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_neon.h>

void ext();

/*
** s64q_1:
**	fmov	d0, x0
**	ins	v0\.d\[1\], x1
**	ret
*/
int64x2_t s64q_1(int64_t a0, int64_t a1) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int64x2_t) { a1, a0 };
  else
    return (int64x2_t) { a0, a1 };
}
/*
** s64q_2:
**	fmov	d0, x0
**	ld1	{v0\.d}\[1\], \[x1\]
**	ret
*/
int64x2_t s64q_2(int64_t a0, int64_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int64x2_t) { ptr[0], a0 };
  else
    return (int64x2_t) { a0, ptr[0] };
}
/*
** s64q_3:
**	ldr	d0, \[x0\]
**	ins	v0\.d\[1\], x1
**	ret
*/
int64x2_t s64q_3(int64_t *ptr, int64_t a1) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int64x2_t) { a1, ptr[0] };
  else
    return (int64x2_t) { ptr[0], a1 };
}
/*
** s64q_4:
**	stp	x1, x2, \[x0\]
**	ret
*/
void s64q_4(int64x2_t *res, int64_t a0, int64_t a1) {
  res[0] = (int64x2_t) { a0, a1 };
}
/*
** s64q_5:
**	stp	x1, x2, \[x0, #?8\]
**	ret
*/
void s64q_5(uintptr_t res, int64_t a0, int64_t a1) {
  *(int64x2_t *)(res + 8) = (int64x2_t) { a0, a1 };
}
/*
** s64q_6:
**	...
**	stp	x0, x1, .*
**	...
**	ldr	q0, .*
**	...
**	ret
*/
int64x2_t s64q_6(int64_t a0, int64_t a1) {
  int64x2_t res = { a0, a1 };
  ext ();
  return res;
}

/*
** f64q_1:
**	uzp1	v0\.2d, v0\.2d, v1\.2d
**	ret
*/
float64x2_t f64q_1(float64_t a0, float64_t a1) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float64x2_t) { a1, a0 };
  else
    return (float64x2_t) { a0, a1 };
}
/*
** f64q_2:
**	ld1	{v0\.d}\[1\], \[x0\]
**	ret
*/
float64x2_t f64q_2(float64_t a0, float64_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float64x2_t) { ptr[0], a0 };
  else
    return (float64x2_t) { a0, ptr[0] };
}
/*
** f64q_3:
**	ldr	d0, \[x0\]
**	uzp1	v0\.2d, v0\.2d, v1\.2d
**	ret
*/
float64x2_t f64q_3(float64_t a0, float64_t a1, float64_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float64x2_t) { a1, ptr[0] };
  else
    return (float64x2_t) { ptr[0], a1 };
}
/*
** f64q_4:
**	stp	d0, d1, \[x0\]
**	ret
*/
void f64q_4(float64x2_t *res, float64_t a0, float64_t a1) {
  res[0] = (float64x2_t) { a0, a1 };
}
/*
** f64q_5:
**	stp	d0, d1, \[x0, #?8\]
**	ret
*/
void f64q_5(uintptr_t res, float64_t a0, float64_t a1) {
  *(float64x2_t *)(res + 8) = (float64x2_t) { a0, a1 };
}
/*
** f64q_6:
**	...
**	stp	d0, d1, .*
**	...
**	ldr	q0, .*
**	...
**	ret
*/
float64x2_t f64q_6(float64_t a0, float64_t a1) {
  float64x2_t res = { a0, a1 };
  ext ();
  return res;
}

/*
** s32q_1:
**	uzp1	v0\.2d, v0\.2d, v1\.2d
**	ret
*/
int32x4_t s32q_1(int32x2_t a0, int32x2_t a1) {
  return vcombine_s32 (a0, a1);
}
/*
** s32q_2:
**	ld1	{v0\.d}\[1\], \[x0\]
**	ret
*/
int32x4_t s32q_2(int32x2_t a0, int32x2_t *ptr) {
  return vcombine_s32 (a0, ptr[0]);
}
/*
** s32q_3:
**	ldr	d0, \[x0\]
**	uzp1	v0\.2d, v0\.2d, v1\.2d
**	ret
*/
int32x4_t s32q_3(int32x2_t a0, int32x2_t a1, int32x2_t *ptr) {
  return vcombine_s32 (ptr[0], a1);
}
/*
** s32q_4:
**	stp	d0, d1, \[x0\]
**	ret
*/
void s32q_4(int32x4_t *res, int32x2_t a0, int32x2_t a1) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    res[0] = vcombine_s32 (a1, a0);
  else
    res[0] = vcombine_s32 (a0, a1);
}
/*
** s32q_5:
**	stp	d0, d1, \[x0, #?8\]
**	ret
*/
void s32q_5(uintptr_t res, int32x2_t a0, int32x2_t a1) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    *(int32x4_t *)(res + 8) = vcombine_s32 (a1, a0);
  else
    *(int32x4_t *)(res + 8) = vcombine_s32 (a0, a1);
}
/*
** s32q_6:
**	...
**	stp	d0, d1, .*
**	...
**	ldr	q0, .*
**	...
**	ret
*/
int32x4_t s32q_6(int32x2_t a0, int32x2_t a1) {
  int32x4_t res = (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
		   ? vcombine_s32 (a1, a0)
		   : vcombine_s32 (a0, a1));
  ext ();
  return res;
}

/*
** f32q_1:
**	uzp1	v0\.2d, v0\.2d, v1\.2d
**	ret
*/
float32x4_t f32q_1(float32x2_t a0, float32x2_t a1) {
  return vcombine_f32 (a0, a1);
}
/*
** f32q_2:
**	ld1	{v0\.d}\[1\], \[x0\]
**	ret
*/
float32x4_t f32q_2(float32x2_t a0, float32x2_t *ptr) {
  return vcombine_f32 (a0, ptr[0]);
}
/*
** f32q_3:
**	ldr	d0, \[x0\]
**	uzp1	v0\.2d, v0\.2d, v1\.2d
**	ret
*/
float32x4_t f32q_3(float32x2_t a0, float32x2_t a1, float32x2_t *ptr) {
  return vcombine_f32 (ptr[0], a1);
}
/*
** f32q_4:
**	stp	d0, d1, \[x0\]
**	ret
*/
void f32q_4(float32x4_t *res, float32x2_t a0, float32x2_t a1) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    res[0] = vcombine_f32 (a1, a0);
  else
    res[0] = vcombine_f32 (a0, a1);
}
/*
** f32q_5:
**	stp	d0, d1, \[x0, #?8\]
**	ret
*/
void f32q_5(uintptr_t res, float32x2_t a0, float32x2_t a1) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    *(float32x4_t *)(res + 8) = vcombine_f32 (a1, a0);
  else
    *(float32x4_t *)(res + 8) = vcombine_f32 (a0, a1);
}
/*
** f32q_6:
**	...
**	stp	d0, d1, .*
**	...
**	ldr	q0, .*
**	...
**	ret
*/
float32x4_t f32q_6(float32x2_t a0, float32x2_t a1) {
  float32x4_t res = (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
		     ? vcombine_f32 (a1, a0)
		     : vcombine_f32 (a0, a1));
  ext ();
  return res;
}
