/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_neon.h>

void ext();

/*
** s32_1:
**	fmov	s0, w0
**	ins	v0\.s\[1\], w1
**	ret
*/
int32x2_t s32_1(int32_t a0, int32_t a1) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int32x2_t) { a1, a0 };
  else
    return (int32x2_t) { a0, a1 };
}
/*
** s32_2:
**	fmov	s0, w0
**	ld1	{v0\.s}\[1\], \[x1\]
**	ret
*/
int32x2_t s32_2(int32_t a0, int32_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int32x2_t) { ptr[0], a0 };
  else
    return (int32x2_t) { a0, ptr[0] };
}
/*
** s32_3:
**	ldr	s0, \[x0\]
**	ins	v0\.s\[1\], w1
**	ret
*/
int32x2_t s32_3(int32_t *ptr, int32_t a1) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int32x2_t) { a1, ptr[0] };
  else
    return (int32x2_t) { ptr[0], a1 };
}
/*
** s32_4:
**	stp	w1, w2, \[x0\]
**	ret
*/
void s32_4(int32x2_t *res, int32_t a0, int32_t a1) {
  res[0] = (int32x2_t) { a0, a1 };
}
/*
** s32_5:
**	stp	w1, w2, \[x0, #?4\]
**	ret
*/
void s32_5(uintptr_t res, int32_t a0, int32_t a1) {
  *(int32x2_t *)(res + 4) = (int32x2_t) { a0, a1 };
}
/* Currently uses d8 to hold res across the call.  */
int32x2_t s32_6(int32_t a0, int32_t a1) {
  int32x2_t res = { a0, a1 };
  ext ();
  return res;
}

/*
** f32_1:
**	uzp1	v0\.2s, v0\.2s, v1\.2s
**	ret
*/
float32x2_t f32_1(float32_t a0, float32_t a1) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float32x2_t) { a1, a0 };
  else
    return (float32x2_t) { a0, a1 };
}
/*
** f32_2:
**	ld1	{v0\.s}\[1\], \[x0\]
**	ret
*/
float32x2_t f32_2(float32_t a0, float32_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float32x2_t) { ptr[0], a0 };
  else
    return (float32x2_t) { a0, ptr[0] };
}
/*
** f32_3:
**	ldr	s0, \[x0\]
**	uzp1	v0\.2s, v0\.2s, v1\.2s
**	ret
*/
float32x2_t f32_3(float32_t a0, float32_t a1, float32_t *ptr) {
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (float32x2_t) { a1, ptr[0] };
  else
    return (float32x2_t) { ptr[0], a1 };
}
/*
** f32_4:
**	stp	s0, s1, \[x0\]
**	ret
*/
void f32_4(float32x2_t *res, float32_t a0, float32_t a1) {
  res[0] = (float32x2_t) { a0, a1 };
}
/*
** f32_5:
**	stp	s0, s1, \[x0, #?4\]
**	ret
*/
void f32_5(uintptr_t res, float32_t a0, float32_t a1) {
  *(float32x2_t *)(res + 4) = (float32x2_t) { a0, a1 };
}
/* Currently uses d8 to hold res across the call.  */
float32x2_t f32_6(float32_t a0, float32_t a1) {
  float32x2_t res = { a0, a1 };
  ext ();
  return res;
}
