/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" { target aarch64_little_endian } } } */

#include <arm_neon.h>

/*
** s32x2_1:
**	dup	v0\.2s, w0
**	ret
*/
int32x2_t
s32x2_1 (int32_t x)
{
  int32_t arr[] = { x, x };
  return vld1_s32 (arr);
}

/*
** s32x2_2:
**	fmov	s0, w0
**	ret
*/
int32x2_t
s32x2_2 (int32_t x)
{
  int32_t arr[] = { x, 0 };
  return vld1_s32 (arr);
}

/*
** s32x2_3:
**	fmov	s0, w0
**	ins	v0\.s\[1\], w1
**	ret
*/
int32x2_t
s32x2_3 (int32_t x, int32_t y)
{
  int32_t arr[] = { x, y };
  return vld1_s32 (arr);
}

/*
** f32x2_1:
**	dup	v0\.2s, v0.s\[0\]
**	ret
*/
float32x2_t
f32x2_1 (float32_t x)
{
  float32_t arr[] = { x, x };
  return vld1_f32 (arr);
}

/*
** f32x2_2:
**	ins	v0\.s\[1\], v1.s\[0\]
**	ret
*/
float32x2_t
f32x2_2 (float32_t x, float32_t y)
{
  float32_t arr[] = { x, y };
  return vld1_f32 (arr);
}

/*
** s16x4_1:
**	dup	v0\.4h, w0
**	ret
*/
int16x4_t
s16x4_1 (int16_t x)
{
  int16_t arr[] = { x, x, x, x };
  return vld1_s16 (arr);
}

/*
** s16x4_2:
**	...
**	fmov	[dsh]0, [wx][0-9]+
**	ret
*/
int16x4_t
s16x4_2 (int16_t x)
{
  int16_t arr[] = { x, 0, 0, 0 };
  return vld1_s16 (arr);
}

/*
** s16x4_3:
**	dup	v0\.4h, w1
**	ins	v0.h\[0\], w0
**	ret
*/
int16x4_t
s16x4_3 (int16_t x, int16_t y)
{
  int16_t arr[] = { x, y, y, y };
  return vld1_s16 (arr);
}

/*
** f16x4_1:
**	dup	v0\.4h, v0.h\[0\]
**	ret
*/
float16x4_t
f16x4_1 (float16_t x)
{
  float16_t arr[] = { x, x, x, x };
  return vld1_f16 (arr);
}

/*
** s64x2_1:
**	dup	v0\.2d, x0
**	ret
*/
int64x2_t
s64x2_1 (int64_t x)
{
  int64_t arr[] = { x, x };
  return vld1q_s64 (arr);
}

/*
** s64x2_2: { xfail *-*-* }
**	fmov	d0, x0
**	ret
*/
int64x2_t
s64x2_2 (int64_t x)
{
  int64_t arr[] = { x, 0 };
  return vld1q_s64 (arr);
}

/*
** s64x2_3:
**	fmov	d0, x0
**	ins	v0\.d\[1\], x1
**	ret
*/
int64x2_t
s64x2_3 (int64_t x, int64_t y)
{
  int64_t arr[] = { x, y };
  return vld1q_s64 (arr);
}

/*
** f64x2_1:
**	dup	v0\.2d, v0.d\[0\]
**	ret
*/
float64x2_t
f64x2_1 (float64_t x)
{
  float64_t arr[] = { x, x };
  return vld1q_f64 (arr);
}

/*
** f64x2_2:
**	ins	v0\.d\[1\], v1.d\[0\]
**	ret
*/
float64x2_t
f64x2_2 (float64_t x, float64_t y)
{
  float64_t arr[] = { x, y };
  return vld1q_f64 (arr);
}

/*
** s32x4_1:
**	dup	v0\.4s, w0
**	ret
*/
int32x4_t
s32x4_1 (int32_t x)
{
  int32_t arr[] = { x, x, x, x };
  return vld1q_s32 (arr);
}

/*
** s32x4_2: { xfail *-*-* }
**	fmov	s0, w0
**	ret
*/
int32x4_t
s32x4_2 (int32_t x)
{
  int32_t arr[] = { x, 0, 0, 0 };
  return vld1q_s32 (arr);
}

/*
** s32x4_3:
**	dup	v0\.4s, w1
**	ins	v0.s\[0\], w0
**	ret
*/
int32x4_t
s32x4_3 (int32_t x, int32_t y)
{
  int32_t arr[] = { x, y, y, y };
  return vld1q_s32 (arr);
}

/*
** f32x4_1:
**	dup	v0\.4s, v0.s\[0\]
**	ret
*/
float32x4_t
f32x4_1 (float32_t x)
{
  float32_t arr[] = { x, x, x, x };
  return vld1q_f32 (arr);
}

void consume (float32x4_t, float32x4_t, float32x4_t, float32x4_t);

/*
** produce_1:
** (
**	dup	v0\.4s, v0\.s\[0\]
**	dup	v1\.4s, v1\.s\[0\]
**	dup	v2\.4s, v2\.s\[0\]
**	dup	v3\.4s, v3\.s\[0\]
** |
**	dup	v3\.4s, v3\.s\[0\]
**	dup	v2\.4s, v2\.s\[0\]
**	dup	v1\.4s, v1\.s\[0\]
**	dup	v0\.4s, v0\.s\[0\]
** )
**	b	consume
*/
void
produce_1 (float32_t a, float32_t b, float32_t c, float32_t d)
{
  float arr[4][4] = {
    { a, a, a, a },
    { b, b, b, b },
    { c, c, c, c },
    { d, d, d, d }
  };
  consume (vld1q_f32 (arr[0]), vld1q_f32 (arr[1]),
	   vld1q_f32 (arr[2]), vld1q_f32 (arr[3]));
}

/*
** produce_2:
** (
**	dup	v0\.4s, v0\.s\[0\]
**	dup	v1\.4s, v1\.s\[0\]
**	dup	v2\.4s, v2\.s\[0\]
**	dup	v3\.4s, v3\.s\[0\]
** |
**	dup	v3\.4s, v3\.s\[0\]
**	dup	v2\.4s, v2\.s\[0\]
**	dup	v1\.4s, v1\.s\[0\]
**	dup	v0\.4s, v0\.s\[0\]
** )
**	b	consume
*/
void
produce_2 (float32_t a, float32_t b, float32_t c, float32_t d)
{
  float arr0[] = { a, a, a, a };
  float arr1[] = { b, b, b, b };
  float arr2[] = { c, c, c, c };
  float arr3[] = { d, d, d, d };
  consume (vld1q_f32 (arr0), vld1q_f32 (arr1),
	   vld1q_f32 (arr2), vld1q_f32 (arr3));
}
