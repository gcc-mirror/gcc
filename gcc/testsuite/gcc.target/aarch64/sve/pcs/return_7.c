/* { dg-do compile } */
/* { dg-options "-O -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** callee_s8:
**	mov	z0\.b, #1
**	mov	z1\.b, #2
**	ret
*/
svint8x2_t __attribute__((noipa))
callee_s8 (void)
{
  return svcreate2 (svdup_s8 (1), svdup_s8 (2));
}

/*
** caller_s8:
**	...
**	bl	callee_s8
**	trn1	z0\.b, z0\.b, z1\.b
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint8_t __attribute__((noipa))
caller_s8 (void)
{
  svint8x2_t res;
  res = callee_s8 ();
  return svtrn1 (svget2 (res, 0), svget2 (res, 1));
}

/*
** callee_u8:
**	mov	z0\.b, #3
**	mov	z1\.b, #4
**	ret
*/
svuint8x2_t __attribute__((noipa))
callee_u8 (void)
{
  return svcreate2 (svdup_u8 (3), svdup_u8 (4));
}

/*
** caller_u8:
**	...
**	bl	callee_u8
**	trn2	z0\.b, z1\.b, z0\.b
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint8_t __attribute__((noipa))
caller_u8 (void)
{
  svuint8x2_t res;
  res = callee_u8 ();
  return svtrn2 (svget2 (res, 1), svget2 (res, 0));
}

/*
** callee_s16:
**	mov	z0\.h, #1
**	mov	z1\.h, #2
**	ret
*/
svint16x2_t __attribute__((noipa))
callee_s16 (void)
{
  return svcreate2 (svdup_s16 (1), svdup_s16 (2));
}

/*
** caller_s16:
**	...
**	bl	callee_s16
**	trn1	z0\.h, z0\.h, z1\.h
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint16_t __attribute__((noipa))
caller_s16 (void)
{
  svint16x2_t res;
  res = callee_s16 ();
  return svtrn1 (svget2 (res, 0), svget2 (res, 1));
}

/*
** callee_u16:
**	mov	z0\.h, #3
**	mov	z1\.h, #4
**	ret
*/
svuint16x2_t __attribute__((noipa))
callee_u16 (void)
{
  return svcreate2 (svdup_u16 (3), svdup_u16 (4));
}

/*
** caller_u16:
**	...
**	bl	callee_u16
**	trn2	z0\.h, z1\.h, z0\.h
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint16_t __attribute__((noipa))
caller_u16 (void)
{
  svuint16x2_t res;
  res = callee_u16 ();
  return svtrn2 (svget2 (res, 1), svget2 (res, 0));
}

/*
** callee_f16:
**	fmov	z0\.h, #5\.0(?:e\+0)?
**	fmov	z1\.h, #6\.0(?:e\+0)?
**	ret
*/
svfloat16x2_t __attribute__((noipa))
callee_f16 (void)
{
  return svcreate2 (svdup_f16 (5), svdup_f16 (6));
}

/*
** caller_f16:
**	...
**	bl	callee_f16
**	zip1	z0\.h, z1\.h, z0\.h
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svfloat16_t __attribute__((noipa))
caller_f16 (void)
{
  svfloat16x2_t res;
  res = callee_f16 ();
  return svzip1 (svget2 (res, 1), svget2 (res, 0));
}

/*
** callee_bf16:
**	mov	z0\.h, h2
**	mov	z1\.h, h3
**	ret
*/
svbfloat16x2_t __attribute__((noipa))
callee_bf16 (bfloat16_t h0, bfloat16_t h1, bfloat16_t h2, bfloat16_t h3)
{
  return svcreate2 (svdup_bf16 (h2), svdup_bf16 (h3));
}

/*
** caller_bf16:
**	...
**	bl	callee_bf16
**	zip2	z0\.h, z1\.h, z0\.h
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svbfloat16_t __attribute__((noipa))
caller_bf16 (bfloat16_t h0, bfloat16_t h1, bfloat16_t h2, bfloat16_t h3)
{
  svbfloat16x2_t res;
  res = callee_bf16 (h0, h1, h2, h3);
  return svzip2 (svget2 (res, 1), svget2 (res, 0));
}

/*
** callee_s32:
**	mov	z0\.s, #1
**	mov	z1\.s, #2
**	ret
*/
svint32x2_t __attribute__((noipa))
callee_s32 (void)
{
  return svcreate2 (svdup_s32 (1), svdup_s32 (2));
}

/*
** caller_s32:
**	...
**	bl	callee_s32
**	trn1	z0\.s, z0\.s, z1\.s
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint32_t __attribute__((noipa))
caller_s32 (void)
{
  svint32x2_t res;
  res = callee_s32 ();
  return svtrn1 (svget2 (res, 0), svget2 (res, 1));
}

/*
** callee_u32:
**	mov	z0\.s, #3
**	mov	z1\.s, #4
**	ret
*/
svuint32x2_t __attribute__((noipa))
callee_u32 (void)
{
  return svcreate2 (svdup_u32 (3), svdup_u32 (4));
}

/*
** caller_u32:
**	...
**	bl	callee_u32
**	trn2	z0\.s, z1\.s, z0\.s
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint32_t __attribute__((noipa))
caller_u32 (void)
{
  svuint32x2_t res;
  res = callee_u32 ();
  return svtrn2 (svget2 (res, 1), svget2 (res, 0));
}

/*
** callee_f32:
**	fmov	z0\.s, #5\.0(?:e\+0)?
**	fmov	z1\.s, #6\.0(?:e\+0)?
**	ret
*/
svfloat32x2_t __attribute__((noipa))
callee_f32 (void)
{
  return svcreate2 (svdup_f32 (5), svdup_f32 (6));
}

/*
** caller_f32:
**	...
**	bl	callee_f32
**	zip1	z0\.s, z1\.s, z0\.s
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svfloat32_t __attribute__((noipa))
caller_f32 (void)
{
  svfloat32x2_t res;
  res = callee_f32 ();
  return svzip1 (svget2 (res, 1), svget2 (res, 0));
}

/*
** callee_s64:
**	mov	z0\.d, #1
**	mov	z1\.d, #2
**	ret
*/
svint64x2_t __attribute__((noipa))
callee_s64 (void)
{
  return svcreate2 (svdup_s64 (1), svdup_s64 (2));
}

/*
** caller_s64:
**	...
**	bl	callee_s64
**	trn1	z0\.d, z0\.d, z1\.d
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint64_t __attribute__((noipa))
caller_s64 (void)
{
  svint64x2_t res;
  res = callee_s64 ();
  return svtrn1 (svget2 (res, 0), svget2 (res, 1));
}

/*
** callee_u64:
**	mov	z0\.d, #3
**	mov	z1\.d, #4
**	ret
*/
svuint64x2_t __attribute__((noipa))
callee_u64 (void)
{
  return svcreate2 (svdup_u64 (3), svdup_u64 (4));
}

/*
** caller_u64:
**	...
**	bl	callee_u64
**	trn2	z0\.d, z1\.d, z0\.d
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint64_t __attribute__((noipa))
caller_u64 (void)
{
  svuint64x2_t res;
  res = callee_u64 ();
  return svtrn2 (svget2 (res, 1), svget2 (res, 0));
}

/*
** callee_f64:
**	fmov	z0\.d, #5\.0(?:e\+0)?
**	fmov	z1\.d, #6\.0(?:e\+0)?
**	ret
*/
svfloat64x2_t __attribute__((noipa))
callee_f64 (void)
{
  return svcreate2 (svdup_f64 (5), svdup_f64 (6));
}

/*
** caller_f64:
**	...
**	bl	callee_f64
**	zip1	z0\.d, z1\.d, z0\.d
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svfloat64_t __attribute__((noipa))
caller_f64 (void)
{
  svfloat64x2_t res;
  res = callee_f64 ();
  return svzip1 (svget2 (res, 1), svget2 (res, 0));
}
