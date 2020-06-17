/* { dg-do compile } */
/* { dg-options "-O -frename-registers -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** callee_s8:
**	mov	z0\.b, #1
**	mov	z1\.b, #2
**	mov	z2\.b, #3
**	ret
*/
svint8x3_t __attribute__((noipa))
callee_s8 (void)
{
  return svcreate3 (svdup_s8 (1), svdup_s8 (2), svdup_s8 (3));
}

/*
** caller_s8:
**	...
**	bl	callee_s8
**	ptrue	(p[0-7])\.b, all
**	mad	z0\.b, \1/m, z1\.b, z2\.b
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint8_t __attribute__((noipa))
caller_s8 (void)
{
  svint8x3_t res;
  res = callee_s8 ();
  return svmad_x (svptrue_b8 (),
		  svget3 (res, 0), svget3 (res, 1), svget3 (res, 2));
}

/*
** callee_u8:
**	mov	z0\.b, #4
**	mov	z1\.b, #5
**	mov	z2\.b, #6
**	ret
*/
svuint8x3_t __attribute__((noipa))
callee_u8 (void)
{
  return svcreate3 (svdup_u8 (4), svdup_u8 (5), svdup_u8 (6));
}

/*
** caller_u8:
**	...
**	bl	callee_u8
**	ptrue	(p[0-7])\.b, all
**	msb	z0\.b, \1/m, z1\.b, z2\.b
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint8_t __attribute__((noipa))
caller_u8 (void)
{
  svuint8x3_t res;
  res = callee_u8 ();
  return svmsb_x (svptrue_b8 (),
		  svget3 (res, 0), svget3 (res, 1), svget3 (res, 2));
}

/*
** callee_s16:
**	mov	z0\.h, #1
**	mov	z1\.h, #2
**	mov	z2\.h, #3
**	ret
*/
svint16x3_t __attribute__((noipa))
callee_s16 (void)
{
  return svcreate3 (svdup_s16 (1), svdup_s16 (2), svdup_s16 (3));
}

/*
** caller_s16:
**	...
**	bl	callee_s16
**	ptrue	(p[0-7])\.b, all
**	mls	z0\.h, \1/m, z1\.h, z2\.h
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint16_t __attribute__((noipa))
caller_s16 (void)
{
  svint16x3_t res;
  res = callee_s16 ();
  return svmls_x (svptrue_b16 (),
		  svget3 (res, 0), svget3 (res, 1), svget3 (res, 2));
}

/*
** callee_u16:
**	mov	z0\.h, #4
**	mov	z1\.h, #5
**	mov	z2\.h, #6
**	ret
*/
svuint16x3_t __attribute__((noipa))
callee_u16 (void)
{
  return svcreate3 (svdup_u16 (4), svdup_u16 (5), svdup_u16 (6));
}

/*
** caller_u16:
**	...
**	bl	callee_u16
**	ptrue	(p[0-7])\.b, all
**	mla	z0\.h, \1/m, z1\.h, z2\.h
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint16_t __attribute__((noipa))
caller_u16 (void)
{
  svuint16x3_t res;
  res = callee_u16 ();
  return svmla_x (svptrue_b16 (),
		  svget3 (res, 0), svget3 (res, 1), svget3 (res, 2));
}

/*
** callee_f16:
**	fmov	z0\.h, #1\.0(?:e\+0)?
**	fmov	z1\.h, #2\.0(?:e\+0)?
**	fmov	z2\.h, #3\.0(?:e\+0)?
**	ret
*/
svfloat16x3_t __attribute__((noipa))
callee_f16 (void)
{
  return svcreate3 (svdup_f16 (1), svdup_f16 (2), svdup_f16 (3));
}

/*
** caller_f16:
**	...
**	bl	callee_f16
**	ptrue	(p[0-7])\.b, all
**	fmla	z0\.h, \1/m, z1\.h, z2\.h
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svfloat16_t __attribute__((noipa))
caller_f16 (void)
{
  svfloat16x3_t res;
  res = callee_f16 ();
  return svmla_x (svptrue_b16 (),
		  svget3 (res, 0), svget3 (res, 1), svget3 (res, 2));
}

/*
** callee_bf16:
**	mov	z0\.h, h0
**	mov	z1\.h, h1
**	mov	z2\.h, h2
**	ret
*/
svbfloat16x3_t __attribute__((noipa))
callee_bf16 (bfloat16_t h0, bfloat16_t h1, bfloat16_t h2)
{
  return svcreate3 (svdup_bf16 (h0), svdup_bf16 (h1), svdup_bf16 (h2));
}

/*
** caller_bf16:
**	...
**	bl	callee_bf16
**	trn2	z0\.h, z0\.h, z2\.h
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svbfloat16_t __attribute__((noipa))
caller_bf16 (bfloat16_t h0, bfloat16_t h1, bfloat16_t h2)
{
  svbfloat16x3_t res;
  res = callee_bf16 (h0, h1, h2);
  return svtrn2 (svget3 (res, 0), svget3 (res, 2));
}

/*
** callee_s32:
**	mov	z0\.s, #1
**	mov	z1\.s, #2
**	mov	z2\.s, #3
**	ret
*/
svint32x3_t __attribute__((noipa))
callee_s32 (void)
{
  return svcreate3 (svdup_s32 (1), svdup_s32 (2), svdup_s32 (3));
}

/*
** caller_s32:
**	...
**	bl	callee_s32
**	ptrue	(p[0-7])\.b, all
**	mad	z0\.s, \1/m, z1\.s, z2\.s
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint32_t __attribute__((noipa))
caller_s32 (void)
{
  svint32x3_t res;
  res = callee_s32 ();
  return svmad_x (svptrue_b32 (),
		  svget3 (res, 0), svget3 (res, 1), svget3 (res, 2));
}

/*
** callee_u32:
**	mov	z0\.s, #4
**	mov	z1\.s, #5
**	mov	z2\.s, #6
**	ret
*/
svuint32x3_t __attribute__((noipa))
callee_u32 (void)
{
  return svcreate3 (svdup_u32 (4), svdup_u32 (5), svdup_u32 (6));
}

/*
** caller_u32:
**	...
**	bl	callee_u32
**	ptrue	(p[0-7])\.b, all
**	msb	z0\.s, \1/m, z1\.s, z2\.s
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint32_t __attribute__((noipa))
caller_u32 (void)
{
  svuint32x3_t res;
  res = callee_u32 ();
  return svmsb_x (svptrue_b32 (),
		  svget3 (res, 0), svget3 (res, 1), svget3 (res, 2));
}

/*
** callee_f32:
**	fmov	z0\.s, #1\.0(?:e\+0)?
**	fmov	z1\.s, #2\.0(?:e\+0)?
**	fmov	z2\.s, #3\.0(?:e\+0)?
**	ret
*/
svfloat32x3_t __attribute__((noipa))
callee_f32 (void)
{
  return svcreate3 (svdup_f32 (1), svdup_f32 (2), svdup_f32 (3));
}

/*
** caller_f32:
**	...
**	bl	callee_f32
**	ptrue	(p[0-7])\.b, all
**	fmla	z0\.s, \1/m, z1\.s, z2\.s
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svfloat32_t __attribute__((noipa))
caller_f32 (void)
{
  svfloat32x3_t res;
  res = callee_f32 ();
  return svmla_x (svptrue_b32 (),
		  svget3 (res, 0), svget3 (res, 1), svget3 (res, 2));
}

/*
** callee_s64:
**	mov	z0\.d, #1
**	mov	z1\.d, #2
**	mov	z2\.d, #3
**	ret
*/
svint64x3_t __attribute__((noipa))
callee_s64 (void)
{
  return svcreate3 (svdup_s64 (1), svdup_s64 (2), svdup_s64 (3));
}

/*
** caller_s64:
**	...
**	bl	callee_s64
**	ptrue	(p[0-7])\.b, all
**	mls	z0\.d, \1/m, z1\.d, z2\.d
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint64_t __attribute__((noipa))
caller_s64 (void)
{
  svint64x3_t res;
  res = callee_s64 ();
  return svmls_x (svptrue_b64 (),
		  svget3 (res, 0), svget3 (res, 1), svget3 (res, 2));
}

/*
** callee_u64:
**	mov	z0\.d, #4
**	mov	z1\.d, #5
**	mov	z2\.d, #6
**	ret
*/
svuint64x3_t __attribute__((noipa))
callee_u64 (void)
{
  return svcreate3 (svdup_u64 (4), svdup_u64 (5), svdup_u64 (6));
}

/*
** caller_u64:
**	...
**	bl	callee_u64
**	ptrue	(p[0-7])\.b, all
**	mla	z0\.d, \1/m, z1\.d, z2\.d
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint64_t __attribute__((noipa))
caller_u64 (void)
{
  svuint64x3_t res;
  res = callee_u64 ();
  return svmla_x (svptrue_b64 (),
		  svget3 (res, 0), svget3 (res, 1), svget3 (res, 2));
}

/*
** callee_f64:
**	fmov	z0\.d, #1\.0(?:e\+0)?
**	fmov	z1\.d, #2\.0(?:e\+0)?
**	fmov	z2\.d, #3\.0(?:e\+0)?
**	ret
*/
svfloat64x3_t __attribute__((noipa))
callee_f64 (void)
{
  return svcreate3 (svdup_f64 (1), svdup_f64 (2), svdup_f64 (3));
}

/*
** caller_f64:
**	...
**	bl	callee_f64
**	ptrue	(p[0-7])\.b, all
**	fmla	z0\.d, \1/m, z1\.d, z2\.d
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svfloat64_t __attribute__((noipa))
caller_f64 (void)
{
  svfloat64x3_t res;
  res = callee_f64 ();
  return svmla_x (svptrue_b64 (),
		  svget3 (res, 0), svget3 (res, 1), svget3 (res, 2));
}
