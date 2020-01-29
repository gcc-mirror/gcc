/* { dg-do compile } */
/* { dg-options "-O -frename-registers -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** callee_s8:
**	mov	z0\.b, #1
**	mov	z1\.b, #2
**	mov	z2\.b, #3
**	mov	z3\.b, #4
**	ret
*/
svint8x4_t __attribute__((noipa))
callee_s8 (void)
{
  return svcreate4 (svdup_s8 (1), svdup_s8 (2), svdup_s8 (3), svdup_s8 (4));
}

/*
** caller_s8:
**	...
**	bl	callee_s8
**	add	(z[2-7]\.b), z2\.b, z3\.b
**	ptrue	(p[0-7])\.b, all
**	mla	z0\.b, \2/m, (z1\.b, \1|\1, z1\.b)
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint8_t __attribute__((noipa))
caller_s8 (void)
{
  svint8x4_t res;
  res = callee_s8 ();
  return svmla_x (svptrue_b8 (), svget4 (res, 0), svget4 (res, 1),
		  svadd_x (svptrue_b8 (),
			   svget4 (res, 2),
			   svget4 (res, 3)));
}

/*
** callee_u8:
**	mov	z0\.b, #4
**	mov	z1\.b, #5
**	mov	z2\.b, #6
**	mov	z3\.b, #7
**	ret
*/
svuint8x4_t __attribute__((noipa))
callee_u8 (void)
{
  return svcreate4 (svdup_u8 (4), svdup_u8 (5), svdup_u8 (6), svdup_u8 (7));
}

/*
** caller_u8:
**	...
**	bl	callee_u8
**	sub	(z[2-7]\.b), z2\.b, z3\.b
**	ptrue	(p[0-7])\.b, all
**	mla	z0\.b, \2/m, (z1\.b, \1|\1, z1\.b)
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint8_t __attribute__((noipa))
caller_u8 (void)
{
  svuint8x4_t res;
  res = callee_u8 ();
  return svmla_x (svptrue_b8 (), svget4 (res, 0), svget4 (res, 1),
		  svsub_x (svptrue_b8 (),
			   svget4 (res, 2),
			   svget4 (res, 3)));
}

/*
** callee_s16:
**	mov	z0\.h, #1
**	mov	z1\.h, #2
**	mov	z2\.h, #3
**	mov	z3\.h, #4
**	ret
*/
svint16x4_t __attribute__((noipa))
callee_s16 (void)
{
  return svcreate4 (svdup_s16 (1), svdup_s16 (2),
		    svdup_s16 (3), svdup_s16 (4));
}

/*
** caller_s16:
**	...
**	bl	callee_s16
**	add	(z[2-7]\.h), z2\.h, z3\.h
**	ptrue	(p[0-7])\.b, all
**	mad	z0\.h, \2/m, (z1\.h, \1|\1, z1\.h)
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint16_t __attribute__((noipa))
caller_s16 (void)
{
  svint16x4_t res;
  res = callee_s16 ();
  return svmad_x (svptrue_b16 (), svget4 (res, 0), svget4 (res, 1),
		  svadd_x (svptrue_b16 (),
			   svget4 (res, 2),
			   svget4 (res, 3)));
}

/*
** callee_u16:
**	mov	z0\.h, #4
**	mov	z1\.h, #5
**	mov	z2\.h, #6
**	mov	z3\.h, #7
**	ret
*/
svuint16x4_t __attribute__((noipa))
callee_u16 (void)
{
  return svcreate4 (svdup_u16 (4), svdup_u16 (5),
		    svdup_u16 (6), svdup_u16 (7));
}

/*
** caller_u16:
**	...
**	bl	callee_u16
**	sub	(z[2-7]\.h), z2\.h, z3\.h
**	ptrue	(p[0-7])\.b, all
**	mad	z0\.h, \2/m, (z1\.h, \1|\1, z1\.h)
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint16_t __attribute__((noipa))
caller_u16 (void)
{
  svuint16x4_t res;
  res = callee_u16 ();
  return svmad_x (svptrue_b16 (), svget4 (res, 0), svget4 (res, 1),
		  svsub_x (svptrue_b16 (),
			   svget4 (res, 2),
			   svget4 (res, 3)));
}

/*
** callee_f16:
**	fmov	z0\.h, #1\.0(?:e\+0)?
**	fmov	z1\.h, #2\.0(?:e\+0)?
**	fmov	z2\.h, #3\.0(?:e\+0)?
**	fmov	z3\.h, #4\.0(?:e\+0)?
**	ret
*/
svfloat16x4_t __attribute__((noipa))
callee_f16 (void)
{
  return svcreate4 (svdup_f16 (1), svdup_f16 (2),
		    svdup_f16 (3), svdup_f16 (4));
}

/*
** caller_f16:
**	...
**	bl	callee_f16
**	fadd	(z[0-9]+\.h), z0\.h, z1\.h
**	fmul	(z[0-9]+\.h), \1, z2\.h
**	fadd	z0\.h, \2, z3\.h
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svfloat16_t __attribute__((noipa))
caller_f16 (void)
{
  svfloat16x4_t res;
  res = callee_f16 ();
  return svadd_x (svptrue_b16 (),
		  svmul_x (svptrue_b16 (),
			   svadd_x (svptrue_b16 (), svget4 (res, 0),
				    svget4 (res, 1)),
			   svget4 (res, 2)),
		  svget4 (res, 3));
}

/*
** callee_bf16:
**	mov	z0\.h, h4
**	mov	z1\.h, h5
**	mov	z2\.h, h6
**	mov	z3\.h, h7
**	ret
*/
svbfloat16x4_t __attribute__((noipa))
callee_bf16 (bfloat16_t h0, bfloat16_t h1, bfloat16_t h2, bfloat16_t h3,
	     bfloat16_t h4, bfloat16_t h5, bfloat16_t h6, bfloat16_t h7)
{
  return svcreate4 (svdup_bf16 (h4), svdup_bf16 (h5),
		    svdup_bf16 (h6), svdup_bf16 (h7));
}

/*
** caller_bf16:
**	...
**	bl	callee_bf16
**	trn2	z0\.h, z0\.h, z3\.h
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svbfloat16_t __attribute__((noipa))
caller_bf16 (bfloat16_t h0, bfloat16_t h1, bfloat16_t h2, bfloat16_t h3,
	     bfloat16_t h4, bfloat16_t h5, bfloat16_t h6, bfloat16_t h7)
{
  svbfloat16x4_t res;
  res = callee_bf16 (h0, h1, h2, h3, h4, h5, h6, h7);
  return svtrn2 (svget4 (res, 0), svget4 (res, 3));
}

/*
** callee_s32:
**	mov	z0\.s, #1
**	mov	z1\.s, #2
**	mov	z2\.s, #3
**	mov	z3\.s, #4
**	ret
*/
svint32x4_t __attribute__((noipa))
callee_s32 (void)
{
  return svcreate4 (svdup_s32 (1), svdup_s32 (2),
		    svdup_s32 (3), svdup_s32 (4));
}

/*
** caller_s32:
**	...
**	bl	callee_s32
**	add	(z[2-7]\.s), z2\.s, z3\.s
**	ptrue	(p[0-7])\.b, all
**	msb	z0\.s, \2/m, (z1\.s, \1|\1, z1\.s)
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint32_t __attribute__((noipa))
caller_s32 (void)
{
  svint32x4_t res;
  res = callee_s32 ();
  return svmsb_x (svptrue_b32 (), svget4 (res, 0), svget4 (res, 1),
		  svadd_x (svptrue_b32 (),
			   svget4 (res, 2),
			   svget4 (res, 3)));
}

/*
** callee_u32:
**	mov	z0\.s, #4
**	mov	z1\.s, #5
**	mov	z2\.s, #6
**	mov	z3\.s, #7
**	ret
*/
svuint32x4_t __attribute__((noipa))
callee_u32 (void)
{
  return svcreate4 (svdup_u32 (4), svdup_u32 (5),
		    svdup_u32 (6), svdup_u32 (7));
}

/*
** caller_u32:
**	...
**	bl	callee_u32
**	sub	(z[2-7]\.s), z2\.s, z3\.s
**	ptrue	(p[0-7])\.b, all
**	msb	z0\.s, \2/m, (z1\.s, \1|\1, z1\.s)
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint32_t __attribute__((noipa))
caller_u32 (void)
{
  svuint32x4_t res;
  res = callee_u32 ();
  return svmsb_x (svptrue_b32 (), svget4 (res, 0), svget4 (res, 1),
		  svsub_x (svptrue_b32 (),
			   svget4 (res, 2),
			   svget4 (res, 3)));
}

/*
** callee_f32:
**	fmov	z0\.s, #1\.0(?:e\+0)?
**	fmov	z1\.s, #2\.0(?:e\+0)?
**	fmov	z2\.s, #3\.0(?:e\+0)?
**	fmov	z3\.s, #4\.0(?:e\+0)?
**	ret
*/
svfloat32x4_t __attribute__((noipa))
callee_f32 (void)
{
  return svcreate4 (svdup_f32 (1), svdup_f32 (2),
		    svdup_f32 (3), svdup_f32 (4));
}

/*
** caller_f32:
**	...
**	bl	callee_f32
**	fadd	(z[0-9]+\.s), z0\.s, z1\.s
**	fmul	(z[0-9]+\.s), \1, z2\.s
**	fadd	z0\.s, \2, z3\.s
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svfloat32_t __attribute__((noipa))
caller_f32 (void)
{
  svfloat32x4_t res;
  res = callee_f32 ();
  return svadd_x (svptrue_b32 (),
		  svmul_x (svptrue_b32 (),
			   svadd_x (svptrue_b32 (), svget4 (res, 0),
				    svget4 (res, 1)),
			   svget4 (res, 2)),
		  svget4 (res, 3));
}

/*
** callee_s64:
**	mov	z0\.d, #1
**	mov	z1\.d, #2
**	mov	z2\.d, #3
**	mov	z3\.d, #4
**	ret
*/
svint64x4_t __attribute__((noipa))
callee_s64 (void)
{
  return svcreate4 (svdup_s64 (1), svdup_s64 (2),
		    svdup_s64 (3), svdup_s64 (4));
}

/*
** caller_s64:
**	...
**	bl	callee_s64
**	add	(z[2-7]\.d), z2\.d, z3\.d
**	ptrue	(p[0-7])\.b, all
**	mls	z0\.d, \2/m, (z1\.d, \1|\1, z1\.d)
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svint64_t __attribute__((noipa))
caller_s64 (void)
{
  svint64x4_t res;
  res = callee_s64 ();
  return svmls_x (svptrue_b64 (), svget4 (res, 0), svget4 (res, 1),
		  svadd_x (svptrue_b64 (),
			   svget4 (res, 2),
			   svget4 (res, 3)));
}

/*
** callee_u64:
**	mov	z0\.d, #4
**	mov	z1\.d, #5
**	mov	z2\.d, #6
**	mov	z3\.d, #7
**	ret
*/
svuint64x4_t __attribute__((noipa))
callee_u64 (void)
{
  return svcreate4 (svdup_u64 (4), svdup_u64 (5),
		    svdup_u64 (6), svdup_u64 (7));
}

/*
** caller_u64:
**	...
**	bl	callee_u64
**	sub	(z[2-7]\.d), z2\.d, z3\.d
**	ptrue	(p[0-7])\.b, all
**	mls	z0\.d, \2/m, (z1\.d, \1|\1, z1\.d)
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svuint64_t __attribute__((noipa))
caller_u64 (void)
{
  svuint64x4_t res;
  res = callee_u64 ();
  return svmls_x (svptrue_b64 (), svget4 (res, 0), svget4 (res, 1),
		  svsub_x (svptrue_b64 (),
			   svget4 (res, 2),
			   svget4 (res, 3)));
}

/*
** callee_f64:
**	fmov	z0\.d, #1\.0(?:e\+0)?
**	fmov	z1\.d, #2\.0(?:e\+0)?
**	fmov	z2\.d, #3\.0(?:e\+0)?
**	fmov	z3\.d, #4\.0(?:e\+0)?
**	ret
*/
svfloat64x4_t __attribute__((noipa))
callee_f64 (void)
{
  return svcreate4 (svdup_f64 (1), svdup_f64 (2),
		    svdup_f64 (3), svdup_f64 (4));
}

/*
** caller_f64:
**	...
**	bl	callee_f64
**	fadd	(z[0-9]+\.d), z0\.d, z1\.d
**	fmul	(z[0-9]+\.d), \1, z2\.d
**	fadd	z0\.d, \2, z3\.d
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svfloat64_t __attribute__((noipa))
caller_f64 (void)
{
  svfloat64x4_t res;
  res = callee_f64 ();
  return svadd_x (svptrue_b64 (),
		  svmul_x (svptrue_b64 (),
			   svadd_x (svptrue_b64 (), svget4 (res, 0),
				    svget4 (res, 1)),
			   svget4 (res, 2)),
		  svget4 (res, 3));
}
