/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** add1:
**	add	z0\.s, (z1\.s, z0\.s|z0\.s, z1\.s)
**	ret
*/
svint32_t
add1 (svint32_t x, svint32_t y)
{
  return svadd_z (svptrue_b8 (), x, y);
}

/*
** add2:
**	add	z0\.s, (z1\.s, z0\.s|z0\.s, z1\.s)
**	ret
*/
svint32_t
add2 (svint32_t x, svint32_t y)
{
  return svadd_z (svptrue_b16 (), x, y);
}

/*
** add3:
**	add	z0\.s, (z1\.s, z0\.s|z0\.s, z1\.s)
**	ret
*/
svint32_t
add3 (svint32_t x, svint32_t y)
{
  return svadd_z (svptrue_b32 (), x, y);
}

/*
** add4:
**	...
**	movprfx	[^\n]+
**	...
**	ret
*/
svint32_t
add4 (svint32_t x, svint32_t y)
{
  return svadd_z (svptrue_b64 (), x, y);
}

/*
** add5:
**	add	z0\.s, (z1\.s, z0\.s|z0\.s, z1\.s)
**	ret
*/
svint32_t
add5 (svint32_t x, svint32_t y)
{
  return svadd_m (svptrue_b8 (), x, y);
}

/*
** add6:
**	add	z0\.s, (z1\.s, z0\.s|z0\.s, z1\.s)
**	ret
*/
svint32_t
add6 (svint32_t x, svint32_t y)
{
  return svadd_m (svptrue_b16 (), x, y);
}

/*
** add7:
**	add	z0\.s, (z1\.s, z0\.s|z0\.s, z1\.s)
**	ret
*/
svint32_t
add7 (svint32_t x, svint32_t y)
{
  return svadd_m (svptrue_b32 (), x, y);
}

/*
** add8:
**	ptrue	(p[0-7])\.d(?:, all)?
**	add	z0\.s, \1/m, z0\.s, z1\.s
**	ret
*/
svint32_t
add8 (svint32_t x, svint32_t y)
{
  return svadd_m (svptrue_b64 (), x, y);
}

/*
** add9:
**	ptrue	(p[0-7])\.s(?:, all)?
**	add	z0\.h, \1/m, z0\.h, z1\.h
**	ret
*/
svint16_t
add9 (svint16_t x, svint16_t y)
{
  return svadd_m (svptrue_b32 (), x, y);
}

/*
** and1:
**	and	z0\.s, z0\.s, #(?:0x)?1
**	ret
*/
svint32_t
and1 (svint32_t x)
{
  return svand_z (svptrue_b8 (), x, 1);
}

/*
** and2:
**	and	z0\.s, z0\.s, #(?:0x)?1
**	ret
*/
svint32_t
and2 (svint32_t x)
{
  return svand_z (svptrue_b16 (), x, 1);
}

/*
** and3:
**	and	z0\.s, z0\.s, #(?:0x)?1
**	ret
*/
svint32_t
and3 (svint32_t x)
{
  return svand_z (svptrue_b32 (), x, 1);
}

/*
** and4:
**	(?!and	z0\.s, z0\.s, #).*
**	ret
*/
svint32_t
and4 (svint32_t x)
{
  return svand_z (svptrue_b64 (), x, 1);
}

/*
** and5:
**	and	z0\.s, z0\.s, #(?:0x)?1
**	ret
*/
svint32_t
and5 (svint32_t x)
{
  return svand_m (svptrue_b8 (), x, 1);
}

/*
** and6:
**	and	z0\.s, z0\.s, #(?:0x)?1
**	ret
*/
svint32_t
and6 (svint32_t x)
{
  return svand_m (svptrue_b16 (), x, 1);
}

/*
** and7:
**	and	z0\.s, z0\.s, #(?:0x)?1
**	ret
*/
svint32_t
and7 (svint32_t x)
{
  return svand_m (svptrue_b32 (), x, 1);
}

/*
** and8:
**	(?!and	z0\.s, z0\.s, #).*
**	ret
*/
svint32_t
and8 (svint32_t x)
{
  return svand_m (svptrue_b64 (), x, 1);
}

/*
** and9:
** (
**	and	p0\.b, p0/z, p1\.b, p1\.b
** |
**	and	p0\.b, p1/z, p0\.b, p0\.b
** )
**	ret
*/
svbool_t
and9 (svbool_t x, svbool_t y)
{
  return svand_z (svptrue_b8 (), x, y);
}

/*
** not1:
**	ptrue	(p[0-7])\.b(?:, all)?
**	not	z0\.s, \1/m, z1\.s
**	ret
*/
svint32_t
not1 (svint32_t x, svint32_t y)
{
  return svnot_m (x, svptrue_b8 (), y);
}

/*
** cvt1:
**	ptrue	(p[0-7])\.b(?:, all)?
**	fcvtzs	z0\.s, \1/m, z0\.h
**	ret
*/
svint32_t
cvt1 (svfloat16_t x)
{
  return svcvt_s32_z (svptrue_b8 (), x);
}

/*
** cvt2:
**	ptrue	(p[0-7])\.b(?:, all)?
**	fcvtzs	z0\.s, \1/m, z0\.h
**	ret
*/
svint32_t
cvt2 (svfloat16_t x)
{
  return svcvt_s32_z (svptrue_b16 (), x);
}

/*
** cvt3:
**	ptrue	(p[0-7])\.b(?:, all)?
**	fcvtzs	z0\.s, \1/m, z0\.h
**	ret
*/
svint32_t
cvt3 (svfloat16_t x)
{
  return svcvt_s32_z (svptrue_b32 (), x);
}

/*
** cvt4:
**	...
**	movprfx	[^\n]+
**	...
**	ret
*/
svint32_t
cvt4 (svfloat16_t x)
{
  return svcvt_s32_z (svptrue_b64 (), x);
}

/*
** cvt5:
**	ptrue	(p[0-7])\.b(?:, all)?
**	fcvt	z0\.h, \1/m, z0\.s
**	ret
*/
svfloat16_t
cvt5 (svfloat32_t x)
{
  return svcvt_f16_z (svptrue_b8 (), x);
}

/*
** cvt6:
**	ptrue	(p[0-7])\.b(?:, all)?
**	fcvt	z0\.h, \1/m, z0\.s
**	ret
*/
svfloat16_t
cvt6 (svfloat32_t x)
{
  return svcvt_f16_z (svptrue_b16 (), x);
}

/*
** cvt7:
**	ptrue	(p[0-7])\.b(?:, all)?
**	fcvt	z0\.h, \1/m, z0\.s
**	ret
*/
svfloat16_t
cvt7 (svfloat32_t x)
{
  return svcvt_f16_z (svptrue_b32 (), x);
}

/*
** cvt8:
**	...
**	movprfx	[^\n]+
**	...
**	ret
*/
svfloat16_t
cvt8 (svfloat32_t x)
{
  return svcvt_f16_z (svptrue_b64 (), x);
}

/*
** cvt9:
**	ptrue	(p[0-7])\.b(?:, all)?
**	scvtf	z0\.h, \1/m, z0\.h
**	ret
*/
svfloat16_t
cvt9 (svint16_t x)
{
  return svcvt_f16_z (svptrue_b8 (), x);
}

/*
** cvt10:
**	ptrue	(p[0-7])\.b(?:, all)?
**	scvtf	z0\.h, \1/m, z0\.h
**	ret
*/
svfloat16_t
cvt10 (svint16_t x)
{
  return svcvt_f16_z (svptrue_b16 (), x);
}

/*
** cvt11:
**	...
**	movprfx	[^\n]+
**	...
**	ret
*/
svfloat16_t
cvt11 (svint16_t x)
{
  return svcvt_f16_z (svptrue_b32 (), x);
}

/*
** cvt12:
**	...
**	movprfx	[^\n]+
**	...
**	ret
*/
svfloat16_t
cvt12 (svint16_t x)
{
  return svcvt_f16_z (svptrue_b64 (), x);
}

#ifdef __cplusplus
}
#endif
