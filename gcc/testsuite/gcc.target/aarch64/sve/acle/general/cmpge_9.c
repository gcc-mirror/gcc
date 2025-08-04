/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** test1:
**	fcmge	p0\.h, p0/z, z0\.h, z1\.h
**	ret
*/
svbool_t
test1 (svbool_t pg, svfloat16_t x, svfloat16_t y)
{
  return svand_z (svptrue_b8 (),
		  svcmpge (pg, x, y),
		  svptrue_b16 ());
}

/*
** test2:
**	fcmge	p0\.s, p0/z, z0\.s, z1\.s
**	ret
*/
svbool_t
test2 (svbool_t pg, svfloat32_t x, svfloat32_t y)
{
  return svand_z (svptrue_b8 (),
		  svcmpge (pg, x, y),
		  svptrue_b16 ());
}

/*
** test3:
**	fcmge	p0\.s, p0/z, z0\.s, z1\.s
**	ret
*/
svbool_t
test3 (svbool_t pg, svfloat32_t x, svfloat32_t y)
{
  return svand_z (svptrue_b8 (),
		  svcmpge (pg, x, y),
		  svptrue_b32 ());
}

/*
** test4:
**	fcmge	p0\.d, p0/z, z0\.d, z1\.d
**	ret
*/
svbool_t
test4 (svbool_t pg, svfloat64_t x, svfloat64_t y)
{
  return svand_z (svptrue_b8 (),
		  svcmpge (pg, x, y),
		  svptrue_b16 ());
}

/*
** test5:
**	fcmge	p0\.d, p0/z, z0\.d, z1\.d
**	ret
*/
svbool_t
test5 (svbool_t pg, svfloat64_t x, svfloat64_t y)
{
  return svand_z (svptrue_b32 (),
		  svcmpge (pg, x, y),
		  svptrue_b8 ());
}

/*
** test6:
**	fcmge	p0\.d, p0/z, z0\.d, z1\.d
**	ret
*/
svbool_t
test6 (svbool_t pg, svfloat64_t x, svfloat64_t y)
{
  return svand_z (svptrue_b8 (),
		  svcmpge (pg, x, y),
		  svptrue_b64 ());
}

/*
** test7:
**	fcmge	p0\.d, p0/z, z0\.d, z1\.d
**	ret
*/
svbool_t
test7 (svbool_t pg, svfloat64_t x, svfloat64_t y)
{
  return svand_z (svptrue_b32 (),
		  svcmpge (pg, x, y),
		  svptrue_b64 ());
}

#ifdef __cplusplus
}
#endif
