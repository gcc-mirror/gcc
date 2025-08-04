/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** test1:
**	cmpne	p0\.h, p0/z, z0\.h, z1\.d
**	ret
*/
svbool_t
test1 (svbool_t pg, svint16_t x, svint64_t y)
{
  return svand_z (svptrue_b8 (),
		  svcmpne_wide (pg, x, y),
		  svptrue_b16 ());
}

/*
** test2:
**	cmpne	p0\.s, p0/z, z0\.s, z1\.d
**	ret
*/
svbool_t
test2 (svbool_t pg, svint32_t x, svint64_t y)
{
  return svand_z (svptrue_b8 (),
		  svcmpne_wide (pg, x, y),
		  svptrue_b16 ());
}

/*
** test3:
**	cmpne	p0\.s, p0/z, z0\.s, z1\.d
**	ret
*/
svbool_t
test3 (svbool_t pg, svint32_t x, svint64_t y)
{
  return svand_z (svptrue_b8 (),
		  svcmpne_wide (pg, x, y),
		  svptrue_b32 ());
}

#ifdef __cplusplus
}
#endif
