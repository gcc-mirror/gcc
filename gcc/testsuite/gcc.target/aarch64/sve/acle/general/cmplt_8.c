/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** test1:
**	cmplo	p0\.h, p0/z, z0\.h, z1\.d
**	ret
*/
svbool_t
test1 (svbool_t pg, svuint16_t x, svuint64_t y)
{
  return svand_z (svptrue_b8 (),
		  svcmplt_wide (pg, x, y),
		  svptrue_b16 ());
}

/*
** test2:
**	cmplo	p0\.s, p0/z, z0\.s, z1\.d
**	ret
*/
svbool_t
test2 (svbool_t pg, svuint32_t x, svuint64_t y)
{
  return svand_z (svptrue_b8 (),
		  svcmplt_wide (pg, x, y),
		  svptrue_b16 ());
}

/*
** test3:
**	cmplo	p0\.s, p0/z, z0\.s, z1\.d
**	ret
*/
svbool_t
test3 (svbool_t pg, svuint32_t x, svuint64_t y)
{
  return svand_z (svptrue_b8 (),
		  svcmplt_wide (pg, x, y),
		  svptrue_b32 ());
}

#ifdef __cplusplus
}
#endif
