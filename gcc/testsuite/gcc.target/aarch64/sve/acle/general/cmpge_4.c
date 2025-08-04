/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** test1:
**	cmphs	p0\.b, p0/z, z0\.b, z1\.b
**	ret
*/
svbool_t
test1 (svbool_t p0, svuint8_t x, svuint8_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (pg, svcmpge (pg, x, y), p0);
}

/*
** test2:
**	cmphs	p0\.b, p0/z, z0\.b, z1\.b
**	ret
*/
svbool_t
test2 (svbool_t p0, svuint8_t x, svuint8_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (p0, svcmpge (pg, x, y), p0);
}

/*
** test3:
**	cmphs	p0\.b, p0/z, z0\.b, z1\.b
**	ret
*/
svbool_t
test3 (svbool_t p0, svuint8_t x, svuint8_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (p0, svcmpge (pg, x, y), pg);
}

/*
** test4:
** (
**	cmphs	p0\.b, p0/z, z0\.b, #10
** |
**	cmphi	p0\.b, p0/z, z0\.b, #9
** )
**	ret
*/
svbool_t
test4 (svbool_t p0, svuint8_t x)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (pg, svcmpge (pg, x, 10), p0);
}

/*
** test5:
** (
**	cmphs	p0\.b, p0/z, z0\.b, #10
** |
**	cmphi	p0\.b, p0/z, z0\.b, #9
** )
**	ret
*/
svbool_t
test5 (svbool_t p0, svuint8_t x)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (p0, svcmpge (pg, x, 10), p0);
}

/*
** test6:
** (
**	cmphs	p0\.b, p0/z, z0\.b, #10
** |
**	cmphi	p0\.b, p0/z, z0\.b, #9
** )
**	ret
*/
svbool_t
test6 (svbool_t p0, svuint8_t x)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (p0, svcmpge (pg, x, 10), pg);
}

/*
** test7:
**	cmphs	p0\.h, p0/z, z0\.h, z1\.h
**	ret
*/
svbool_t
test7 (svbool_t p0, svuint16_t x, svuint16_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (pg, svcmpge (pg, x, y), p0);
}

/*
** test8:
**	cmphs	p0\.h, p0/z, z0\.h, #10
**	ret
*/
svbool_t
test8 (svbool_t p0, svuint16_t x)
{
  svbool_t pg = svptrue_b16 ();
  return svand_z (pg, svcmpge (pg, x, 10), p0);
}

/*
** test9:
**	cmphs	p0\.s, p0/z, z0\.s, z1\.s
**	ret
*/
svbool_t
test9 (svbool_t p0, svuint32_t x, svuint32_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (pg, svcmpge (pg, x, y), p0);
}

/*
** test10:
**	cmphs	p0\.s, p0/z, z0\.s, #10
**	ret
*/
svbool_t
test10 (svbool_t p0, svuint32_t x)
{
  svbool_t pg = svptrue_b16 ();
  return svand_z (pg, svcmpge (pg, x, 10), p0);
}

/*
** test11:
**	cmphs	p0\.d, p0/z, z0\.d, z1\.d
**	ret
*/
svbool_t
test11 (svbool_t p0, svuint64_t x, svuint64_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (pg, svcmpge (pg, x, y), p0);
}

/*
** test12:
**	cmphs	p0\.d, p0/z, z0\.d, #10
**	ret
*/
svbool_t
test12 (svbool_t p0, svuint64_t x)
{
  svbool_t pg = svptrue_b16 ();
  return svand_z (pg, svcmpge (pg, x, 10), p0);
}

#ifdef __cplusplus
}
#endif
