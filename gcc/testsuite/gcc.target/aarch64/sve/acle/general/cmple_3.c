/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** test1:
**	cmple	p0\.b, p0/z, z0\.b, z1\.b
**	ret
*/
svbool_t
test1 (svbool_t p0, svint8_t x, svint8_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (pg, svcmple (pg, x, y), p0);
}

/*
** test2:
**	cmple	p0\.b, p0/z, z0\.b, z1\.b
**	ret
*/
svbool_t
test2 (svbool_t p0, svint8_t x, svint8_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (p0, svcmple (pg, x, y), p0);
}

/*
** test3:
**	cmple	p0\.b, p0/z, z0\.b, z1\.b
**	ret
*/
svbool_t
test3 (svbool_t p0, svint8_t x, svint8_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (p0, svcmple (pg, x, y), pg);
}

/*
** test4:
**	cmple	p0\.b, p0/z, z0\.b, #10
**	ret
*/
svbool_t
test4 (svbool_t p0, svint8_t x)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (pg, svcmple (pg, x, 10), p0);
}

/*
** test5:
**	cmple	p0\.b, p0/z, z0\.b, #10
**	ret
*/
svbool_t
test5 (svbool_t p0, svint8_t x)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (p0, svcmple (pg, x, 10), p0);
}

/*
** test6:
**	cmple	p0\.b, p0/z, z0\.b, #10
**	ret
*/
svbool_t
test6 (svbool_t p0, svint8_t x)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (p0, svcmple (pg, x, 10), pg);
}

/*
** test7:
**	cmple	p0\.h, p0/z, z0\.h, z1\.h
**	ret
*/
svbool_t
test7 (svbool_t p0, svint16_t x, svint16_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (pg, svcmple (pg, x, y), p0);
}

/*
** test8:
**	cmple	p0\.h, p0/z, z0\.h, #10
**	ret
*/
svbool_t
test8 (svbool_t p0, svint16_t x)
{
  svbool_t pg = svptrue_b16 ();
  return svand_z (pg, svcmple (pg, x, 10), p0);
}

/*
** test9:
**	cmple	p0\.s, p0/z, z0\.s, z1\.s
**	ret
*/
svbool_t
test9 (svbool_t p0, svint32_t x, svint32_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (pg, svcmple (pg, x, y), p0);
}

/*
** test10:
**	cmple	p0\.s, p0/z, z0\.s, #10
**	ret
*/
svbool_t
test10 (svbool_t p0, svint32_t x)
{
  svbool_t pg = svptrue_b16 ();
  return svand_z (pg, svcmple (pg, x, 10), p0);
}

/*
** test11:
**	cmple	p0\.d, p0/z, z0\.d, z1\.d
**	ret
*/
svbool_t
test11 (svbool_t p0, svint64_t x, svint64_t y)
{
  svbool_t pg = svptrue_b8 ();
  return svand_z (pg, svcmple (pg, x, y), p0);
}

/*
** test12:
**	cmple	p0\.d, p0/z, z0\.d, #10
**	ret
*/
svbool_t
test12 (svbool_t p0, svint64_t x)
{
  svbool_t pg = svptrue_b16 ();
  return svand_z (pg, svcmple (pg, x, 10), p0);
}

#ifdef __cplusplus
}
#endif
