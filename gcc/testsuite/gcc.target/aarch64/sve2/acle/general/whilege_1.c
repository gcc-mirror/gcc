/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** test1:
**	whilege	p0\.h, w0, w1
**	ret
*/
svbool_t
test1 (int32_t x, int32_t y)
{
  return svand_z (svptrue_b8 (),
		  svwhilege_b16 (x, y),
		  svptrue_b16 ());
}

/*
** test2:
**	whilege	p0\.h, x0, x1
**	ret
*/
svbool_t
test2 (int64_t x, int64_t y)
{
  return svand_z (svptrue_b16 (),
		  svwhilege_b16 (x, y),
		  svptrue_b16 ());
}

/*
** test3:
**	whilehs	p0\.s, w0, w1
**	ret
*/
svbool_t
test3 (uint32_t x, uint32_t y)
{
  return svand_z (svptrue_b8 (),
		  svwhilege_b32 (x, y),
		  svptrue_b16 ());
}

/*
** test4:
**	whilehs	p0\.s, x0, x1
**	ret
*/
svbool_t
test4 (uint64_t x, uint64_t y)
{
  return svand_z (svptrue_b8 (),
		  svwhilege_b32 (x, y),
		  svptrue_b32 ());
}

/*
** test5:
**	whilege	p0\.s, w0, w1
**	ret
*/
svbool_t
test5 (int32_t x, int32_t y)
{
  return svand_z (svptrue_b16 (),
		  svwhilege_b32 (x, y),
		  svptrue_b32 ());
}

/*
** test6:
**	whilehs	p0\.s, w0, w1
**	ret
*/
svbool_t
test6 (uint32_t x, uint32_t y)
{
  return svand_z (svptrue_b32 (),
		  svwhilege_b32 (x, y),
		  svptrue_b32 ());
}

/*
** test7:
**	whilehs	p0\.d, w0, w1
**	ret
*/
svbool_t
test7 (uint32_t x, uint32_t y)
{
  return svand_z (svptrue_b8 (),
		  svwhilege_b64 (x, y),
		  svptrue_b64 ());
}

/*
** test8:
**	whilege	p0\.d, x0, x1
**	ret
*/
svbool_t
test8 (int64_t x, int64_t y)
{
  return svand_z (svptrue_b16 (),
		  svwhilege_b64 (x, y),
		  svptrue_b32 ());
}

/*
** test9:
**	whilehs	p0\.d, x0, x1
**	ret
*/
svbool_t
test9 (uint64_t x, uint64_t y)
{
  return svand_z (svptrue_b64 (),
		  svwhilege_b64 (x, y),
		  svptrue_b64 ());
}

#ifdef __cplusplus
}
#endif
