/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** test1:
**	whilele	p0\.h, w0, w1
**	ret
*/
svbool_t
test1 (int32_t x, int32_t y)
{
  return svand_z (svptrue_b8 (),
		  svwhilele_b16 (x, y),
		  svptrue_b16 ());
}

/*
** test2:
**	whilele	p0\.h, x0, x1
**	ret
*/
svbool_t
test2 (int64_t x, int64_t y)
{
  return svand_z (svptrue_b16 (),
		  svwhilele_b16 (x, y),
		  svptrue_b16 ());
}

/*
** test3:
**	whilels	p0\.s, w0, w1
**	ret
*/
svbool_t
test3 (uint32_t x, uint32_t y)
{
  return svand_z (svptrue_b8 (),
		  svwhilele_b32 (x, y),
		  svptrue_b16 ());
}

/*
** test4:
**	whilels	p0\.s, x0, x1
**	ret
*/
svbool_t
test4 (uint64_t x, uint64_t y)
{
  return svand_z (svptrue_b8 (),
		  svwhilele_b32 (x, y),
		  svptrue_b32 ());
}

/*
** test5:
**	whilele	p0\.s, w0, w1
**	ret
*/
svbool_t
test5 (int32_t x, int32_t y)
{
  return svand_z (svptrue_b16 (),
		  svwhilele_b32 (x, y),
		  svptrue_b32 ());
}

/*
** test6:
**	whilels	p0\.s, w0, w1
**	ret
*/
svbool_t
test6 (uint32_t x, uint32_t y)
{
  return svand_z (svptrue_b32 (),
		  svwhilele_b32 (x, y),
		  svptrue_b32 ());
}

/*
** test7:
**	whilels	p0\.d, w0, w1
**	ret
*/
svbool_t
test7 (uint32_t x, uint32_t y)
{
  return svand_z (svptrue_b8 (),
		  svwhilele_b64 (x, y),
		  svptrue_b64 ());
}

/*
** test8:
**	whilele	p0\.d, x0, x1
**	ret
*/
svbool_t
test8 (int64_t x, int64_t y)
{
  return svand_z (svptrue_b16 (),
		  svwhilele_b64 (x, y),
		  svptrue_b32 ());
}

/*
** test9:
**	whilels	p0\.d, x0, x1
**	ret
*/
svbool_t
test9 (uint64_t x, uint64_t y)
{
  return svand_z (svptrue_b64 (),
		  svwhilele_b64 (x, y),
		  svptrue_b64 ());
}

#ifdef __cplusplus
}
#endif
