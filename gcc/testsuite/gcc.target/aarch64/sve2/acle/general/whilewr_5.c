/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** test1:
**	whilewr	p0\.h, x0, x1
**	ret
*/
svbool_t
test1 (int16_t *x, int16_t *y)
{
  return svand_z (svptrue_b8 (),
		  svwhilewr (x, y),
		  svptrue_b16 ());
}

/*
** test2:
**	whilewr	p0\.h, x0, x1
**	ret
*/
svbool_t
test2 (uint16_t *x, uint16_t *y)
{
  return svand_z (svptrue_b16 (),
		  svwhilewr (x, y),
		  svptrue_b16 ());
}

/*
** test3:
**	whilewr	p0\.s, x0, x1
**	ret
*/
svbool_t
test3 (int32_t *x, int32_t *y)
{
  return svand_z (svptrue_b8 (),
		  svwhilewr (x, y),
		  svptrue_b16 ());
}

/*
** test4:
**	whilewr	p0\.s, x0, x1
**	ret
*/
svbool_t
test4 (uint32_t *x, uint32_t *y)
{
  return svand_z (svptrue_b8 (),
		  svwhilewr (x, y),
		  svptrue_b32 ());
}

/*
** test5:
**	whilewr	p0\.s, x0, x1
**	ret
*/
svbool_t
test5 (float32_t *x, float32_t *y)
{
  return svand_z (svptrue_b16 (),
		  svwhilewr (x, y),
		  svptrue_b32 ());
}

/*
** test6:
**	whilewr	p0\.s, x0, x1
**	ret
*/
svbool_t
test6 (int32_t *x, int32_t *y)
{
  return svand_z (svptrue_b32 (),
		  svwhilewr (x, y),
		  svptrue_b32 ());
}

/*
** test7:
**	whilewr	p0\.d, x0, x1
**	ret
*/
svbool_t
test7 (int64_t *x, int64_t *y)
{
  return svand_z (svptrue_b8 (),
		  svwhilewr (x, y),
		  svptrue_b64 ());
}

/*
** test8:
**	whilewr	p0\.d, x0, x1
**	ret
*/
svbool_t
test8 (uint64_t *x, uint64_t *y)
{
  return svand_z (svptrue_b16 (),
		  svwhilewr (x, y),
		  svptrue_b32 ());
}

/*
** test9:
**	whilewr	p0\.d, x0, x1
**	ret
*/
svbool_t
test9 (float64_t *x, float64_t *y)
{
  return svand_z (svptrue_b64 (),
		  svwhilewr (x, y),
		  svptrue_b64 ());
}

#ifdef __cplusplus
}
#endif
