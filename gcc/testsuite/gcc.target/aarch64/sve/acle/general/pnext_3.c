/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** test1:
**	pnext	p0\.h, p1, p0\.h
**	ret
*/
svbool_t
test1 (svbool_t pg, svbool_t prev)
{
  return svand_z (svptrue_b8 (),
		  svpnext_b16 (prev, pg),
		  svptrue_b16 ());
}

/*
** test2:
**	pnext	p0\.h, p1, p0\.h
**	ret
*/
svbool_t
test2 (svbool_t pg, svbool_t prev)
{
  return svand_z (svptrue_b16 (),
		  svpnext_b16 (prev, pg),
		  svptrue_b8 ());
}

/*
** test3:
**	pnext	p0\.h, p1, p0\.h
**	ret
*/
svbool_t
test3 (svbool_t pg, svbool_t prev)
{
  return svand_z (svptrue_b16 (),
		  svpnext_b16 (prev, pg),
		  svptrue_b16 ());
}

/*
** test4:
**	pnext	p0\.s, p1, p0\.s
**	ret
*/
svbool_t
test4 (svbool_t pg, svbool_t prev)
{
  return svand_z (svptrue_b32 (),
		  svpnext_b32 (prev, pg),
		  svptrue_b8 ());
}

/*
** test5:
**	pnext	p0\.s, p1, p0\.s
**	ret
*/
svbool_t
test5 (svbool_t pg, svbool_t prev)
{
  return svand_z (svptrue_b16 (),
		  svpnext_b32 (prev, pg),
		  svptrue_b8 ());
}

/*
** test6:
**	pnext	p0\.s, p1, p0\.s
**	ret
*/
svbool_t
test6 (svbool_t pg, svbool_t prev)
{
  return svand_z (svptrue_b8 (),
		  svpnext_b32 (prev, pg),
		  svptrue_b32 ());
}

/*
** test7:
**	pnext	p0\.d, p1, p0\.d
**	ret
*/
svbool_t
test7 (svbool_t pg, svbool_t prev)
{
  return svand_z (svptrue_b16 (),
		  svpnext_b64 (prev, pg),
		  svptrue_b8 ());
}

/*
** test8:
**	pnext	p0\.d, p1, p0\.d
**	ret
*/
svbool_t
test8 (svbool_t pg, svbool_t prev)
{
  return svand_z (svptrue_b32 (),
		  svpnext_b64 (prev, pg),
		  svptrue_b8 ());
}

/*
** test9:
**	pnext	p0\.d, p1, p0\.d
**	ret
*/
svbool_t
test9 (svbool_t pg, svbool_t prev)
{
  return svand_z (svptrue_b8 (),
		  svpnext_b64 (prev, pg),
		  svptrue_b64 ());
}

#ifdef __cplusplus
}
#endif
