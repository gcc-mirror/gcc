/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** test1:
**	...
**	ptrue	(p[0-3])\.h, all
**	...
**	zip1	p0\.h, p[0-3]\.h, \1\.h
**	ret
*/
svbool_t
test1 ()
{
  return svzip1_b16 (svptrue_b8 (), svptrue_b16 ());
}

/*
** test2:
**	...
**	ptrue	(p[0-3])\.h, all
**	...
**	zip1	p0\.h, \1\.h, p[0-3]\.h
**	ret
*/
svbool_t
test2 ()
{
  return svzip1_b16 (svptrue_b16 (), svptrue_b8 ());
}

/*
** test3:
**	...
**	ptrue	(p[0-3])\.s, all
**	...
**	zip1	p0\.s, p[0-3]\.s, \1\.s
**	ret
*/
svbool_t
test3 ()
{
  return svzip1_b32 (svptrue_b8 (), svptrue_b32 ());
}

/*
** test4:
**	...
**	ptrue	(p[0-3])\.s, all
**	...
**	zip1	p0\.s, \1\.s, p[0-3]\.s
**	ret
*/
svbool_t
test4 ()
{
  return svzip1_b32 (svptrue_b32 (), svptrue_b8 ());
}

/*
** test5:
**	...
**	ptrue	(p[0-3])\.d, all
**	...
**	zip1	p0\.d, p[0-3]\.d, \1\.d
**	ret
*/
svbool_t
test5 ()
{
  return svzip1_b64 (svptrue_b8 (), svptrue_b64 ());
}

/*
** test6:
**	...
**	ptrue	(p[0-3])\.d, all
**	...
**	zip1	p0\.d, \1\.d, p[0-3]\.d
**	ret
*/
svbool_t
test6 ()
{
  return svzip1_b64 (svptrue_b64 (), svptrue_b8 ());
}

#ifdef __cplusplus
}
#endif
