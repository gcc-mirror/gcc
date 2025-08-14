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
**	uzp1	p0\.h, p[0-3]\.h, \1\.h
**	ret
*/
svbool_t
test1 ()
{
  return svuzp1_b16 (svptrue_b8 (), svptrue_b16 ());
}

/*
** test2:
**	...
**	ptrue	(p[0-3])\.h, all
**	...
**	uzp1	p0\.h, \1\.h, p[0-3]\.h
**	ret
*/
svbool_t
test2 ()
{
  return svuzp1_b16 (svptrue_b16 (), svptrue_b8 ());
}

/*
** test3:
**	...
**	ptrue	(p[0-3])\.s, all
**	...
**	uzp1	p0\.s, p[0-3]\.s, \1\.s
**	ret
*/
svbool_t
test3 ()
{
  return svuzp1_b32 (svptrue_b8 (), svptrue_b32 ());
}

/*
** test4:
**	...
**	ptrue	(p[0-3])\.s, all
**	...
**	uzp1	p0\.s, \1\.s, p[0-3]\.s
**	ret
*/
svbool_t
test4 ()
{
  return svuzp1_b32 (svptrue_b32 (), svptrue_b8 ());
}

/*
** test5:
**	...
**	ptrue	(p[0-3])\.d, all
**	...
**	uzp1	p0\.d, p[0-3]\.d, \1\.d
**	ret
*/
svbool_t
test5 ()
{
  return svuzp1_b64 (svptrue_b8 (), svptrue_b64 ());
}

/*
** test6:
**	...
**	ptrue	(p[0-3])\.d, all
**	...
**	uzp1	p0\.d, \1\.d, p[0-3]\.d
**	ret
*/
svbool_t
test6 ()
{
  return svuzp1_b64 (svptrue_b64 (), svptrue_b8 ());
}

#ifdef __cplusplus
}
#endif
