/* { dg-options "-O2 -mcpu=neoverse-v2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** foo:
**	movi	d0, #0
**	mov	z0.s, p0/m, #1
**	ret
*/
svint32_t foo (svbool_t pg)
{
  return svsel (pg, svdup_s32 (1), svdup_s32 (0));
}

/*
** foo2:
**	movi	d0, #0
**	fmov	z0.s, p0/m, #1.0
**	ret
*/
svfloat32_t foo2 (svbool_t pg)
{
  return svsel (pg, svdup_f32 (1.0f), svdup_f32 (0.0f));
}

